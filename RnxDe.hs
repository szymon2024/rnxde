-- 2026-01-11

{- | The program creates a version of the RINEX 3.04 navigation file,
     replacing the letter "D" or "d" with the letter "e" in numbers,
     so that "e" is used in scientific notation instead of the
     Fortran-style "D." It is intended to be used as a command on the
     command line.

     Scientific notation letters are searched for within a defined
     range from the end of the Double field. The range is defined as
     (width, end) where width is manualy calculated with formula:
     field length - precision of number. For example, in the rinex304
     specification on page 73, we read that for header label =
     IONOSPHERIC CORR, D12.4 is specified. The field length is 12 and
     the precision is 4. Width = 12 - 4 = 8.

     NOTE:
       It is important to detect END OF HEADER from column 60,
       because there can be END OF HEADER in comment fields.

-}


{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.ByteString.Lazy.Internal       (ByteString (Empty))
import qualified Data.ByteString.Builder    as B    
import           Data.Char                           (isSpace)
import           Data.Int                            (Int64)
import           Text.Printf
import           Control.Monad                       (when, unless)
import           Data.Time.Clock                     (getCurrentTime, diffUTCTime)
import           System.IO                           (hFlush, stdout, stderr)
import           System.Exit                         (exitFailure, exitSuccess)
import           System.Directory                    (doesFileExist)
import           System.Environment                  (getArgs)
import           System.FilePath                     (splitFileName, (</>))    
    
data Range = Range { width :: Int64     -- ^ from rinex 3.04 specification: field length - number precision
                   , end   :: Int64     -- ^ from rinex 3.04 specification: field start + field length
                   }

programVersion :: String
programVersion = "1.0.0"
           
main :: IO ()
main = do
  args <- getArgs

  case args of
    ["--help"]    -> showHelp
    ["-h"]        -> showHelp
    ["--version"] -> showVersion
    ["-v"]        -> showVersion
                     
    [input] -> do
        runWithOptions input Nothing

    [input, "-o", output] ->
        runWithOptions input (Just output)

    _ -> do
        hPrintf stderr "Usage:   rnxde INPUT [-o OUTPUT]\n\
                       \Try  :   rnxde --help\n\n"
        exitFailure

-- | Version printing function
showVersion :: IO ()
showVersion = do
  printf "rnxde version %s\n" programVersion
  exitSuccess

----------------------------------------------------------------------  

-- | Help printing function
showHelp :: IO ()
showHelp = do
  printf "rnxde - The program replaces the letters 'D' and 'd' with 'e'\n\
         \        in the scientific notation of numbers of the RINEX 3.04\n\
         \        navigation file (not an observation file).\n\
         \\n\
         \Usage: rnxde INPUT [-o OUTPUT]\n\
         \Options:\n\
         \  -o OUTPUT       writes the result to OUTPUT,\n\
         \  -h, --help      writes this help\n\
         \  -v, --version   writes a version of the program.\n\
         \\n\
         \Arguments:\n\
         \ INPUT    input file RINEX\n\
         \ OUTPUT   output file RINEX (optional)\n\
         \\n\
         \If OUTPUT is not specified, the program will create a file:\n\
         \ de_INPUT\n\
         \\n\
         \The program blocks overwriting the input file and warns,\n\
         \if the output file already exists.\n\
         \\n"
  exitSuccess

----------------------------------------------------------------------
          
validate :: FilePath -> Maybe FilePath -> IO FilePath
validate input mOutput  = do
    inExists <- doesFileExist input
    unless inExists $ do
        hPrintf stderr "Error: input file \"%s\" does not exist.\n" input
        exitFailure

    -- Set the output file name
    let output = case mOutput of
            Just o  -> o
            Nothing -> addPrefix "de_" input    

    when (input == output) $ do
        hPrintf stderr "Error: Input and output files cannot have the same name.\n"
        exitFailure

    outExists <- doesFileExist output
    when outExists $ do
        printf "Output file \"%s\" already exists.\n" output
        putStr "Overwrite ? [y/n]: "
        hFlush stdout
        ans <- getLine
        when (ans /= "y" && ans /= "Y") $ do
            hPrintf stderr "Aborted - file was not overwritten.\n"
            exitFailure

    return output

----------------------------------------------------------------------

-- | Add prefix to file name
addPrefix :: String -> FilePath -> FilePath
addPrefix prefix path =
    let (dir, file) = splitFileName path
    in dir </> (prefix ++ file)

----------------------------------------------------------------------
           
runWithOptions :: FilePath -> Maybe FilePath -> IO ()
runWithOptions input mOutput = do
  
  output <- validate input mOutput
              
  t0 <- getCurrentTime
         
  printf "Start reading %s\n" input
  bs <- L8.readFile input
  let bs' = deRnx bs
  printf "Start writing %s\n" output
  L8.writeFile output bs'
  printf "Processing complete\n"
         
  t <- getCurrentTime
  let diffT = diffUTCTime t t0
  when (diffT >= 0.001) $
       printf "Processing time: %.3f s.\n" (realToFrac diffT::Double)

----------------------------------------------------------------------

-- | Replace the letters 'D' and 'd' with 'e' in the scientific
--   notation of numbers of the RINEX 3.04 navigation file.
deRnx :: L8.ByteString -> L8.ByteString
deRnx bs =
    let (b, body) = deHdr bs
    in  B.toLazyByteString $ (b <> deBody body)

----------------------------------------------------------------------

-- | Replace the letters 'D' and 'd' with 'e' in the scientific
--   notation of numbers in the body of the RINEX 3.04 navigation
--   file.  According to the rinex specification, the line length is
--   80 characters. Processing is line by line.
deBody :: L8.ByteString -> B.Builder
deBody Empty = mempty
deBody bs =
    let (l80, bs1)  = L8.splitAt 80 bs
        (eol   , bs2) = L8.span (`L8.elem` "\n\r") bs1
    in case L8.head bs of
         ' ' -> deFields [ Range 7 23, Range 7 19 , Range 7 19 , Range 7 19 ] l80
                <> B.lazyByteString eol
                <> deBody bs2
         c | c `L8.elem` "GREJCIS" ->
               deFields [ Range 7 42, Range 7 19 , Range 7 19] l80
                <> B.lazyByteString eol
                <> deBody bs2
         c   -> errorWithoutStackTrace $
                  "Error\n\
                  \Unexpected character '" ++ [c] ++ "' at the beginning of the line."

----------------------------------------------------------------------

-- | Replace the letter 'd' or 'D' with 'e' in header line according
--   to list of ranges selected by label.
deHdrLine :: L8.ByteString -> B.Builder
deHdrLine l =
    let (dataFld, labelFld) = L8.splitAt 60 l
        label = trim labelFld
        ranges =
            case label of                         
              "IONOSPHERIC CORR" ->
                  [ Range 8 17, Range 8 12, Range 8 12, Range 8 12]            
              "TIME SYSTEM CORR" ->
                  [ Range 7 22, Range 5 16]
              _                  -> []
    in deFields ranges dataFld <> B.lazyByteString labelFld
            
----------------------------------------------------------------------

-- | Replace letter 'd' or 'D' with 'e' in ByteString according to list
--   of ranges.
deFields :: [Range] -> L8.ByteString -> B.Builder
deFields _  Empty = mempty
deFields [] bs0 = B.lazyByteString bs0                 
deFields rs bs0 =
    let (acc, rest) =  foldl step (mempty, bs0) rs
    in acc <> B.lazyByteString rest
        where
          step (acc, bs) r =
              let (bs1, bs2) = readToRange r bs
                  (bs3, bs4) = readRange   r bs2
                  b = B.lazyByteString bs1 <> de bs3
              in (acc <> b, bs4)
          readToRange (Range w e) = L8.splitAt (e - w)
          readRange   (Range w _) = L8.splitAt w

----------------------------------------------------------------------

-- | Replace first 'd' or 'D' from end of Bytestring
--   with 'e'.
de :: L8.ByteString -> B.Builder
de bs =
    case L8.findIndexEnd  (`L8.elem` "dD") bs of
      Nothing -> B.lazyByteString bs
      Just i  ->
          let
              (bs1, bs2) = L8.splitAt i bs
          in case L8.uncons bs2 of
               Nothing -> B.lazyByteString bs
               Just (_, bs') -> B.lazyByteString bs1
                                <> B.char8 'e'
                                <> B.lazyByteString bs'

----------------------------------------------------------------------                                   
                         
-- | Replace the letters 'D' and 'd' with 'e' in the scientific
--   notation of numbers in the header of the RINEX 3.04 navigation
--   file. Uses information about the label position and the fixed
--   length of the header line.  The header record takes one line and
--   consists of:
--    - data field   0-59,
--    - label field 60-79.
--   The function cannot use the RINEX 3.04 specification knowledge
--   that the width of a line should always be 80 characters, because
--   last line sometimes breaks this rule.
deHdr :: L8.ByteString ->  (B.Builder, L8.ByteString)
deHdr Empty = errorWithoutStackTrace "Error: Empty input."
deHdr bs0
    | rnxVer bs0 /= "3.04"    = errorWithoutStackTrace
                                "Not RINEX 3.04 file"
    | rnxFileType bs0 /= "N"  = errorWithoutStackTrace
                                "Not navigation file"
    | otherwise               = go bs0
    where
      go :: L8.ByteString -> (B.Builder, L8.ByteString)
      go Empty  = errorWithoutStackTrace
                  "END OF HEADER not found."
      go bs
          | lookEOH bs =
                let -- Read last line
                    (l73, bs1) = read73 bs
                    (xs , bs2) = readToEOL bs1
                    (eol, bs3) = readEOL bs2
                    b1 = B.lazyByteString l73
                    b2 = B.lazyByteString xs
                    b3 = B.lazyByteString eol
                in (b1 <> b2 <> b3, bs3)
          | otherwise  =
                let -- Read line before last line
                    (l80, bs1) = read80 bs
                    (eol, bs2) = readEOL bs1
                    b1         = deHdrLine l80
                    b2         = B.lazyByteString eol
                    (b3 , bs3) = go bs2
                in (b1 <> b2 <> b3, bs3)


      lookEOH = (== "END OF HEADER") . L8.take 13 . L8.dropWhile isSpace . L8.drop 60

      read80    = L8.splitAt 80
      
      readEOL   = L8.span (`L8.elem` "\r\n")      -- Note: It reads empty lines too.

      read73    = L8.splitAt 73

      readToEOL = L8.break (`L8.elem` "\n\r")
                    
      -- Header fields
      rnxVer      = trim . takeField  0 9
      rnxFileType = trim . takeField 20 1
                   
----------------------------------------------------------------------
  
-- | Trim leading and trailing whitespace from a ByteString.              
trim :: L8.ByteString -> L8.ByteString
trim = L8.dropWhile isSpace . L8.dropWhileEnd isSpace

----------------------------------------------------------------------

-- | Extract a substring from a line:
--   starting at position 'start' (0-based),
--   with length 'len'.
--   Used to read fixed-width fields.       
takeField :: Int64 -> Int64 -> L8.ByteString -> L8.ByteString
takeField start len = L8.take len . L8.drop start       
