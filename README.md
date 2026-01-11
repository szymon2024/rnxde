2026-01-11

EN:
---

Rnxde
-----

The program creates a version of the RINEX 3.04 navigation file,
replacing the letter "D" or "d" with the letter "e" in numbers written
in scientific notation. It is used as a command on the command line.

Description of how the program works:

The program searches the file for fragments of numbers written in
scientific notation and replaces the letter 'D' or 'd' with 'e'. The
number types listed in the publicly available rinex304 specification
as Double are changed. The position of the numbers is also determined
based on the rinex304 specification. The result is a new RINEX
file. The input file is not modified.


Downloading the program
-----------------------

If you just want to run the program on Windows, download the pre-built
rnxde.exe file from Releases.

No need to download other files.


Example of use
--------------

On the command line:
```
rnxde xxxxxx.25g
```


Compiling the source code (optional)
------------------------------------

If you want to compile the program yourself on MS Windows, you need to
download and install the Haskell compiler called ghc. Then, in the
command line:
```
ghc -O2 RnxDe.hs -o rnxde.exe
```
The resulting *.h and *.o files can be deleted.


PL:
---

Rnxde
-----

Program tworzy wersję pliku nawigacyjnego RINEX 3.04, zastępując
literę „D” lub „d” literą „e” w liczbach zapisanych w notacji
naukowej. Używany jest jako polecenie w wierszu poleceń.

Opis działania programu:

Program wyszukuje w pliku fragmenty liczb zapisanych w notacji
naukowej i zamienia literę 'D' lub 'd' na 'e'. Zmieniane są liczby
wymienione w publicznie dostępnej specyfikacji rinex304 jako
Double. Pozycja liczb również została ustalona na podstawie
specyfikacji rinex304. Wynikiem działania jest nowy plik
RINEX.  Plik wejściowy nie jest modyfikowany.


Pobieranie programu
-------------------

Jeśli chcesz tylko uruchomić program w systemie Windows, pobierz
gotowy plik rnxde.exe z Releases.

Nie trzeba pobierać innych plików.


Przykład użycia
---------------

W wierszu poleceń:
```
rnxde xxxxxx.25g
```


Kompilacja kodu źródłowego (opcjonalnie)
----------------------------------------

Jeśli chcesz skompilować program samodzielnie w systemie MS Windows,
to trzeba pobrać i zainstalować kompilator języka haskell o nazwie
ghc. Następnie w wierszu poleceń:
```
ghc -O2 RnxDe.hs -o rnxde.exe
```
Powstałe plik *.hi *.o można usunąć.