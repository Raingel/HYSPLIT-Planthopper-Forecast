@ECHO OFF

set /P YEAR=TYPE YEAR (e.g. 1979) and press ENTER:
set /P MONTH=TYPE MONTH (e.g. 01) and press ENTER:

for %%d in (01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31) do call data_hour.bat %YEAR% %MONTH% %%d
