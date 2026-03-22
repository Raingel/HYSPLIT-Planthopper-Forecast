@ECHO OFF

rem any line that starts with rem is only a remark, i.e., it is not executed
rem this is an MS-DOS batch file, that is run from the MS-DOS command line, by typing "set" and then return
rem or that can be called from another batch file
rem in this case, it is called from run.bat

rem example set of replaceable parameters

rem param 1 = source latitude
rem param 2 = source long
rem param 3 = height description
rem param 4 = emis rate


rem **********************************************************************
rem
rem 3. Starting Location [lat,long, height (meters above ground)]
rem    west longitudes are negative; south latitudes are negative

if %3==low goto lowhght
if %3==mid goto midhght

:lowhght
ECHO   %1   %2  100.00  %4   >>CONTROL.DAT
goto endhght

if %8==mid goto midhght

:midhght 
ECHO   %1   %2  1000.00  %4  >>CONTROL.DAT
goto endhght

:endhght
