@ECHO OFF

REM 1=YEAR 2=MONTH 3=DAY 4=HOUR

set GRIB=narr-a_221_%1%2%3_%4_000.grb
ECHO Processing: %GRIB%
IF NOT EXIST %GRIB% goto error

narr2arl %GRIB%
type DATA.NARR   >>NARR%YEAR%%MONTH%
del  DATA.NARR
goto end

:error
ECHO GRIB file not found!
PAUSE

:end
