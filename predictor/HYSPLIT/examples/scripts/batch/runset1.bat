@ECHO OFF

rem any line that starts with rem is only a remark, i.e., it is not executed
rem this is an MS-DOS batch file, that is run from the MS-DOS command line, by typing "set" and then return
rem or that can be called from another batch file
rem in this case, it is called from run.bat

rem example set of replaceable parameters

rem param 1 = start year
rem param 2 = start month
rem param 3 = start day
rem param 4 = start hr
rem param 5 = number of sources

rem
rem **********************************************************************
rem
rem 1. UTC Starting Time for the Simulation
rem
rem    (year, month, day, hour);
rem    two-digit values for the UTC time that the calculation is to start
rem    Enter 0's to start at the beginning of the initial met file
rem
ECHO   %1    %2    %3    %4  >CONTROL.DAT
rem
rem **********************************************************************
rem
rem 2. Number of Starting Locations
rem
rem    Note: recompiliation required for different emissions
rem    from different sources
rem
ECHO            %5 >>CONTROL.DAT
