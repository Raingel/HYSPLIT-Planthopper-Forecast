REM Last Revised: 14 SEP 2021

REM customize to installation
CD c:\hysplit\testing

SET PGM=..\exec

IF EXIST GEMCONC_037      DEL GEM*
IF EXIST TDUMP_001        DEL TDUMP*
IF EXIST CDUMP_010        DEL CDUMP*
IF EXIST PARDUMP_042      DEL PARDUMP_*
IF EXIST pdump_510        DEL pdump_*
IF EXIST rdump_510        DEL rdump_*
IF EXIST CCROP_133        DEL CCROP*
IF EXIST GELABEL_html.txt DEL *_html.txt
IF EXIST HYSPLIT_html.kml DEL *_html.kml
IF EXIST TRAJPLOT_C0.HTML DEL TRAJPLOT_*.HTML
IF EXIST CLUSPLOT.HTML    DEL CLUSPLOT.HTML
IF EXIST trajfreq.html    DEL trajfreq.html
IF EXIST plot_001.html    DEL plot_???.html
IF EXIST statA.txt        DEL statA.txt
IF EXIST stat_039.txt     DEL stat_???.txt
IF EXIST MAPTEXT.CFG      DEL MAPTEXT.CFG
IF EXIST ccoev            DEL ccoev
IF EXIST WARNING.043      DEL WARNING.*

PAUSE
date /t  >xtime.txt
time /t >>xtime.txt

IF EXIST RESULTS.HTML  DEL *.HTML

REM ------------------------
ECHO 'TITLE^&','Forward Trajectory (001)^&' >LABELS.CFG
%PGM%\hyts_std 001
%PGM%\trajplot +g1 -itdump_001 -oplot_001.html
DEL LABELS.CFG TRAJ.CFG WARNING.001 MESSAGE.001
TYPE plot_001.html >results.html

REM ------------------------
ECHO 'TITLE^&','Add Backward Trajectory (002)^&' >LABELS.CFG
%PGM%\hyts_std 002
%PGM%\trajplot +g1 -itdump_001+tdump_002 -oplot_002.html
DEL LABELS.CFG TRAJ.CFG WARNING.002 MESSAGE.002
TYPE plot_002.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Forward Isobaric Trajectory (003)^&' >LABELS.CFG
%PGM%\hyts_std 003
%PGM%\trajplot +g1 -itdump_003 -oplot_003.html
DEL LABELS.CFG TRAJ.CFG WARNING.003 MESSAGE.003
TYPE plot_003.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Add Backward Isobaric Trajectory (004)^&' >LABELS.CFG
%PGM%\hyts_std 004
%PGM%\trajplot +g1 -itdump_003+tdump_004 -oplot_004.html
DEL LABELS.CFG TRAJ.CFG WARNING.004 MESSAGE.004
TYPE plot_004.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Trajectory Ensemble (005)^&' >LABELS.CFG
%PGM%\hyts_ens 005
%PGM%\trajplot +g1 -itdump_005 -oplot_005.html
DEL LABELS.CFG TRAJ.CFG WARNING.005 MESSAGE.005
TYPE plot_005.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Trajectory Matrix (006)^&' >LABELS.CFG
COPY CONTROL.006 CONTROL
%PGM%\latlon
%PGM%\hyts_std
%PGM%\trajplot +g1 -s1 -l0 -z90 -itdump_006 -oplot_006.html
DEL CONTROL WARNING MESSAGE LABELS.CFG TRAJ.CFG
TYPE plot_006.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Multiple-in-Time Trajectory (007)^&' >LABELS.CFG
COPY SETUP.007 SETUP.CFG
%PGM%\hyts_std 007
%PGM%\trajplot +g1 -l3 -itdump_007 -oplot_007.html
DEL LABELS.CFG TRAJ.CFG SETUP.CFG WARNING.007 MESSAGE.007
TYPE plot_007.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Splitting-in-Time Trajectory (008)^&' >LABELS.CFG
COPY SETUP.008 SETUP.CFG
%PGM%\hyts_std 008
%PGM%\trajplot +g1 -s0 -l0 -itdump_008 -oplot_008.html
DEL LABELS.CFG TRAJ.CFG SETUP.CFG WARNING.008 MESSAGE.008
TYPE plot_008.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Terrain below Trajectory (009)^&' >LABELS.CFG
COPY SETUP.009 SETUP.CFG
%PGM%\hyts_std 009
ECHO '../graphics/county.shp' 0 0.002 0.4 0.6 0.8 >SHAPEFILES.009
ECHO '../graphics/states.shp' 0 0.015 0.2 0.2 0.5 >>SHAPEFILES.009
%PGM%\trajplot +g1 -v1 -itdump_009 -oplot_009.html -jshapefiles.009
DEL LABELS.CFG TRAJ.CFG SETUP.CFG WARNING.009 MESSAGE.009 SHAPEFILES.009
TYPE plot_009.html >>results.html

REM ------------------------
echo 'TITLE^&','Multiple Meteorology Trajectory (109)^&' >LABELS.CFG
%PGM%\hyts_std 109
%PGM%\trajplot +g1 -v1 -itdump_109 -oplot_109.html
DEL LABELS.CFG SETUP.CFG TRAJ.CFG WARNING.109 MESSAGE.109
TYPE plot_109.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Standard Dispersion Simulation (010)^&' >LABELS.CFG
%PGM%\hycs_std 010
%PGM%\concplot +g1 -icdump_010 -oplot_010.html -c4 -v1.0E-12::255255000+1.0E-13::000000255+1.0E-14::000255000+1.0E-15::000255255
DEL LABELS.CFG CONC.010 WARNING.010 MESSAGE.010 VMSDIST.010
TYPE plot_010.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Standard Dispersion With 4 User Set Contours (110)^&' >LABELS.CFG
%PGM%\concplot +g1 -icdump_010 -oplot_110.html -c4 -v1E-12+1E-13+1E-14+1E-15
DEL LABELS.CFG
TYPE plot_110.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Standard Dispersion With 8 User Set Contours + GE (210)^&' >LABELS.CFG
%PGM%\concplot +g1 -icdump_010 -oplot_210.html -a3 -c4 -v1E-12+1E-13+1E-14+1E-15+1E-16+1E-17+1E-18+1E-19
DEL LABELS.CFG gistmp_html.txt GELABEL_html.txt
TYPE plot_210.html >>results.html

REM ------------------------
ECHO 'TITLE^&','MPI Dispersion (UNIX only) using 4 processors (310)^&' >LABELS.CFG
%PGM%\concplot +g1 -icdump_010 -oplot_310.html -c4 -v1E-12+1E-13+1E-14+1E-15+1E-16+1E-17+1E-18+1E-19
DEL LABELS.CFG 
TYPE plot_310.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Polar Concentration Grid Particles (410)^&' >LABELS.CFG
%PGM%\hycs_std 410
%PGM%\poleplot +g1 -ccdump_410 -l0.5
RENAME poleplot.html plot_410.html
DEL LABELS.CFG CONC.410 WARNING.410 MESSAGE.410 VMSDIST.410
TYPE plot_410.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Polar Concentration Grid Puffs (420)^&' >LABELS.CFG
%PGM%\hycs_std 420
%PGM%\poleplot +g1 -ccdump_420 -l0.5
RENAME poleplot.html plot_420.html
DEL LABELS.CFG CONC.420 WARNING.420 MESSAGE.420 VMSDIST.420
TYPE plot_420.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Polar+Rectangular Grid (510)^&' >LABELS.CFG
%PGM%\hycs_std 510
%PGM%\concplot +g1 -irdump_510 -oplot_51R.html -c4 -v1.0E-12::255255000+1.0E-13::000000255+1.0E-14::000255000+1.0E-15::000255255
TYPE plot_51R.html >>results.html
%PGM%\poleplot +g1 -cpdump_510 -l0.5
RENAME poleplot.html plot_51P.html
TYPE plot_51P.html >>results.html
DEL LABELS.CFG CONC.510 WARNING.510 MESSAGE.510 VMSDIST.510

REM ------------------------
ECHO 'TITLE^&','Backward Dispersion for Attribution (011)^&' >LABELS.CFG
%PGM%\hycs_std 011
%PGM%\concplot +g1 -icdump_011 -oplot_011.html -c4 -v1.0E-12::255255000+1.0E-13::000000255+1.0E-14::000255000+1.0E-15::000255255
DEL LABELS.CFG CONC.011 WARNING.011 MESSAGE.011 VMSDIST.011
TYPE plot_011.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Dispersion using Deformation (012)^&' >LABELS.CFG
%PGM%\hycs_std 012
%PGM%\concplot +g1 -icdump_012 -oplot_012.html -c4 -v1.0E-12::255255000+1.0E-13::000000255+1.0E-14::000255000+1.0E-15::000255255
DEL LABELS.CFG CONC.012 WARNING.012 MESSAGE.012 VMSDIST.012
TYPE plot_012.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Top-Hat Linear Puff Dispersion (013)^&' >LABELS.CFG
%PGM%\hycs_std 013
%PGM%\concplot +g1 -icdump_013 -oplot_013.html -c4 -v1.0E-12::255255000+1.0E-14::000000255+1.0E-16::000255000+1.0E-18::000255255
DEL LABELS.CFG CONC.013 WARNING.013 MESSAGE.013 VMSDIST.013
TYPE plot_013.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Top-Hat Empirical Puff Dispersion (014)^&' >LABELS.CFG
%PGM%\hycs_std 014
%PGM%\concplot +g1 -icdump_014 -oplot_014.html -c4 -v1.0E-12::255255000+1.0E-13::000000255+1.0E-14::000255000+1.0E-15::000255255
DEL LABELS.CFG CONC.014 WARNING.014 MESSAGE.014 VMSDIST.014
TYPE plot_014.html >>results.html

REM ------------------------
REM gaussian empirical dispersion (force contours)
ECHO 'TITLE^&','Gaussian Empirical Puff Dispersion (015)^&' >LABELS.CFG
%PGM%\hycs_std 015
%PGM%\concplot +g1 -icdump_015 -oplot_015.html -c4 -v1E-13+1E-14+1E-15+1E-16
DEL LABELS.CFG CONC.015 WARNING.015 MESSAGE.015 VMSDIST.015
TYPE plot_015.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Deposition using Size and Density (016)^&' >LABELS.CFG
%PGM%\hycs_std 016
%PGM%\concplot +g1 -icdump_016 -oplot_016.html -c4 -v1.0E-10::255255000+1.0E-12::000000255+1.0E-14::000255000+1.0E-16::000255255
DEL LABELS.CFG CONC.016 WARNING.016 MESSAGE.016 VMSDIST.016
TYPE plot_016.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Deposition using Multiple Size Bins (116)^&' >LABELS.CFG
%PGM%\hycs_std 116
%PGM%\concplot +g1 -s0 -icdump_116 -oplot_116.html -c4 -v1.0E-9::255255000+1.0E-11::000000255+1.0E-13::000255000+1.0E-15::000255255
DEL LABELS.CFG CONC.116 WARNING.116 MESSAGE.116 VMSDIST.116
TYPE plot_116.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Deposition using specified velocity (017)^&' >LABELS.CFG
%PGM%\hycs_std 017
%PGM%\concplot +g1 -icdump_017 -oplot_017.html -c4 -v1.0E-9::255255000+1.0E-10::000000255+1.0E-11::000255000+1.0E-12::000255255
DEL LABELS.CFG CONC.017 WARNING.017 MESSAGE.017 VMSDIST.017
TYPE plot_017.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Probability Deposition using 1000 particles (018)^&' >LABELS.CFG
%PGM%\hycs_std 018
%PGM%\concplot +g1 -icdump_018 -oplot_018.html -c4 -v1.0E-9::255255000+1.0E-10::000000255+1.0E-11::000255000+1.0E-12::000255255
DEL LABELS.CFG CONC.018 WARNING.018 MESSAGE.018 VMSDIST.018
TYPE plot_018.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Probability Deposition using 10000 particles (019)^&' >LABELS.CFG
%PGM%\hycs_std 019
%PGM%\concplot +g1 -icdump_019 -oplot_019.html -c4 -v1.0E-10::255255000+1.0E-11::000000255+1.0E-12::000255000+1.0E-13::000255255
DEL LABELS.CFG CONC.019 WARNING.019 MESSAGE.019 VMSDIST.019
TYPE plot_019.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Species conversion at 10%% per hour (020)^&' >LABELS.CFG
%PGM%\hycs_std 020
%PGM%\concplot +g1 -s2 -icdump_020 -oplot_020.html -c4 -v1E-6+1E-7+1E-8+1E-9
DEL LABELS.CFG CONC.020 WARNING.020 MESSAGE.020 VMSDIST.020
TYPE plot_020.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Cycle Short Emissions Every 3 hrs (021)^&' >LABELS.CFG
%PGM%\hycs_std 021
%PGM%\concplot +g1 -icdump_021 -oplot_021.html -c4 -v1.0E-11::255255000+1.0E-13::000000255+1.0E-15::000255000+1.0E-17::000255255
DEL LABELS.CFG CONC.021 WARNING.021 MESSAGE.021 VMSDIST.021
TYPE plot_021.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Simple 4-point Moving Line Source (022)^&' >LABELS.CFG
ECHO YYYY MM DD HH    DURATION(hhhh) #RECORDS >EMITIMES
ECHO YYYY MM DD HH MM DURATION(hhmm) LAT LON HGT(m) RATE(/h) AREA(m2) HEAT(w) >>EMITIMES
ECHO 1995 10 16 00 0003 1 >>EMITIMES
ECHO 1995 10 16 00 00 0100 40.000 -90.000 10.0 1.0 0.0 0.0 >>EMITIMES
ECHO 1995 10 16 03 0003 1 >>EMITIMES
ECHO 1995 10 16 03 00 0100 40.000 -88.000 10.0 1.0 0.0 0.0 >>EMITIMES
ECHO 1995 10 16 06 0003 1 >>EMITIMES
ECHO 1995 10 16 06 00 0100 40.000 -86.000 10.0 1.0 0.0 0.0 >>EMITIMES
ECHO 1995 10 16 09 0003 1 >>EMITIMES
ECHO 1995 10 16 09 00 0100 40.000 -84.000 10.0 1.0 0.0 0.0 >>EMITIMES
%PGM%\hycs_std 022
%PGM%\concplot +g1 -icdump_022 -oplot_022.html -c4 -v1.0E-12::255255000+1.0E-13::000000255+1.0E-14::000255000+1.0E-15::000255255
DEL EMITIMES LABELS.CFG CONC.022 WARNING.022 MESSAGE.022 VMSDIST.022
TYPE plot_022.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Simultaneous Multiple Sources in Space (023)^&' >LABELS.CFG
%PGM%\hycs_std 023
%PGM%\concplot +g1 -icdump_023 -oplot_023.html -c4 -v1.0E-12::255255000+1.0E-13::000000255+1.0E-14::000255000+1.0E-15::000255255
DEL LABELS.CFG CONC.023 WARNING.023 MESSAGE.023 VMSDIST.023
TYPE plot_023.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Simultaneous Multiple Sources in Height (024)^&' >LABELS.CFG
%PGM%\hycs_std 024
%PGM%\concplot +g1 -icdump_024 -oplot_024.html -c4 -v1.0E-13::255255000+1.0E-14::000000255+1.0E-15::000255000+1.0E-16::000255255
DEL LABELS.CFG CONC.024 WARNING.024 MESSAGE.024 VMSDIST.024
TYPE plot_024.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Vertical Line Source (025)^&' >LABELS.CFG
%PGM%\hycs_std 025
%PGM%\concplot +g1 -icdump_025 -oplot_025.html -c4 -v1.0E-14::255255000+1.0E-16::000000255+1.0E-18::000255000+1.0E-20::000255255
DEL LABELS.CFG CONC.025 WARNING.025 MESSAGE.025 VMSDIST.025
TYPE plot_025.html >>results.html

REM ------------------------
ECHO 'TITLE^&','9-Member Meteorological Ensemble Means (026)^&' >LABELS.CFG
COPY CONTROL.026 CONTROL
for %%f in (1 2 3 4 5 6 7 8 9) do %PGM%\hycs_ens %%f
%PGM%\conprob -bcdump_026
%PGM%\concplot +g1 -icmean -oplot_026.html -c4 -v1.0E-12::255255000+1.0E-14::000000255+1.0E-16::000255000+1.0E-18::000255255
DEL CONTROL CMEAN CNUMB CVARN CMAX?? PROB?? WARNING.00? MESSAGE.00? VMSDIST.00? CDUMP_026.00?
DEL LABELS.CFG CONC.CFG
TYPE plot_026.html >>results.html

REM ------------------------
ECHO 'TITLE^&','9-Member Variance Ensemble Means (027)^&' >LABELS.CFG
COPY SETUP.027 SETUP.CFG
COPY CONTROL.027 CONTROL
for %%f in (1 2 3 4 5 6 7 8 9) do %PGM%\hycs_var %%f
%PGM%\conprob -bcdump_027
%PGM%\concplot +g1 -icmean -oplot_027.html -c4 -v1.0E-12::255255000+1.0E-13::000000255+1.0E-14::000255000+1.0E-15::000255255
DEL CONTROL CMEAN CNUMB CVARN CMAX?? PROB?? WARNING.00? MESSAGE.00? VMSDIST.00? CDUMP_027.00?
DEL LABELS.CFG CONC.CFG SETUP.CFG
TYPE plot_027.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Dust Storm Emission Algorithm (028)^&' >LABELS.CFG
COPY SETUP.028 SETUP.CFG
COPY CONTROL.028 CONTROL
%PGM%\dustbdy
%PGM%\hycs_std
%PGM%\concplot +g1 -uug -x1E+6 -icdump_028 -oplot_028.html -81 -c4 -v10000::255255000+1000::000000255+100::000255000+10::000255255
DEL fort.70 CONTROL WARNING MESSAGE VMSDIST LABELS.CFG CONC.CFG SETUP.CFG
TYPE plot_028.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Matrix - Downwind Receptors (029)^&' >LABELS.CFG
COPY SETUP.029 SETUP.CFG
COPY CONTROL.029 CONTROL
%PGM%\latlon
%PGM%\hycs_std
%PGM%\matrix -icdump_029 -y40.0 -x-90.0 -ms -ocdump
%PGM%\concplot +g1 -icdump -oplot_029.html -c4 -v1.0E-13::255255000+1.0E-14::000000255+1.0E-15::000255000+1.0E-16::000255255
DEL CONTROL WARNING MESSAGE VMSDIST CDUMP LABELS.CFG CONC.CFG SETUP.CFG
TYPE plot_029.html >>results.html

REM ------------------------
ECHO 'TITLE^&','Matrix - Upwind Sources (030)^&' >LABELS.CFG
%PGM%\matrix -icdump_029 -y40.0 -x-90.0 -mr -ocdump
%PGM%\concplot +g1 -icdump -oplot_030.html -c4 -v1.0E-13::255255000+1.0E-15::000000255+1.0E-17::000255000+1.0E-19::000255255
DEL CDUMP LABELS.CFG CONC.CFG
TYPE plot_030.html >>results.html

REM -------------------------
REM Volcanic ash - vertical line, multi-pollutant, multi-level
%PGM%\hycs_std 031
%PGM%\concrop -icdump_131 -occrop_131 -f0
%PGM%\volcplot +g1 -iccrop_131 -10 -21 -30 -40 -5TEST-131 -61 -z90 -oplot_131.html
DEL LABELS.CFG DATE.TXT CCROP_131

ECHO 'TITLE^&','Volcanic ash (231)^&' >LABELS.CFG
%PGM%\concplot +g1 -icdump_231 -k3 -s0 -d2 -l115 -z90 -oplot_231.html +l1 -c4 -v1.0E-15::194194194+1.0E-17::029029029+1.0E-19::150150150+1.0E-21::179179179

ECHO 'TITLE^&','Volcanic ash (altitude in feet) (331)^&' >LABELS.CFG
ECHO 'ALTTD^&','ft^&' >>LABELS.CFG
%PGM%\concplot +g1 -icdump_231 -k3 -s0 -d2 -l115 -z90 -oplot_331.html +l1 -c4 -v1.0E-15::194194194+1.0E-17::029029029+1.0E-19::150150150+1.0E-21::179179179

DEL LABELS.CFG CONC.031 WARNING.031 MESSAGE.031 VMSDIST.031

TYPE plot_131.html >>results.html
TYPE plot_231.html >>results.html
TYPE plot_331.html >>results.html

REM ------------------------
REM RSMC - Regional Specialized Meteorological Center

%PGM%\hyts_std 032
ECHO 'TITLE^&','RSMC Trajectory (032)^&' >LABELS.CFG
%PGM%\trajplot +g1 -itdump_032 -z80 -oplot_032.html
TYPE plot_032.html >>results.html
DEL LABELS.CFG TRAJ.CFG WARNING.032 MESSAGE.032 VMSDIST.032

%PGM%\hycs_std 033
ECHO 'TITLE^&','RSMC Dispersion (033)^&' >LABELS.CFG
%PGM%\concrop -icdump_033 -occrop_033 -f0
@REM 5 MAR 2020
@REM Note contour levels are not set because there are multiple
@REM plots and contour levels are different from one plot to another.
%PGM%\concplot +g1 -iccrop_033 -e1 -r3 -uBq -z80 -oplot_033.html +l1
DEL cdump2
TYPE plot_033.html >>results.html
DEL CCROP_033 LABELS.CFG CONC.033 WARNING.033 MESSAGE.033 VMSDIST.033

ECHO 'TITLE^&','RSMC Dispersion With Dynamic Colors (133)^&' >LABELS.CFG
%PGM%\concrop -icdump_033 -occrop_133 -f0
@REM 5 MAR 2020
@REM Note contour levels are not set because there are multiple
@REM plots and contour levels are different from one plot to another.
%PGM%\concplot +g1 -iccrop_133 -e1 -r3 -uBq -z80 -31 -oplot_133.html +l1
TYPE plot_133.html >>results.html
DEL -f LABELS.CFG

REM ------------------------
REM HLS

%PGM%\hycs_std 034
ECHO 'TITLE^&','HLS fine with shapefile for map (134)^&' >LABELS.CFG
%PGM%\concrop -icdump_134 -occrop_134 -f0
COPY ..\graphics\roads.shp .
COPY ..\graphics\county.shp .
COPY ..\graphics\states.shp .
COPY ..\graphics\shapefiles_rds.txt shapefiles.txt
%PGM%\concplot +g1 -iccrop_134 -d2 -z80 -jshapefiles.txt -oplot_134.html -c4 -v1.0E-10::255255000+1.0E-11::000000255+1.0E-12::000255000+1.0E-13::000255255
TYPE plot_134.html >>results.html
DEL roads.shp county.shp states.shp
DEL shapefiles.txt

ECHO 'TITLE^&','HLS coarse (234)^&' >LABELS.CFG
%PGM%\concrop -icdump_234 -occrop_234 -f0
%PGM%\concplot +g1 -iccrop_234 -d2 -z80 -oplot_234.html -c4 -v1.0E-12::255255000+1.0E-13::000000255+1.0E-14::000255000+1.0E-15::000255255
TYPE plot_234.html >>results.html
DEL CCROP_134 CCROP_234 LABELS.CFG CONC.034 WARNING.034 MESSAGE.034 VMSDIST.034

REM ------------------------
ECHO 'TITLE^&','Short-Range Dispersion Simulation (035)^&' >LABELS.CFG
%PGM%\hycs_std 035
%PGM%\concplot +g1 -icdump_035 -oplot_035.html -c4 -v1.0E-09::255255000+1.0E-10::000000255+1.0E-11::000255000+1.0E-12::000255255
DEL LABELS.CFG CONC.035 WARNING.035 MESSAGE.035 VMSDIST.035
TYPE plot_035.html >>results.html

REM -------------------------
REM Trajectory cluster analysis
CALL XCLUSTER_SVG.BAT

REM ------------------------
echo 'TITLE^&','Trajectory Frequency Plot^&' >LABELS.CFG
echo 'MAPID^&',' Values ^&'                 >>LABELS.CFG
echo 'UNITS^&',' %%^&'                      >>LABELS.CFG
echo 'VOLUM^&',' ^&'                        >>LABELS.CFG
COPY ..\cluster\example\output\INFILE INFILE
%PGM%\trajfreq -iINFILE -ftrajfreq.bin
%PGM%\concplot +g1 -itrajfreq.bin -otrajfreq -z80 -c4 -v10+5+2+1+0.5+0.2+0.1
TYPE trajfreq.html >>results.html
DEL LABELS.CFG trajfreq.bin INFILE

REM ------------------------
REM GLOBAL MODEL

ECHO 'TITLE^&','Transport to 24 hours (036)^&' >LABELS.CFG
%PGM%\hycs_std 036
%PGM%\concplot +g1 -icdump_036 -d2 -z80 -oplot_036.html -c4 -v1.0E-11::255255000+1.0E-12::000000255+1.0E-13::000255000+1.0E-14::000255255
DEL LABELS.CFG CONC.036 WARNING.036 MESSAGE.036 VMSDIST.036
TYPE plot_036.html >>results.html

echo ^&GEMPARM        >GEMPARM.CFG
echo wfreq = 6,      >>GEMPARM.CFG
echo ^/              >>GEMPARM.CFG
%PGM%\hycs_gem 037

ECHO 'TITLE^&','GEM@12h - Lagrangian grid (037)^&' >LABELS.CFG
%PGM%\concplot +g1 -icdump_037 -d2 -z80 -oplot_037.html -c4 -v1.0E-11::255255000+1.0E-12::000000255+1.0E-13::000255000+1.0E-14::000255255
DEL GEMDUMP.BIN GEMPARM.CFG LABELS.CFG CONC.037 WARNING.037 MESSAGE.037 VMSDIST.037
TYPE plot_037.html >>results.html

ECHO 'TITLE^&','GEM@12h - Eulerian grid (137)^&' >LABELS.CFG
%PGM%\gridplot +g1 -igemconc.bin -oplot_137 -l-1 -d10.0
RENAME GEMCONC.BIN gemconc_037
DEL LABELS.CFG 
TYPE plot_137.html >>results.html

ECHO 'TITLE^&','GEM@12h - Lagrangian + Eulerian grid (237)^&' >LABELS.CFG
%PGM%\concadd -icdump_037 -bgemconc_037 -ogemhysp_037
%PGM%\gridplot +g1 -igemhysp_037 -oplot_237 -l-1 -d10.0
DEL LABELS.CFG
TYPE plot_237.html >>results.html

ECHO 'TITLE^&','Chemical Dispersion Simulation (038)^&' >LABELS.CFG
%PGM%\hycs_std 038
%PGM%\concplot +g1 -icdump_038 -oplot_038.html -m0 -d2 -b100 -t100 -e2 -c4 -r2 -z70 -uppm -x156411.162068966 -y453592.37 -k1 -v20:AEGL-3+2:AEGL-2+0.5:AEGL-1+0.0:AEGL
DEL LABELS.CFG CONC.038 WARNING.038 MESSAGE.038 VMSDIST.038
TYPE plot_038.html >>results.html

ECHO 'TITLE^&','Chemical Dispersion Simulation (138)^& with INITV=0' >LABELS.CFG
%PGM%\hycs_std 138
%PGM%\concplot +g1 -icdump_138 -oplot_138.html -m0 -d2 -b100 -t100 -e2 -c4 -r2 -z70 -uppm -x156411.162068966 -y453592.37 -k1 -v20:AEGL-3+2:AEGL-2+0.5:AEGL-1+0.0:AEGL
DEL LABELS.CFG CONC.138 WARNING.138 MESSAGE.138 VMSDIST.138
TYPE plot_138.html >>results.html

REM -------------------------

%PGM%\hycs_std 039
ECHO 'TITLE^&','CAPTEX Release Number Two (039)^&' >LABELS.CFG
%PGM%\concplot +g1 -icdump_039 -oplot_039.html -z90 -x1.0E+12 -n3:8 -upg -q..\datem\captex2.txt -c4 -v3000+1000+300+100+30+10
DEL CONC.039 WARNING.039 MESSAGE.039 VMSDIST.039
TYPE plot_039.html >>results.html

%PGM%\c2datem  -icdump_039 -ocdump_039.txt -m..\datem\captex2.txt -c1.0E+12 -xi
%PGM%\statmain -t0 -o1 -l10 -d..\datem\captex2.txt -rcdump_039.txt
%PGM%\scatter +g1 -idataA.txt -oplot_139.html -p10
TYPE plot_139.html >>results.html
DEL dataA.txt cdump_039.txt MESSAGE.txt
RENAME statA.txt stat_039.txt

ECHO 'TITLE^&','CAPTEX Time Series at Station 608^&' >LABELS.CFG
echo 'UNITS^&',' pg^&'                              >>LABELS.CFG
ECHO 608 41.98 -77.57 >station.txt
%PGM%\con2stn -icdump_039 -ohysp_608.txt -r2 -c1.0E+12 -sstation.txt
%PGM%\timeplot +g1 -ihysp_608.txt -s..\datem\captex2_608.txt -r2
TYPE timeplot.html >>results.html
RENAME timeplot.html plot_239.html
DEL station.txt LABELS.CFG hysp_608.txt 

REM -------------------------

%PGM%\hycs_std 040
ECHO 'TITLE^&','CAPTEX R#2 vary VSCALE (040)^&' >LABELS.CFG
%PGM%\concplot +g1 -icdump_040 -oplot_040.html -z90 -x1.0E+12 -n3:8 -upg -q..\datem\captex2.txt -c4 -v3000+1000+300+100+30+10
DEL CONC.040 WARNING.040 MESSAGE.040 VMSDIST.040
TYPE plot_040.html >>results.html

%PGM%\c2datem  -icdump_040 -ocdump_040.txt -m..\datem\captex2.txt -c1.0E+12 -xi
%PGM%\statmain -t0 -o1 -l10 -d..\datem\captex2.txt -rcdump_040.txt
%PGM%\scatter +g1 -idataA.txt -oplot_140.html -p10
TYPE plot_140.html >>results.html
DEL dataA.txt cdump_040.txt MESSAGE.txt
RENAME statA.txt stat_040.txt

ECHO 'TITLE^&','CAPTEX Time Series at Stn 608 vary VSCALE^&' >LABELS.CFG
echo 'UNITS^&',' pg^&'                              >>LABELS.CFG
ECHO 608 41.98 -77.57 >station.txt
%PGM%\con2stn -icdump_040 -ohysp_608.txt -r2 -c1.0E+12 -sstation.txt
%PGM%\timeplot +g1 -ihysp_608.txt -s..\datem\captex2_608.txt -r2
TYPE timeplot.html >>results.html
RENAME timeplot.html plot_240.html
DEL station.txt LABELS.CFG hysp_608.txt 

REM -------------------------
REM Sweat Farm Road, Georgia wildfire

echo "TITLE&","Sweat Farm Road Smoke Simulation &" >LABELS.CFG
copy ..\examples\wildfire\EMITIMES.txt EMITIMES
%PGM%\hycs_std 041
%PGM%\concplot +g1 -icdump_041 -oplot_041.html -b100 -t100  -c4 -k2 -z90 -uug -n40:40 -v500+200+100+50+20+10
DEL LABELS.CFG CONC.041 WARNING.041 MESSAGE.041 VMSDIST.041 EMITIMES
TYPE plot_041.html >>results.html

%PGM%\c2datem -icdump_041 -ocdump_041.txt -m..\examples\wildfire\Mayo_DATEM.txt -xi
%PGM%\statmain -t0 -o1 -l10 -d..\examples\wildfire\Mayo_DATEM.txt -rcdump_041.txt
%PGM%\scatter +g1 -idataA.txt -oplot_141.html -p10
TYPE plot_141.html >>results.html
RENAME statA.txt stat_041.txt
DEL dataA.txt cdump_041.txt

echo "TITLE&","Time Series at Station Mayo&" >LABELS.CFG
echo "UNITS&"," ug&" >>LABELS.CFG
echo 1 30.2556 -81.4533 >station.txt
%PGM%\con2stn -icdump_041 -ohysp_Mayo.txt -r1 -xi -sstation.txt
%PGM%\timeplot +g1 -ihysp_Mayo.txt -s..\examples\wildfire\mayo_pm25.txt
TYPE timeplot.html >>results.html
RENAME timeplot.html plot_241.html
DEL station.txt LABELS.CFG hysp_mayo.txt


REM ------------------------
REM Volcano  Soufriere_Hills with DATEM stats

ECHO 'TITLE^&','Volcano  Soufriere_Hills Simulation (042)^&' >LABELS.CFG
ECHO 'VOLUM^&','/m2^&'                                      >>LABELS.CFG
ECHO 'MAPID^&','Mass loading^&'                             >>LABELS.CFG

COPY ..\examples\volcano\MAPTEXT.042 MAPTEXT.CFG

%PGM%\hycs_std 042

%PGM%\concplot +g1 -icdump_042 -oplot_042 -m0 -d2 -z75 -n14:14 -a0 -k2 -s0 -l115 -ug  -x1000 -q..\examples\volcano\datem_mass.txt -e4 -c4 -v10::255255000+1::000000255+0.1::000255000+0.01::000255255
TYPE plot_042.html >>results.html
%PGM%\parxplot +g1 -iPARDUMP_042 -k1 -n3 -oplot_142.html
TYPE plot_142.html >>results.html
DEL LABELS.CFG CONC.042 WARNING.042 MESSAGE.042 VMSDIST.042 MAPTEXT.CFG

%PGM%\concsum -icdump_042 -ocsum_042
%PGM%\c2datem -icsum_042.bin -omodel.txt -m..\examples\volcano\datem_mass.txt -c1.8e+07
%PGM%\statmain -d..\examples\volcano\datem_mass.txt -rmodel.txt -o1 -t0 -l1.0 -b
%PGM%\scatter +g1 -idataA.txt -oplot_242.html -p0.1
RENAME statA.txt stat_042.txt
DEL dataA.txt model.txt message.txt csum_042.bin
TYPE plot_242.html >>results.html

REM -------------------------
REM line source test

ECHO 'TITLE^&','Four line sources with an EMITTIMES file (043)^&' >LABELS.CFG
ECHO 'VOLUM^&','/m2^&'                                     >>LABELS.CFG
ECHO 'MAPID^&','Mass loading^&'                            >>LABELS.CFG

ECHO YYYY MM DD HH    DURATION(hhhh) #RECORDS  > EMITTIMES.txt
ECHO YYYY MM DD HH MM DURATION(hhmm) LAT LON HGT(m) RATE(/h) AREA(m2) HEAT(w)  >> EMITTIMES.txt
ECHO  1995 10 17 00  9999  8                                                   >> EMITTIMES.txt
ECHO  1995 10 17 00 00 0100 40.50 -91.00 2000   0.5 0   0                       >> EMITTIMES.txt
ECHO  1995 10 17 00 00 0100 40.50 -91.00 10000  0.0 0   0                       >> EMITTIMES.txt
ECHO  1995 10 17 00 00 0100 40.00 -90.50 10000  1.0 0   0                       >> EMITTIMES.txt
ECHO  1995 10 17 00 00 0100 40.00 -90.50 15000  0.0 0   0                       >> EMITTIMES.txt
ECHO  1995 10 17 00 00 0100 39.50 -90.00 4000   1.5 0   0                       >> EMITTIMES.txt
ECHO  1995 10 17 00 00 0100 39.50 -90.00 5000   0.0 0   0                       >> EMITTIMES.txt
ECHO  1995 10 17 00 00 0100 39.00 -89.50 1000   2.0 0   0                       >> EMITTIMES.txt
ECHO  1995 10 17 00 00 0100 39.00 -89.50 10000  0.0 0   0                       >> EMITTIMES.txt

%PGM%\hycs_std 043
%PGM%\concplot +g1 -icdump_043 -oplot_043.html -d2 -s0 -e4 -c4 -v1.0E-09::255255000+1.0E-10::000000255+1.0E-11::000255000+1.0E-12::000255255
TYPE plot_043.html >>results.html
%PGM%\parxplot +g1 -iPARDUMP_043 -k1 -oplot_043.html
DEL LABELS.CFG EMITTIMES.txt  MESSAGE.043 CONC.043 VMSDIST.043
TYPE plot_043.html >>results.html
DEL PARDUMP_043 cdump_043

REM -------------------------

%PGM%\hycs_std 050
ECHO 'TITLE^&','CAPTEX Release Number Two ARW-WRF kblt=2 (050)^&' >LABELS.CFG
%PGM%\concplot +g1 -icdump_050 -oplot_050.html -z90 -x1.0E+12 -n3:8 -upg -q..\datem\captex2.txt -c4 -v3000+1000+300+100+30+10
DEL CONC.050 WARNING.050 MESSAGE.050 VMSDIST.050
TYPE plot_050.html >>results.html

%PGM%\c2datem  -icdump_050 -ocdump_050.txt -m..\datem\captex2.txt -c1.0E+12 -xi
%PGM%\statmain -t0 -o1 -l10 -d..\datem\captex2.txt -rcdump_050.txt
%PGM%\scatter +g1 -idataA.txt -oplot_150.html -p10
TYPE plot_150.html >>results.html
DEL dataA.txt cdump_050.txt MESSAGE.txt
RENAME statA.txt stat_050.txt

ECHO 'TITLE^&','CAPTEX Time Series at Station 608 ARW-WRF kblt=2^&' >LABELS.CFG
echo 'UNITS^&',' pg^&'                              >>LABELS.CFG
ECHO 608 41.98 -77.57 >station.txt
%PGM%\con2stn -icdump_050 -ohysp_608.txt -r2 -c1.0E+12 -sstation.txt
%PGM%\timeplot +g1 -ihysp_608.txt -s..\datem\captex2_608.txt -r2
TYPE timeplot.html >>results.html
RENAME timeplot.html plot_250.html
DEL station.txt LABELS.CFG hysp_608.txt

REM -------------------------

%PGM%\hycs_std 051
ECHO 'TITLE^&','CAPTEX Release Number Two ARW-WRF kblt=3 (051)^&' >LABELS.CFG
%PGM%\concplot +g1 -icdump_051 -oplot_051.html -z90 -x1.0E+12 -n3:8 -upg -q..\datem\captex2.txt -c4 -v3000+1000+300+100+30+10
DEL CONC.051 WARNING.051 MESSAGE.051 VMSDIST.051
TYPE plot_051.html >>results.html

%PGM%\c2datem  -icdump_051 -ocdump_051.txt -m..\datem\captex2.txt -c1.0E+12 -xi
%PGM%\statmain -t0 -o1 -l10 -d..\datem\captex2.txt -rcdump_051.txt
%PGM%\scatter +g1 -idataA.txt -oplot_151.html -p10
TYPE plot_151.html >>results.html
DEL dataA.txt cdump_051.txt MESSAGE.txt
RENAME statA.txt stat_051.txt

ECHO 'TITLE^&','CAPTEX Time Series at Station 608 ARW-WRF kblt=3^&' >LABELS.CFG
echo 'UNITS^&',' pg^&'                              >>LABELS.CFG
ECHO 608 41.98 -77.57 >station.txt
%PGM%\con2stn -icdump_051 -ohysp_608.txt -r2 -c1.0E+12 -sstation.txt
%PGM%\timeplot +g1 -ihysp_608.txt -s..\datem\captex2_608.txt -r2
TYPE timeplot.html >>results.html
RENAME timeplot.html plot_251.html
DEL station.txt LABELS.CFG hysp_608.txt

REM -------------------------

%PGM%\hycs_std 052
ECHO 'TITLE^&','CAPTEX Release Number Two ARW-WRF STILT Features (052)^&' >LABELS.CFG
%PGM%\concplot +g1 -icdump_052 -oplot_052.html -z90 -x1.0E+12 -n3:8 -upg -q..\datem\captex2.txt -c4 -v3000+1000+300+100+30+10
DEL CONC.052 WARNING.052 MESSAGE.052 VMSDIST.052
TYPE plot_052.html >>results.html

%PGM%\c2datem  -icdump_052 -ocdump_052.txt -m..\datem\captex2.txt -c1.0E+12 -xi
%PGM%\statmain -t0 -o1 -l10 -d..\datem\captex2.txt -rcdump_052.txt
%PGM%\scatter +g1 -idataA.txt -oplot_152.html -p10
TYPE plot_152.html >>results.html
DEL dataA.txt cdump_052.txt MESSAGE.txt
RENAME statA.txt stat_052.txt

ECHO 'TITLE^&','CAPTEX Time Series at Station 608 ARW-WRF STILT Features^&' >LABELS.CFG
echo 'UNITS^&',' pg^&'                              >>LABELS.CFG
ECHO 608 41.98 -77.57 >station.txt
%PGM%\con2stn -icdump_052 -ohysp_608.txt -r2 -c1.0E+12 -sstation.txt
%PGM%\timeplot +g1 -ihysp_608.txt -s..\datem\captex2_608.txt -r2
TYPE timeplot.html >>results.html
RENAME timeplot.html plot_252.html
DEL station.txt LABELS.CFG hysp_608.txt

REM ------------------------
REM FIX PAGE NUMBERS IN CONCATENATED FILE

date /t >>xtime.txt
time /t >>xtime.txt

RENAME results.html results_tmp.html
%PGM%\cat2svg -iresults_tmp.html -oresults.html
DEL results_tmp.html
