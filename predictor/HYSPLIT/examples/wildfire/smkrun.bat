rem HYSPLIt simulation of Sweat Farm Road, Georgia wildfire
rem

echo "TITLE&","Sweat Farm Road Smoke Simulation &" >>LABELS.CFG
copy setup_wildfire SETUP.CFG
copy control_wildfire CONTROL
copy EMITIMES.txt EMITIMES
copy ..\..\bdyfiles\ASCDATA.CFG ASCDATA.CFG
copy ..\..\graphics\arlmap arlmap

REM run the model
..\..\exec\hycs_std

del SETUP.CFG
del CONTROL
del EMITIMES
del ASCDATA.CFG

REM plot hour 1600 smoke concentration map
..\..\exec\concplot -icdump -oplot.ps -b100 -t100  -c4 -k2 -z90 -uug -n40:40 -v500+200+100+50+20+10

del LABELS.CFG
del MESSAGE
del VMSDIST

copy plot.ps results.ps
echo "TITLE&","Time Series at Station Mayo&" >>LABELS.CFG
echo "UNITS&"," ug&"                              ">>LABELS.CFG
..\..\exec\c2datem -icdump -ocdump.txt -mMayo_DATEM.txt -xi
..\..\exec\statmain -t0 -o1 -l10 -dMayo_DATEM.txt -rcdump.txt
..\..\exec\scatter -idataA.txt -oplot.ps -p10
type plot.ps >>results.ps
del cdump.txt
del plot.ps

echo 
echo 1 30.2556 -81.4533 >station.txt
..\..\exec\con2stn -icdump -ohysp_Mayo.txt -r1 -xi -sstation.txt
..\..\exec\timeplot -ihysp_Mayo.txt -smayo_pm25.txt
type timeplot.ps >>results.ps
del hysp_Mayo.txt
del timeplot.ps
del station.txt
del LABELS.CFG
del arlmap
del cdump
del CONC.CFG

