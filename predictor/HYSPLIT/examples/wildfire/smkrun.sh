#!/bin/sh

#---------------------------
# HYSPLIT simulation of Sweat Farm Road, Georgia wildfire
export PGM=../../exec/

rm -f *.ps
rm -f cdump*
# ------------------------
echo "'TITLE&','Sweat Farm Road Smoke Simulation &'" >LABELS.CFG
cp setup_wildfire SETUP.CFG
cp control_wildfire CONTROL 
cp EMITIMES.txt EMITIMES
cp ../../bdyfiles/ASCDATA.CFG . 
cp ../../graphics/arlmap . 
${PGM}hycs_std

${PGM}/concplot -icdump -oplot.ps -b100 -t100  -c4 -k2 -z90 -uug -n40:40 -v500+200+100+50+20+10
rm -f LABELS.CFG SETUP.CFG CONC.CFG MESSAGE VMSDIST
cat plot.ps >>results.ps

${PGM}c2datem  -icdump -ocdump.txt -mMayo_DATEM.txt -xi
${PGM}statmain -t0 -o1 -l10 -dMayo_DATEM.txt -rcdump.txt
${PGM}scatter -idataA.txt -oplot.ps -p10
cat plot.ps >>results.ps
rm -f cdump.txt

echo "'TITLE&','Time Series at Station Mayo&'" >LABELS.CFG
echo "'UNITS&',' ug&'                              ">>LABELS.CFG
echo 1 30.2556 -81.4533 >station.txt
${PGM}con2stn -icdump -ohysp_Mayo.txt -r1 -xi -sstation.txt
${PGM}timeplot -ihysp_Mayo.txt -smayo_pm25.txt
cat timeplot.ps >>results.ps
mv timeplot.ps plot.ps
rm -f station.txt LABELS.CFG hysp_mayo.txt

# ------------------------
# fix page numbers in concatenated file
  if [ -s results.ps ]; then
     mv results.ps results.tmp.ps
     ${PGM}/catps2ps -iresults.tmp.ps -oresults.ps
     rm results.tmp.ps
  fi
rm -f hysp_Mayo.txt
rm -f plot.ps
rm -f arlmap
rm -f ASCDATA.CFG
rm -f CONTROL
rm -f EMITIMES
rm -f cdump
chmod g+w *
