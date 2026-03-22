#!/bin/ksh

#-------------------------------------------------------------
# set default directory structure if not passed through

  MDL="/home/hysplit"
  OUT="."
  MET="/work"
  cd $OUT

#--------------------------------------------------------------
# set model simulation variables    

  syr=90
  smo=08
  shr=12
     
  olat=29.0
  olon=48.0
  olvl=100.0
        
  run=-48 
  ztop=10000.0
  met1="aug90"
  met2="sep90"


#----------------------------------------------------------
# basic simulation loop

  for sda in 03 04 05 06 07 08 09 10 11 12 13 14 15 \
      16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31; do

#----------------------------------------------------------
# set up control file for dispersion/concentration simulation

  echo "$syr $smo $sda $shr    " >CONTROL
  echo "1                      ">>CONTROL
  echo "$olat $olon $olvl      ">>CONTROL
  echo "$run                   ">>CONTROL
  echo "0                      ">>CONTROL
  echo "$ztop                  ">>CONTROL
  echo "2                      ">>CONTROL
  echo "$MET/                  ">>CONTROL
  echo "$met1                  ">>CONTROL
  echo "$MET/                  ">>CONTROL
  echo "$met2                  ">>CONTROL
  echo "$OUT/                  ">>CONTROL
  echo "tdump                  ">>CONTROL

#----------------------------------------------------------
# run the concentration simulation

  if [ -f tdump ];then rm tdump; fi
  ${MDL}/exec/hyts_std    
  ${MDL}/exec/trajplot tdump
  mv trajplot.ps plot${smo}${sda}.ps
# mv tdump tdump_${smo}_${sda}

#----------------------------------------------------------
# simulation loop exit 

  done
