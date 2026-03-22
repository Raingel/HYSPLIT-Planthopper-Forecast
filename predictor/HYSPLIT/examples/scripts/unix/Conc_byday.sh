#!/bin/ksh

#-------------------------------------------------------------
# set default directory structure if not passed through

  MDL="/home/hysplit4"
  OUT="."
  MET="/work/data"
  cd $OUT

#--------------------------------------------------------------
# set model simulation variables    

  poll="IND"

  syr=98
  smo=05
  shr=00
     
  olat=27.00
  olon=72.05
  olvl=10.0
        
  let run=840
  ztop=10000.0
  met1="fnl.nh.may98.001"
  met2="fnl.nh.may98.002"
  met3="fnl.nh.jun98.001"


#----------------------------------------------------------
# basic simulation loop

  for sda in 12 13 14 15; do

#----------------------------------------------------------
# set up control file for dispersion/concentration simulation

  echo "$syr $smo $sda $shr    " >CONTROL
  echo "1                      ">>CONTROL
  echo "$olat $olon $olvl      ">>CONTROL
  echo "$run                   ">>CONTROL
  echo "0                      ">>CONTROL
  echo "$ztop                  ">>CONTROL
  echo "3                      ">>CONTROL
  echo "$MET/                  ">>CONTROL
  echo "$met1                  ">>CONTROL
  echo "$MET/                  ">>CONTROL
  echo "$met2                  ">>CONTROL
  echo "$MET/                  ">>CONTROL
  echo "$met3                  ">>CONTROL
  echo "1                      ">>CONTROL
  echo "$poll                  ">>CONTROL
  echo "10.0                   ">>CONTROL
  echo "0.1                    ">>CONTROL
  echo "$syr $smo $sda $shr 00 ">>CONTROL
  echo "1                      ">>CONTROL
  echo "30.0 70.0              ">>CONTROL
  echo "0.50 0.50              ">>CONTROL
  echo "20.0 20.0              ">>CONTROL
  echo "$OUT/                  ">>CONTROL
  echo "cdump                  ">>CONTROL
  echo "1                      ">>CONTROL
  echo "10                     ">>CONTROL
  echo "00 00 00 00 00         ">>CONTROL
  echo "00 00 00 00 00         ">>CONTROL
  echo "00 24 00               ">>CONTROL
  echo "1                      ">>CONTROL
  echo "0.0 0.0 0.0            ">>CONTROL
  echo "0.0 0.0 0.0 0.0 0.0    ">>CONTROL
  echo "0.0 0.0 0.0            ">>CONTROL
  echo "5.27                   ">>CONTROL
  echo "0.0                    ">>CONTROL

#----------------------------------------------------------
# run the concentration simulation

  if [ -f cdump ];then rm cdump; fi
  ${MDL}/exec/hycs_std   
  mv cdump ${poll}${smo}${sda}

#----------------------------------------------------------
# simulation loop exit 

  let run=$run-24  
  done
