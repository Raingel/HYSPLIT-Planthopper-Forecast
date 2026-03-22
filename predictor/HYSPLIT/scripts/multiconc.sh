#!/bin/ksh

#-------------------------------------------------------------
# Set default directory structure if not passed through
# designed to be run from /hysplit/working or some other
# parallel directory. Can be installed elsewhere if needed.

  MDL="../exec"
  MET="/pub/archives/gdas1/"

  OUT="."
  cd $OUT

#--------------------------------------------------------------
# Create the simulation location file. Comment out this section
# if it is to be replaced by a real data file.

  echo "001 35.0 -90.0"  >SAMPLERS.TXT
  echo "002 45.0 -90.0" >>SAMPLERS.TXT
  echo "003 55.0 -90.0" >>SAMPLERS.TXT

#--------------------------------------------------------------
# set model simulation constants    

  shr=00         # starting hour
  olvl=10.0      # starting level
  freq=5         # number of days between simulations

#----------------------------------------------------------
# basic simulation loop

  pmon="gdas1.feb09.w4" # set to meteo file before loop starts

  let nday=${freq}-1    # initialize counter to start at day one

  for syr in 09; do
  for mon in 3 4; do
  for day in  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 \
             16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31; do

  let nday=${nday}+1

# create meteorology file name with month and year

  if [ $mon -eq 1 ];then
     monyr="jan${syr}" 
  elif [ $mon -eq 2 ];then
     monyr="feb${syr}" 
  elif [ $mon -eq 3 ];then
     monyr="mar${syr}" 
  elif [ $mon -eq 4 ];then
     monyr="apr${syr}" 
  elif [ $mon -eq 5 ];then
     monyr="may${syr}" 
  elif [ $mon -eq 6 ];then
     monyr="jun${syr}" 
  elif [ $mon -eq 7 ];then
     monyr="jul${syr}" 
  elif [ $mon -eq 8 ];then
     monyr="aug${syr}" 
  elif [ $mon -eq 9 ];then
     monyr="sep${syr}" 
  elif [ $mon -eq 10 ];then
     monyr="oct${syr}" 
  elif [ $mon -eq 11 ];then
     monyr="nov${syr}" 
  else
     monyr="dec${syr}" 
  fi
  smo=`printf %2.2d $mon`

# determine the correct meteorology week 

  if [ $day -le 7 ];then
     met1=${pmon}
     met2="gdas1.${monyr}.w1"
  elif [ $day -le 14 ]; then
     met1="gdas1.${monyr}.w1"
     met2="gdas1.${monyr}.w2"
  elif [ $day -le 21 ];then
     met1="gdas1.${monyr}.w2"
     met2="gdas1.${monyr}.w3"
  elif [ $day -le 28 ];then
     met1="gdas1.${monyr}.w3"
     met2="gdas1.${monyr}.w4"
  else
     met1="gdas1.${monyr}.w4"
     met2="gdas1.${monyr}.w5"
  fi
  sda=`printf %2.2d $day`
  
  date=${syr}${smo}${sda}  # basic string to name output

#---------------------------------------------------------
# Mimic the functionality of MOD to determine if the
# simulation cycle is to be run this particular day
 
  cycle=`echo "$nday / $freq" | bc`
  value=`echo "$cycle * $freq" | bc` 

  if [ $nday -eq $value ];then

#----------------------------------------------------------
# Set up the control file for dispersion/concentration 
# simulation by looping through all the starting locations 
# from the pre-configured input file

  cat SAMPLERS.TXT | while read site olat olon; do

  echo "$syr $smo $sda $shr    " >CONTROL
  echo "1                      ">>CONTROL
  echo "$olat $olon $olvl      ">>CONTROL
  echo "-240                   ">>CONTROL
  echo "0                      ">>CONTROL
  echo "10000.0                ">>CONTROL
  echo "2                      ">>CONTROL
  echo "$MET                   ">>CONTROL
  echo "$met1                  ">>CONTROL
  echo "$MET                   ">>CONTROL
  echo "$met2                  ">>CONTROL
  echo "1                      ">>CONTROL
  echo "TEST                   ">>CONTROL
  echo "1.0                    ">>CONTROL
  echo "1.0                    ">>CONTROL
  echo "00 00 00 00 00         ">>CONTROL
  echo "1                      ">>CONTROL
  echo "0.0 0.0                ">>CONTROL
  echo "1.0 1.0                ">>CONTROL
  echo "181.0 360.0            ">>CONTROL
  echo "./                     ">>CONTROL
  echo "S${site}_${date}       ">>CONTROL
  echo "1                      ">>CONTROL
  echo "100                    ">>CONTROL
  echo "00 00 00 00 00         ">>CONTROL
  echo "00 00 00 00 00         ">>CONTROL
  echo "00 24 00               ">>CONTROL
  echo "1                      ">>CONTROL
  echo "0.0 0.0 0.0            ">>CONTROL
  echo "0.0 0.0 0.0 0.0 0.0    ">>CONTROL
  echo "0.0 0.0 0.0            ">>CONTROL
  echo "0.0                    ">>CONTROL
  echo "0.0                    ">>CONTROL

  echo "&SETUP"                >SETUP.CFG
  echo "initd = 4,"           >>SETUP.CFG
  echo "khmax = 9999,"        >>SETUP.CFG
  echo "numpar = 100,"        >>SETUP.CFG
  echo "maxpar = 10000,"      >>SETUP.CFG
  echo "/"                    >>SETUP.CFG

#----------------------------------------------------------
# run the concentration simulation 

  echo "Simulation: S${site}_${date}"
  ${MDL}/hycs_std   


#----------------------------------------------------------
# loop and if test terminations

  done           # number of samplers loop
  fi             # simulation cycle test

  done           # end of day loop
  pmon=${met2}   # save previous month last meteo file

  done           # end of month loop
  done           # end of year loop
