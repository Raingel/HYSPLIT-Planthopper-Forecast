  PGM="/home/hysplit/exec"
  dir="/pub"

  rm -f WRFDATA.ARL
  rm -f ARWDATA.CFG
  rm -f ARLDATA.CFG

  for dd in 21 22; do
  for hh in 00 01 02 03 04 05 06 07 08 09 10 11 12 \
            13 14 15 16 17 18 19 20 21 22 23; do
 
      if [ -f ${dir}/wrfout_d03_2010-10-${dd}_${hh}:00:00 ];then
         rm -f ARLDATA.BIN
         ${PGM}/arw2arl -i${dir}/wrfout_d03_2010-10-${dd}_${hh}:00:00 
         cat ARLDATA.BIN >> WRFDATA.ARL 
      fi

  done 
  done
  rm -f ARLDATA.BIN
