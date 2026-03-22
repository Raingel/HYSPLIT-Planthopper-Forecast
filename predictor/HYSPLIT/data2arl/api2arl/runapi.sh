#!/bin/sh

DIR=/data2/scratch
MET=jma_ma_met_hybrid-coordinate

  rm -f DATA.ARL

# rm -f GFS.BIN
# rm -f arldata.cfg api2arl.cfg

# create outfile from last cycle for a time
# that corresponds with the next intialization
# t06z(f06) = t12z(f00)
# ./api2arl -z1 -i${DIR}/gfs.t06z.pgrb2f06.1p0deg 
# ./api2arl -z0 -i${DIR}/gfs.t12z.pgrb2f00.1p0deg 
# mv DATA.ARL GFS.BIN

YY="2011"
MM="03"

# append the remaining forecast times
for DD in 13 14 15 16 17 18; do
for HH in 00 03 06 09 12 15 18 21; do

#   ./api2arl -z1 -i${DIR}/gfs.t12z.pgrb2f${HH}.1p0deg
#   cat DATA.ARL >>GFS.BIN

    ./api2arl -a2 -t41 -i${DIR}/${MET}_${YY}${MM}${DD}${HH}00.grib2.bin
    cat DATA.ARL >>JMA${YY}${MM}${DD}.bin

    rm -f DATA.ARL
done
done
