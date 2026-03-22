_______________
SCRIPTS

#Usage: grib2arl [-options]
# -i[grib input file name {required}]
# -s[supplemental grib data file name {optional}]
# -x[subgrid extract center longitude {-80.0}]
# -y[subgrid extract center latitude {60.0}]
# -g[output projection {0}:conformal extract
#                       1 :northern hemisph polar
#                       2 :southern hemisph polar
#                       3 :lat-lon global grid
#                       4 :lat-lon extract grid
# -n[number of (x:y) extract grid points {100}]
# -k[number of output levels incl sfc {16}]
# -p[output {1}:surface pressure or 0:terrain height]
# -z[zero initialization of output 0:no {1}:yes]

  options="-g3 -p0 -k20"

  base="EN0105"
  dir1="/pub/archives/ecmwf"

  file="ECMGBL"
  dir2="/work"
  rm -f ${dir2}/${file} 

  for day  in 30 31; do
  for hour in 00 03 06 09 12 15 18 21; do
      echo "Processing: ${base}${day}${hour}"  

      rm -f CFG_ARL
      rm -f DATA.ARL

      grib2arl -i${dir1}/${base}${day}${hour} $options 
      cat DATA.ARL >>${dir2}/${file} 

#     mv MESSAGE MESSAGE.${day}${hour}
  done 
  done

