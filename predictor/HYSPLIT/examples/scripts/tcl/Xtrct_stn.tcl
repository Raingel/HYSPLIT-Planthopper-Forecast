#-----------------------------------------------------
# Xtrct_stn.tcl
# for UNIX system uncomment the next line to start using wish \
# exec wish "$0" "$@"
#-----------------------------------------------------
# Runs a program to extract selected meteorological 
# variables at a specific location from an ARL packed
# meteorological data file. In its default mode the
# xtrct_stn program requires the following inputs:
# 1) meteorological data directory name
# 2) meteorological data file
# 3) number of variables to extract
# 4) one line per variable: name level multiplier
# 5) latitude and longitude of location
# 6) extraction method: 0=neighbor 1=interpolation
# 7) output file name
# 8) starting record number for output file (permits
#    output file to be concatenated
#------------------------------------------------------


# meteorological data directory
  set datdir "."
# directory location of executable (xtrct_stn)   
  set exedir "."

#--------------------------------------------------------
# master loop through data files for processing

set record 1

foreach month {mar09} {
foreach half  {001 002} {

      set input ""
      append input "$datdir/\n"
      append input "edas.${month}.${half}\n" 
      append input "3\n"
      append input "PRSS 01 1.0\n"
      append input "MSLP 01 1.0\n"
      append input "T02M 01 1.0\n"
      append input "44.5 -71.3\n"
      append input "0\n"
      append input "mtwash_${month}_${half}.txt\n"
      append input "${record}\n"

      exec "$exedir/xtrct_stn" << $input
      set record [expr $record + 128]
}
}

destroy .
