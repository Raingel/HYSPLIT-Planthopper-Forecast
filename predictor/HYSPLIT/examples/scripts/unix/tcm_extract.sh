#!/bin/sh

#---------------------------------------------------------------------------
# HYSPLIT is run with 4 species defined in the CONTROL file
# Hpar - heavy particle (Wet=4E+4/5E-6 Dry=1.0 cm/s)
# Lpar - light particle (Wet=4E+4/5E-6 Dry=0.1 cm/s)
# Dgas - depositing gas (Henry=0.08    Dry=1.0 cm/s)
# Ngas - noble gas

PGM="/usr/local/hysplit4"
RUN="./"

# HYSPLIT is run in 6-hour segments with for a unit source,
# resulting in concentration output files with the following
# name convention:
# CG_{release date}_{sampling date}

#--------------------------------------------------------------------------
# When the run is completed (for the day or simulation period),
# the individual files get appended into a single file for each
# release period using the program "conappend" with outputs a
# single file with the naming convention: 
# TG_{release date}
# The "TG" files are the base files that will be used by the
# web user to create a concentration time series. 

rm -f merglist.txt
rm -f conclist.txt
rm -f cdump_merged 

for mm in  3 4; do
for dd in  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 \
           16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31; do
for hh in 0 6 12 18; do

    if [ \( $mm -eq 3 \) -a \( $dd -lt 11 \) ]; then continue; fi
    if [ \( $mm -eq 4 \) -a \( $dd -gt 20 \) ]; then continue; fi

    MM=`printf %2.2d $mm`
    DD=`printf %2.2d $dd`
    HH=`printf %2.2d $hh`

#   CG files created by HYSPLIT for {release date}_{sampling date}
#   get appended into a single file for each TG_{release date}
    if [ -f ${RUN}/CG_${MM}${DD}${HH}_${MM}${DD}${HH} ];then
#      when the first release_sampling exists process all files for that release
       ls ${RUN}/CG_${MM}${DD}${HH}_?????? >conclist.txt
       ${PGM}/exec/conappend -iconclist.txt -oTG_${MM}${DD}${HH}
    fi

done
done
done

#----------------------------------------------------------------------------
# Each dispersion factor file is now multiplied by the emission rate
# (cfactors.txt) and decay correction with the output written to DG_{MMDDHH}
# Pollutant index mapping:  1    2    3    4
# TG_{MMDDHH} contains:  Hpar Lpar Dgas Ngas
# cfactors.txt contains:
#    YYYY MM DD HH   I-131g   I-131p  Cs-137p	
#    2011  3 12 0  1.00E+13 1.00E+13 2.00E+12	
# USAGE: condecay -{Cnumb:Index:HalfL:Radio}
# for example -{1:3:8.04:Igas} maps I-131g (column=1) with Dgas (Index=3)

# The program condecay -{Cnumb:Index:HalfL:Radio} multiplies
# the unit source factors in each TG file by the emissions
# defined in file "cfactors.txt" according to the release date.
# The mapping is defined on the command line: condecay -1:3:8.04:Igas -2:2:8.04:Ipar -3:2:11000.0:Cpar
# where in this case the first argument (-1:3:8.04:Igas) multiplies
# the first column in the emission file (I-131g gaseous Iodine) by
# the 3rd variable (Dgas) in the dispersion factor file, using a 
# half-life of 8.04 days and assigning the 4-character variable ID
# as Igas. The output from condecay is written to a file named: DG_{release date}

# standard output mapping
  ${PGM}/exec/condecay  +d../archive/ -1:3:8.04:Igas -2:2:8.04:Ipar -3:2:11000.0:Cpar

#----------------------------------------------------------------------------
# The final step is to merge the individual release files into one file
# using the program "conmerge", the resulting output file
#    cdump_merged
# can be used to display the concentration time series at any location
# using the standard program
#    con2stn

# create list of processed files
  ls DG_?????? >merglist.txt 

# The final step is to merge the individual release files into one file
  ${PGM}/exec/conmerge -imerglist.txt -ocdump_merged

