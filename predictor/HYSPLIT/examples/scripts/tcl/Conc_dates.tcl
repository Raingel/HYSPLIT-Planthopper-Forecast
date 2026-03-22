#-----------------------------------------------------------------------------
# Conc_dates.tcl
# for UNIX system uncomment the next line to start using wish \
# exec wish "$0" "$@"
#-----------------------------------------------------------------------------
# CONC_DATES.TCL: automatically run HYSPLIT for one location for looping 
# through different starting times by hour, day, month, and year.
# Put this script in any directory to execute, but links to executable
# directories may need to be modified to match the local installation.
# Last Revised: 14 Aug 2009 
#-----------------------------------------------------------------------------

set exec_path "c:/hysplit4/exec" 


## HYSPLIT CONFIGURATION
## =====================
## Create a new "set.cfg" file
## with flags for model execution options.
## "Hymodelc.exe" looks for "setup.cfg" in the
## active directory.  "hycs_std" will use
## defaults if this file does not exist.

file delete setup.cfg
set f [open setup.cfg w]
puts $f "&SETUP"
puts $f "isot = 1,"
puts $f "initd = 4,"
puts $f "khmax = 9999,"
puts $f "numpar = 500,"
puts $f "maxpar = 3500,"
puts $f "ndump = 0,"
puts $f "ncycl = 0,"
puts $f "/"
close $f


# HYSPLIT CONTROL FILE FIXED VALUES
# =================================
# Although the loop below creates the CONTROL file
# for each execution, certain values remain fixed
# for all simulations. Set them in this section.

set Start_lat "40.0"
set Start_lon "-90.0"
set Start_hgt "10.0"
set Run_hours "120"
set Vert_coord "0"
set Top_model "15000.0"
set Meteo_path "./"
set Emit_rate "1.0"
set Emit_hours "1.0"
set Output_path "./"
set Output_base "cdump_"


# hycs_std.exe Loop
# =================
# This loop dynamically recreates the "hymodelc.exe"
# input CONTROL file for repeated execution with
# automatic post-processing of the output. Within each
# iteration of the loop a new "Control" file is created,
# hymodelc.exe is executed, and a new output file is
# generated (e.g., $Output_base$Output_numb = cdump1).

foreach {YY} {1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005} {
foreach {MM} {1 2 3 4 5 6 7 8 9 10 11 12} {
foreach {DD} {1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31} {

#  initialize this month's date
   set YY1 $YY
   set MM1 $MM

#  initialize next month's date
   set YY2 $YY
   set MM2 $MM

#  compute the name of next months meteorology file
   set MM2 [expr $MM2 + 1]
   if {$MM2 > 12} {set MM2 1; set YY2 [expr $YY2 + 1]}   

#  turn numeric dates into a string for substitution
   if {$MM1<10} {set MM1 0$MM1}
   if {$MM2<10} {set MM2 0$MM2}
   if {$DD<10}  {set DD  0$DD}

   set Meteo_file1 "RP$YY1$MM1.gbl"
   set Meteo_file2 "RP$YY2$MM2.gbl"

   if {$YY1<2000} {set YY1 [expr $YY1 - 1900]} else {set YY1 0[expr $YY1 - 2000]}
   if {$YY2<2000} {set YY2 [expr $YY2 - 1900]} else {set YY2 0[expr $YY2 - 2000]}
   set Output_file "$Output_base$YY1$MM1$DD"

   file delete Control
   set f [open Control w]
   puts $f "$YY1 $MM1 $DD 00"
   puts $f "1"
   puts $f "$Start_lat $Start_lon $Start_hgt"
   puts $f "$Run_hours"
   puts $f "$Vert_coord"
   puts $f "$Top_model"
   puts $f "2"
   puts $f "$Meteo_path"
   puts $f "$Meteo_file1"
   puts $f "$Meteo_path"
   puts $f "$Meteo_file2"
   puts $f "1"
   puts $f "C137"
   puts $f "$Emit_rate"
   puts $f "$Emit_hours"
   puts $f "00 00 00 00 00"
   puts $f "1.0"
   puts $f "0.0  0.0"
   puts $f "0.25 0.25"
   puts $f "90.0 180.0"
   puts $f "$Output_path"
   puts $f "$Output_file"
   puts $f "1"
   puts $f "100"
   puts $f "00 00 00 00 00"
   puts $f "00 00 00 00 00"
   puts $f "00 24 00"
   puts $f "1"
   puts $f "0.0 0.0 0.0"
   puts $f "0.0 0.0 0.0 0.0 0.0"
   puts $f "0.0 0.0 0.0"
   puts $f "0.0"
   puts $f "0.0"
   close $f

   exec "$exec_path/hycs_std"

## file rename Control CNT_$YY1$MM1$DD
## file rename Message MSG_$YY1$MM1$DD
}
}
}
