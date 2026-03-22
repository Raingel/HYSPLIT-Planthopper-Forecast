#-----------------------------------------------------------------------------
# Autoexec.tcl
# for UNIX system uncomment the next line to start using wish \
# exec wish "$0" "$@"
#-----------------------------------------------------------------------------
# AUTOEXEC.TCL: automatically run HYSPLIT from scheduler
# with the file name created automatically based upon the current date
# Last Revised: 14 Aug 2009
#-----------------------------------------------------------------------------
 
## hycs_std flavor
## ===================
## Create a new "set.cfg" file
## with flags for model execution options.
## "hycs_std.exe" looks for "setup.cfg" in the
## active directory. "hycs_std" will use
## NOAA defaults if this file does not exist.
## 
## If there's a file called "setup.cfg.bak" in
## the active directory, it will be deleted
## when you run this TCL script there.
## 

file delete setup.bak
if [file exists "setup.cfg"] {file copy setup.cfg setup.bak}
file delete setup.cfg
set f [open setup.cfg w]
 puts $f "&SETUP"
 puts $f "initd = 0,"
 puts $f "khmax = 24,"
 puts $f "numpar = 3500,"
 puts $f "qcycle = 0.0,"
 puts $f "isot = 1,"
 puts $f "ndump = 0,"
 puts $f "ncycl = 0,"
 puts $f "mgmin = 10,"
 puts $f "kmsl = 0,"
 puts $f "maxpar = 3500,"
 puts $f "cpack = 1,"
 puts $f "/"
close $f


## Custom Labels
## =============
## Create a new "labels.cfg" file.
## "concplot.exe" looks for "labels.cfg" in
## the active directory. Concplot.exe will 
## use NOAA default labeling if this file 
## does not exist. Units of femtograms are 
## based on invoking "concplot.exe" with the
## -x1e15 switch to convert from [grams/m3].
## 
## If there's a file called "labels.bak" in
## the active directory, it will be deleted
## when you run this TCL script there
##

file delete labels.bak
if [file exists labels.cfg] {file copy labels.cfg labels.bak}
file delete labels.cfg
set f [open labels.cfg w]
 puts $f "'TITLE&','NOAA Air Resources Lab&'"
 puts $f "'LAYER&',' LAYER AVERAGE BETWEEN&'"
 puts $f "'UNITS&','Bq&'"
 puts $f "'/m3 (per unit release)&'"
close $f


# hycs_std Loop
# =================
# This loop dynamically recreates the "hycs_std.exe"
# input control file for repeated execution with
# automatic post-processing of the output.
# Within each iteration of the loop:
# - a new "Control" file is created,
# - "hycs_std" is executed,
#   -- a new output file is generated
#      (e.g., $Output_base$Output_numb = cdump1),
# - "concplot.exe" creates graphical output in postscript format
#   -- (e.g., site$Output_numb.ps = site1.ps)
# 
# If there's a file called "Control" in
# the active directory, it will be deleted
# when you run this TCL script there 

set date [clock format [clock scan now] -format "%y %m %d %H" -gmt 1]
set YRS [lindex [split $date] 0]
set MON [lindex [split $date] 1]
set DAY [lindex [split $date] 2]
set HRS [lindex [split $date] 3]

set Start_lat "0.0"
set Start_lon "0.0"
set Start_hgt "1.0"
set Prog_path "exec"
set Start_time "$YRS $MON $DAY $HRS"
set Run_hours "3"
set Vert_coord "0"
set Top_model "5000.0"
set Meteo_path "c:/hysplit4/working/"
set Meteo_file "ETA$DAY"
set Emit_rate "10.0"
set Emit_hours "0.1"
set Output_path "./"
set Output_base "cdump"

#########################

set Output_numb 1
foreach {Start_lat Start_lon} {39.0 -77.0} {
   set Start_loc "$Start_lat $Start_lon $Start_hgt"
   set Output_file "$Output_base$Output_numb"

   file delete Control
   set f [open Control w]
   puts $f "$Start_time"
   puts $f "1"
   puts $f "$Start_loc"
   puts $f "$Run_hours"
   puts $f "$Vert_coord"
   puts $f "$Top_model"
   puts $f "1"
   puts $f "$Meteo_path"
   puts $f "$Meteo_file"
######################### Emissions
   puts $f "1"
   puts $f "TEST"
   puts $f "$Emit_rate"
   puts $f "$Emit_hours"
   puts $f "00 00 00 00 00"
######################### Concentration Grid
   puts $f "1.0"
   puts $f "0.0  0.0"
   puts $f "0.01 0.01"
   puts $f "5.0  5.0"
#########################
   puts $f "$Output_path"
   puts $f "$Output_file"
######################### Output levels
   puts $f "1"
   puts $f "100"
   puts $f "00 00 00 00 00"
   puts $f "00 00 00 00 00"
   puts $f "00 01 00"
######################### Deposition Section
   puts $f "1"
   puts $f "0.0 0.0 0.0"
   puts $f "0.0 0.0 0.0 0.0 0.0"
   puts $f "0.0 0.0 0.0"
   puts $f "0.0"
   puts $f "0.0"
   close $f

   exec "$Prog_path/hycs_std"

   exec "$Prog_path/concplot" -i$Output_base$Output_numb \
        -osite$Output_numb.ps -z90 -jdc_map -k2 -g4:20

   file delete Control$Output_numb
   file copy Control Control$Output_numb

   file delete Default_conc
   file copy Control Default_conc

   file delete Message$Output_numb
   file copy Message Message$Output_numb
   incr Output_numb
}
destroy .
