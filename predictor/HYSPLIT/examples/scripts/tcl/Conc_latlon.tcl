#-----------------------------------------------------------------------------
# Autoconc.tcl
# for UNIX system uncomment the next line to start using wish \
# exec wish "$0" "$@"
#-----------------------------------------------------------------------------
# AUTOCONC.TCL: automatically run HYSPLIT for several fixed locations
# with latitude-longitudes defined by a list within this script.
# Put this script in the hysplit/concmdl directory to execute. 
# Last Revised: 14 Aug 2009
#-----------------------------------------------------------------------------
 
## hycs_std flavor
## ===================
## Create a new "set.cfg" file
## with flags for model execution options.
## "hycs_std.exe" looks for "setup.cfg" in the
## active directory.  "hycs_std" will use
## NOAA defaults if this file does not exist.
## 
## If there's a file called "setup.cfg.bak" in
## the active directory, it will be deleted
## when you run this TCL script there.
## 

file delete setup.cfg.bak
if [file exists "setup.cfg"] {
file copy setup.cfg setup.cfg.bak
}
file delete setup.cfg
set f [open setup.cfg w]
 puts $f "&SETUP"
 puts $f "tratio = 0.75,"
 puts $f "initd = 3,"
 puts $f "khmax = 24,"
 puts $f "numpar = 3500,"
 puts $f "qcycle = 0.0,"
 puts $f "isot = 1,"
 puts $f "ndump = 0,"
 puts $f "ncycl = 0,"
 puts $f "mgmin = 10,"
 puts $f "kmsl = 0,"
 puts $f "maxpar = 3500,"
 puts $f "cpack = 0,"
 puts $f "dxf = ,"
 puts $f "dyf = ,"
 puts $f "dzf = ,"
 puts $f "ichem = ,"
 puts $f "/"
close $f


## Custom Labels
## =============
## Create a new "labels.cfg" file.
## "concplot.exe" looks for "labels.cfg" in
## the active directory.
## "concplot.exe" will use NOAA default
## labeling if this file does not exist.
## Units of femtograms are based on invoking
## "concplot.exe" with a -x1e15 switch to 
## convert from [grams/m3 (per gram released)]
## 
## If there's a file called "labels.cfg.bak" in
## the active directory, it will be deleted
## when you run this TCL script there
##

file delete labels.cfg.bak
if [file exists labels.cfg] {
  file copy labels.cfg labels.cfg.bak
  }
file delete labels.cfg
set f [open labels.cfg w]
 puts $f "'TITLE&','My Organization&'"
 puts $f "'LAYER&',' LAYER AVERAGE BETWEEN&'"
 puts $f "'UNITS&','femtograms&'"
 puts $f "'/m3 (per gram released)&'"
close $f


## Virtual Sensors
## ===============
## Create a new "stns.txt" file.
## As far as "con2stn.exe" is concerned, this
## file can have any name, but it needs to match
## the name in the "con2stn.exe" call later
## in this script.  
## Format is: "Site-ID# Lat Lon"
## 
## If there's a file called "stns.txt.bak" in
## the active directory, it will be deleted
## when you run this TCL script there
## 

file delete stns.txt.bak
if [file exists "stns.txt"] {
  file copy stns.txt stns.txt.bak
 }
file delete stns.txt
set f [open stns.txt w]
 puts $f "001 39.0 -89.5"
close $f


# hycs_std Loop
# =================
# This loop dynamically recreates the "hycs_std"
# input control file for repeated execution with
# automatic post-processing of the output.
# Within each iteration of the loop:
# - a new "Control" file is created,
# - "hycs_std.exe" is executed,
#   -- a new output file is generated
#      (e.g., $Output_base$Output_numb = cdump1),
# - "concplot.exe" creates graphical output in postscript format
#   -- (e.g., site$Output_numb.ps = site1.ps)
# - "con2stn.exe" creates, in standard ASCII-formatted
#   table, a time series of concentrations for each
#   station specified in "stns.txt"
#   -- (e.g., site$Output_numb.txt = site1.txt)
# 
# If there's a file called "Control" in
# the active directory, it will be deleted
# when you run this TCL script there 

set Start_lat "0.0"
set Start_lon "0.0"
set Start_hgt "1.0"
set Conc_path "../exec"
set Start_time "00 00 00 00"
set Run_hours "48"
set Vert_coord "0"
set Top_model "15000.0"
set Meteo_path "./"
set Meteo_file "oct1618.BIN"
set Emit_rate "1.0"
set Emit_hours "48.0"
set Output_path "./"
set Output_base "cdump"

#

set Output_numb 1
foreach {Start_lat Start_lon} {35.0 -90.0 37.0 -90.0 40.0 -90.0} {
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
#########################
   puts $f "1"
   puts $f "spec"
   puts $f "$Emit_rate"
   puts $f "$Emit_hours"
   puts $f "00 00 00 00 00"
   puts $f "1.0"
   puts $f "38.0 -91.0"
   puts $f "0.05 0.05"
   puts $f "5.0 5.0"
#########################
   puts $f "$Output_path"
   puts $f "$Output_file"
#########################
   puts $f "1"
   puts $f "10"
   puts $f "00 00 00 00 00"
   puts $f "00 00 00 00 00"
   puts $f "00 12 00"
   puts $f "1"
   puts $f "0.0 0.0 0.0"
   puts $f "0.0 0.0 0.0 0.0 0.0"
   puts $f "0.0 0.0 0.0"
   puts $f "0.0"
   puts $f "0.0"
   close $f

   set g [open Diag$Output_numb w]
   puts $g "Loop Number $Output_numb"
   close $g

   exec "$Conc_path/hycs_std"

   exec "concplot" -c5 -i$Output_base$Output_numb -osite$Output_numb.ps -x1e15 -z90

   exec "con2stn" -i$Output_base$Output_numb -osite$Output_numb.txt -sstns.txt -xi -c1e15

#   exec "timeplot" -isite$Output_numb.txt -999
#   file copy timeplot.ps timeplot$Output_numb.ps

   file copy Control Control$Output_numb
   incr Output_numb
}
