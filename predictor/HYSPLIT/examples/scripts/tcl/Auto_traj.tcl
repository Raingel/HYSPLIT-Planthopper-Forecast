# Auto_traj.tcl
# for UNIX system uncomment the next line to start using wish \
# exec wish "$0" "$@"

# sample tcl script to demonstrate how multiple 
# trajectory calculations can be performed by 
# dynamically creating the model's input control
# file in a loop, then executing the model, creating
# a different named output file with each execution.
# This script should be modified to vary the parameters
# required for the simulation.

set Start_lat "0.0"
set Start_lon "0.0"

set Start_hgt "10.0"
set Traj_path  "../exec"
set Start_time "00 00 00 00"
set Run_hours  "24"
set Vert_coord "0"
set Top_model  "10000.0"
set Meteo_path "./"
set Meteo_file "oct1618.BIN"
set Output_path "./"
set Output_base "tdump"

set Output_numb 1
foreach {Start_lat Start_lon} {35.0 -90.0  40.0 -90.0 45.0 -90.0} {
 
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
   puts $f "$Output_path"
   puts $f "$Output_file"
   close $f

   exec "$Traj_path/hyts_std"
#-----------------------------------------
# add additional post-processing exe here
# no quotes required for the arguments
# exec "program.exe" arguments
#-----------------------------------------
   incr Output_numb
}
