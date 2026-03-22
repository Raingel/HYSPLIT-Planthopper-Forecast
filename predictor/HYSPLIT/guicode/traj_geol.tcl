proc traj_geol { } {

#-----------------------------------------------------------------------------
# TRAJ_GEOL.TCL: Run multiple trajectory simulations by looping through
#                sequentially numbered CONTROL files created from a DATEM
#                formatted measured data file
#-----------------------------------------------------------------------------
# Last Revised: 29 Oct 2007 - initial version from auto_geol.tcl
#               14 Nov 2008 - renamed executables
#               01 Jul 2014 - added zero definition
#               14 Aug 2018 - select trajectory start times and add scroll log
#-----------------------------------------------------------------------------

global html_dir Zero traj1 traj2 traj4
if [winfo exists .trajgeol] {destroy .trajgeol}
set wr .trajgeol
toplevel $wr
wm title $wr " Automated Backward Trajectory Simulations for GeoLocation "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.step1
frame $wr.stepa
frame $wr.stepb
frame $wr.stepc
frame $wr.step2
frame $wr.step3
frame $wr.bot
pack $wr.top $wr.step1 $wr.stepa $wr.stepb $wr.stepc $wr.step2 $wr.step3 \
     $wr.bot -side top -pady 5 -padx 5

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 7i \
 -text "Execute a script to run multiple iterations of the backward \
 trajectory calculation for non-zero measured sampling data. \
 Trajectories are started at the beginning, middle, and end of each\
 sampling period. Output files are labeled according to the station\
 ID and trajectory start time. The results can then be overlaid using\
 the trajectory frequency plot."
pack $wr.top.lab

#----------------------------------------------------------
# Step1 define the measured data file

label $wr.step1.lab -width 50 -bg Gray65 -anchor w \
      -text "Step 1: Define the measured data input file"
button $wr.step1.but  -text Browse -width 8 -command {
   set temp [tk_getOpenFile -title "File Selection"]
   if {[string length $temp] > 0} {set name_datem $temp}}
pack  $wr.step1.lab $wr.step1.but -side left -padx 5

entry $wr.stepa.ent -textvariable name_datem -relief sunken -width 60 
pack  $wr.stepa.ent

label  $wr.stepb.lab -width 12 -text "Zero Value:"
entry  $wr.stepb.ent -textvariable Zero -width 15
pack $wr.stepb.lab $wr.stepb.ent -side left -padx 2


#-----------------------------------------------------------
# StepA define the trajectory start times

label $wr.stepc.lab -text "Trajectory Starts with Respect to Sample"
pack  $wr.stepc.lab 
checkbutton $wr.stepc.d0 -text "Beginning" -variable traj1 -background grey
checkbutton $wr.stepc.d1 -text "Middle   " -variable traj2 -background grey
checkbutton $wr.stepc.d2 -text "End      " -variable traj4 -background grey
pack $wr.stepc.d0 $wr.stepc.d1 $wr.stepc.d2 -side left -padx 2


#-------------------------------------------------
# Step2 Create a control file for each measurement

label $wr.step2.lab -width 50 -bg Gray65 -anchor w \
      -text "Step 2: Create all the CONTROL files"
button $wr.step2.but  -text Execute -width 8 -command {make_input $name_datem}
pack $wr.step2.lab $wr.step2.but -side left -padx 5


#-------------------------------------
# Step3 Run the trajectory simulations

label  $wr.step3.lab -width 50 -bg Gray65 -anchor w \
       -text "Step 3: Run the trajectory simulations"
button $wr.step3.but  -text Execute -width 8 -command {run_tgeo}
pack   $wr.step3.lab $wr.step3.but -side left -padx 5


#----------------------
# bottom action buttons

button $wr.bot.dismiss  -bg red -text Quit -width 24 -command "destroy $wr"
button $wr.bot.help -text Help -width 24 \
       -command "load_html [file join $html_dir S257.htm ] "
label  $wr.bot.blank -text "   " -width 8
pack   $wr.bot.dismiss $wr.bot.help $wr.bot.blank -side left -padx 5

set_defaults
}

#======================================================
proc make_input {fname} {

global X_dir exec_dir tcl_platform Zero traj1 traj2 traj4

file copy -force default_traj CONTROL
if [file exists TRAJ.CFG] {file copy -force TRAJ.CFG SETUP.CFG}

set d1 1
set d2 0
while {$d1 <= 999} {
   set DD [format "%3.3u" $d1]
   if [ file exists CONTROL.${DD} ] {file delete CONTROL.${DD}}
   if [ file exists MESSAGE.${DD} ] {file delete MESSAGE.${DD}}
   incr d1
}

set arg1 -i  ; append arg1 $fname
set arg2 -z  ; append arg2 $Zero
set arg3 -t  ; append arg3 [ expr $traj1 + [expr [expr $traj2*2] + [expr $traj4*4]] ]

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
      exec $exec_dir/dat2cntl $arg1 $arg2 $arg3
   } else {
      exec $X_dir/xterm -fn fixed -e $exec_dir/dat2cntl $arg1 $arg2 $arg3
   }
} else {
  exec $exec_dir/dat2cntl.exe $arg1 $arg2 $arg3
  update
  msg_box " Calculations completed for file: $fname "
}

}

#======================================================
proc run_tgeo {} {

global X_dir exec_dir tcl_platform

set d1 1
set d2 0
while {$d1 <= 999} {
   set DD [format "%3.3u" $d1]
   if [ file exists CONTROL.${DD} ] {set d2 $d1}
   incr d1
}

if {$d2 == 0} {
   msg_box " No input files found between 001 and 999"   
   return
}

set log [ScrollText .f]
$log configure -cursor watch
$log insert end "Geolocation calculations started ...\n"
update  

set d1 1
while {$d1 <= $d2} {
   set DD [format "%3.3u" $d1]
   if [ file exists SETUP.CFG ] {file copy -force SETUP.CFG SETUP.$DD}

   if { "$tcl_platform(platform)" == "unix" } {
      if { "$X_dir" == "" } {
         exec $exec_dir/hyts_std ${DD}
      } else {
         exec $X_dir/xterm -fn fixed -e $exec_dir/hyts_std ${DD}
      }
   } else {
     exec $exec_dir/hyts_std.exe ${DD}
   }

   $log insert end " Finished file: CONTROL.${DD} \n"
   update

   if [ file exists SETUP.$DD ] {file delete SETUP.$DD}
   incr d1
}
$log insert end " All calculations completed! \n"
update
}


#------------------------------------------------------------------------------
# set initial defaults

proc set_defaults {} {
global Zero traj1 traj2 traj4

     set traj1 1
     set traj2 1
     set traj4 1

if [ info exists Zero ] {
     if { "$Zero" == "" } {set Zero 0.0}
} else {
     set Zero 0.0
}

}

