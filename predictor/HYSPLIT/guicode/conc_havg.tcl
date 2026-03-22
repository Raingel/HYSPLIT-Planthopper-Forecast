proc conhavg {} {
#------------------------------------------------------------------------------
# CONC_HAVG.TCL: horizontal averaging of a concentration file
# Last Revised: 16 Apr 2013 - initial version
#------------------------------------------------------------------------------

global Grid_dir Grid_name Num_disp
global havrg_file havrg_scan
global html_dir 

if [winfo exists .conhavg] {destroy .conhavg}
set wr .conhavg
toplevel $wr
wm title $wr " Horizontal Concentration Grid Smoothing "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.mid0
frame $wr.mid2
frame $wr.mid5
frame $wr.div1
frame $wr.bot

pack $wr.top -padx 2 -pady 4
pack $wr.mid0 $wr.mid2 $wr.mid5 $wr.div1 $wr.bot -pady 6 -padx 4

#-->information

label $wr.top.lab -fg blue -relief raised -justify left -wraplength 5i\
 -text "Replaces the value in each concentration grid cell by the average\
of a specified number of grid cells surrounding that point. The number to\
include is specified by the scan radius in grid point units."
pack $wr.top.lab

#-->select binary hysplit input file and conversion factor

label $wr.mid0.lab1 -text "Input File: "
pack $wr.mid0.lab1 -side left
set d 0
foreach item $Grid_name {
   radiobutton $wr.mid0.$d -variable Num_disp -text $item -value $d -background grey
   pack $wr.mid0.$d -side left -padx 4
   incr d
}

label $wr.mid2.lab1 -text "Output File: "
entry $wr.mid2.ent1 -textvariable havrg_file -width 12
pack  $wr.mid2.lab1 $wr.mid2.ent1 -side left

label $wr.mid5.lab1 -text "Scan radius: "
entry $wr.mid5.ent1 -textvariable havrg_scan -width 12
pack  $wr.mid5.lab1 $wr.mid5.ent1 -side left

#-->termination

label $wr.div1.lab \
-text "_____________________________________________________________"
pack $wr.div1.lab -side left

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 12 -command "destroy $wr"
button $wr.bot.help -text Help  -width 12 \
       -command "load_html [file join $html_dir S348.htm ] "
button $wr.bot.save  -bg green -text "Create File" -width 20 -command {run_havrg}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 8 
havrg_init
}


#---------------------------------------------------------------------
proc run_havrg {} {

global havrg_file havrg_scan
global Grid_dir Grid_name Num_disp
global X_dir tcl_dir exec_dir tcl_platform

set Grid_dir_n  [lindex $Grid_dir $Num_disp]
set Grid_name_n [lindex $Grid_name $Num_disp]

set arg1 -i  ; append arg1 $Grid_dir_n$Grid_name_n
set arg2 -o  ; append arg2 $havrg_file
set arg3 -s  ; append arg3 $havrg_scan

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
   exec $exec_dir/conhavrg $arg1 $arg2 $arg3
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/conhavrg $arg1 $arg2 $arg3
   }
} else {
   exec $exec_dir/conhavrg.exe $arg1 $arg2 $arg3
}
destroy .conhavg

}


#-----------------------------------------------------------

proc havrg_init {} {

global havrg_file havrg_scan Num_disp

if [ info exists havrg_file ] { } else {
   set havrg_file ""
}

if { $havrg_file == "" } {
   set havrg_file cavrg
   set havrg_scan 1
   set Num_disp 0
}
}
