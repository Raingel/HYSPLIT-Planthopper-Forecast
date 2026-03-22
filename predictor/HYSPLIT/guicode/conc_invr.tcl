proc coninvr {} {
#------------------------------------------------------------------------------
# CONC_INVR.TCL: horizontal averaging of a concentration file
# Last Revised: 02 Sep 2020 - initial version
#------------------------------------------------------------------------------

global Grid_dir Grid_name Num_disp
global cinvr_file 
global html_dir 

if [winfo exists .coninvr] {destroy .coninvr}
set wr .coninvr
toplevel $wr
wm title $wr " Create Concentration Inverse File "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.mid0
frame $wr.mid2
frame $wr.mid5
frame $wr.div1
frame $wr.bot

pack $wr.top -padx 2 -pady 4
pack $wr.mid0 $wr.mid2 $wr.div1 $wr.bot -pady 6 -padx 4

#-->information

label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i\
 -text "Replaces the value in each concentration grid cell by its\
inverse. Used in source attribution applications." 
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
entry $wr.mid2.ent1 -textvariable cinvr_file -width 20
pack  $wr.mid2.lab1 $wr.mid2.ent1 -side left

#-->termination

label $wr.div1.lab \
-text "_____________________________________________________________"
pack $wr.div1.lab -side left

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 12 -command "destroy $wr"
button $wr.bot.help -text Help  -width 12 \
       -command "load_html [file join $html_dir S336.htm ] "
button $wr.bot.save  -bg green -text "Create File" -width 20 -command {run_cinvr}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 8 
cinvr_init
}


#---------------------------------------------------------------------
proc run_cinvr {} {

global cinvr_file 
global Grid_dir Grid_name Num_disp
global X_dir tcl_dir exec_dir tcl_platform

set Grid_dir_n  [lindex $Grid_dir $Num_disp]
set Grid_name_n [lindex $Grid_name $Num_disp]

set arg1 -i  ; append arg1 $Grid_dir_n$Grid_name_n
set arg2 -o  ; append arg2 $cinvr_file

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
   exec $exec_dir/con2inv $arg1 $arg2 
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/con2inv $arg1 $arg2 
   }
} else {
   exec $exec_dir/con2inv.exe $arg1 $arg2 
}
destroy .coninvr

}


#-----------------------------------------------------------

proc cinvr_init {} {

global cinvr_file Num_disp

if [ info exists cinvr_file ] { } else {
   set cinvr_file ""
}

if { $cinvr_file == "" } {
   set cinvr_file concinv.bin
   set Num_disp 0
}
}
