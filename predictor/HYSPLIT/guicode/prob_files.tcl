proc probfiles {} {

#-----------------------------------------------------------------------------
# PROB_FILES.TCL: create concentration probability files from ensemble  
# Last Revised: 02 Sep 2020 - Revised version from disp_prob.tcl
#-----------------------------------------------------------------------------

global html_dir Grid_name Num_disp Timesum Npol Nlev Cmax

if [winfo exists .probfile] {destroy .probfile}
set wr .probfile
toplevel $wr
wm title $wr " Create Ensemble Probability Files "
wm  geometry $wr +100+25

frame $wr.top 
frame $wr.mid0
frame $wr.mid1
frame $wr.mid2
frame $wr.bot

pack $wr.top -side top -pady 5 -padx 1
pack $wr.mid0 -pady 5 
pack $wr.mid1 -pady 5 -padx 1 
pack $wr.mid2 -pady 5
pack $wr.bot -pady 15

#-->description

label $wr.top.lab -fg blue -relief raised -justify left -wraplength 7i \
 -text "Creates probability files from multiple concentration outputs.\
Concentration files require a suffix added to the base name and are\
numbered sequentially from {name}.001. Only one species or pollutant\
can be processed. Multiple time periods can be aggregated if desired.\
Output files include mean, medians, and various probability levels.\
This file creation step is required before creating maps or box plots." 
pack $wr.top.lab

#-->select input binary data file

frame $wr.mid0.lft 
frame $wr.mid0.rgt
pack  $wr.mid0.lft $wr.mid0.rgt -side left -padx 15

label $wr.mid0.lft.lab -text "Input File:"
pack $wr.mid0.lft.lab -side left
set d 0
foreach item $Grid_name {
   radiobutton $wr.mid0.lft.$d -variable Num_disp -text $item -value $d  \
                           -background grey -command "probfiles"
   pack $wr.mid0.lft.$d -side left
   incr d
}

#-->temporal aggregation

label $wr.mid1.lab0 -text "Species Index:"
entry $wr.mid1.ent0 -textvariable Npol -relief sunken -width 2
pack $wr.mid1.lab0 $wr.mid1.ent0 -side left

label $wr.mid1.lab1 -text "  Level Index:"
entry $wr.mid1.ent1 -textvariable Nlev -relief sunken -width 2
pack $wr.mid1.lab1 $wr.mid1.ent1 -side left

label $wr.mid1.lab2 -text "  Aggregation:"
entry $wr.mid1.ent2 -textvariable Timesum -relief sunken -width 2
pack $wr.mid1.lab2 $wr.mid1.ent2 -side left

#-->optional over-ride of maximum concentration

label $wr.mid2.lab -text " Optional maximum for probability to exceed:"
entry $wr.mid2.ent -textvariable Cmax -relief sunken -width 15
pack $wr.mid2.lab $wr.mid2.ent -side left

#-->bottom action buttons

button $wr.bot.dismiss  -bg red -text Quit -width 12 -command "destroy $wr"
button $wr.bot.help -text Help -width 8 \
       -command "load_html [file join $html_dir S321.htm ] "
button $wr.bot.save  -bg green -text "Create Files" -width 24 -command {run_prob_files}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 
set_defaultf
}

#------------------------------------------------------------------------------
# run plotting program

proc run_prob_files {} {

global Grid_name Grid_dir exec_dir Num_disp Timesum Npol Nlev Cmax
global X_dir tcl_platform 

set Grid_dir_n [lindex $Grid_dir $Num_disp]
set Grid_name_n  [lindex $Grid_name $Num_disp]

set arg1 -b  ; append arg1 $Grid_dir_n$Grid_name_n
set arg2 -t  ; append arg2 $Timesum 
set arg3 -p  ; append arg3 $Npol
set arg4 -z  ; append arg4 $Nlev

if { $Cmax == 0 } {
   set arg5 "-:" 
} else {
   set arg5 -c$Cmax
} 

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
   exec $exec_dir/conprob $arg1 $arg5 $arg3 $arg4 $arg2 
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/conprob $arg1 $arg5 $arg3 $arg4 $arg2 
   }
} else {
   exec $exec_dir/conprob.exe $arg1 $arg5 $arg3 $arg4 $arg2 
}

}


#------------------------------------------------------------------------------
# set initial defaults

proc set_defaultf {} {

global Num_disp Timesum Npol Nlev Cmax

if [ info exists Cmax ] { } else {set Cmax ""}

if {$Cmax == ""} {
   set Cmax 0
   set Num_disp 0
   set Timesum 1
   set Npol 1
   set Nlev 1
   }
}
