proc edit_fmdv { } {

#-----------------------------------------------------------------------------
# EDIT_FMDV.TCL: edits the fm_param.txt file for the foot & mouth disease virus
# Last Revised: 09 April 2015 - initial version
#-----------------------------------------------------------------------------

global html_dir
global global FMD_half FMD_temp FMD_relh

if [winfo exists .dispfmd] {destroy .dispfmd}
set wr .dispfmd
toplevel $wr
wm title $wr "Map Border Labels Menu"
wm  geometry $wr +50+50

frame $wr.top  
frame $wr.lab1
frame $wr.lab2
frame $wr.lab3
frame $wr.exit

pack $wr.top -side top -pady 4
pack $wr.lab1 $wr.lab2 $wr.lab3 -side top 
pack $wr.exit -side top -pady 12

#-->description

label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "Modifies the fm_param.txt file for FMDV calculations"
pack $wr.top.lab

#-->label entry menus

label $wr.lab1.txt -text "Virus half-life (min):         "
entry $wr.lab1.ent -textvariable FMD_half -relief sunken -width 8
pack  $wr.lab1.txt $wr.lab1.ent -side left

label $wr.lab2.txt -text "Critical temperature (C):      "
entry $wr.lab2.ent -textvariable FMD_temp -relief sunken -width 8
pack  $wr.lab2.txt $wr.lab2.ent -side left

label $wr.lab3.txt -text "Critical relative humidity (%) "
entry $wr.lab3.ent -textvariable FMD_relh -relief sunken -width 8
pack  $wr.lab3.txt $wr.lab3.ent -side left

#-->termination

button $wr.exit.dismiss  -bg red -text "Quit" -width 12 -command "destroy $wr"
button $wr.exit.reset  -text "Reset" -width 6 -command "clear_labcfg"
button $wr.exit.help -text "Help" -width 6 \
       -command "load_html [file join $html_dir S419.htm ] "
button $wr.exit.cont  -bg green -text "Save" -width 12 -command "save_labcfg fm_param.txt $wr"
pack  $wr.exit.dismiss $wr.exit.reset $wr.exit.help $wr.exit.cont \
      -side left -padx 4

if [file exists fm_param.txt] {
   load_labcfg fm_param.txt .dummy
   } else {
   reset_labcfg
   }
}

#--------------------------------------------------------------------------
proc clear_labcfg {} {
if [file exists fm_param.txt] {file delete -force fm_param.txt}
reset_labcfg
}

#--------------------------------------------------------------------------
proc reset_labcfg {} {

global FMD_half FMD_temp FMD_relh
set FMD_half "120.0"
set FMD_temp "24.0"
set FMD_relh "60.0"
if [file exists fm_param.txt] {file delete -force fm_param.txt}
}


#--------------------------------------------------------------------------
proc load_labcfg {Fname Wname} {

global global FMD_half FMD_temp FMD_relh
if [file exists $Fname] {
   set f [open "$Fname" r]
   gets $f cline
   set FMD_half [lindex $cline 0]
   set FMD_temp [lindex $cline 1]
   set FMD_relh [lindex $cline 2]
}
close $f
if [winfo exists $Wname] {destroy $Wname}
}


#------------------------------------------------------------------------------
proc save_labcfg {Fname Wname} {

global global FMD_half FMD_temp FMD_relh

set f [open "$Fname" w]
puts $f "$FMD_half $FMD_temp $FMD_relh"
close $f
destroy $Wname
}

