proc disp_lab { } {

#-----------------------------------------------------------------------------
# DISP_LAB.TCL: creates LABELS.CFG file 
# Last Revised: 24 Sep 2009 - initial version
#-----------------------------------------------------------------------------

global html_dir
global title mapid layer units volum releas

if [winfo exists .displab] {destroy .displab}
set wr .displab
toplevel $wr
wm title $wr "Map Border Labels Menu"
wm  geometry $wr +50+50

frame $wr.top  
frame $wr.lab1
frame $wr.lab2
frame $wr.lab3
frame $wr.lab4
frame $wr.lab5
frame $wr.lab6
frame $wr.exit

pack $wr.top -side top -pady 4
pack $wr.lab1 $wr.lab2 $wr.lab3 $wr.lab4 $wr.lab5 $wr.lab6 -side top 
pack $wr.exit -side top -pady 12

#-->description

label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "Creates the LABELS.CFG file to modify plot border labels"
pack $wr.top.lab

#-->label entry menus

label $wr.lab1.txt -text "Title Line: "
entry $wr.lab1.ent -textvariable title -relief sunken -width 40
pack  $wr.lab1.txt $wr.lab1.ent -side left

label $wr.lab2.txt -text "  Map Type: "
entry $wr.lab2.ent -textvariable mapid -relief sunken -width 40
pack  $wr.lab2.txt $wr.lab2.ent -side left

label $wr.lab3.txt -text "     Layer: "
entry $wr.lab3.ent -textvariable layer -relief sunken -width 40
pack  $wr.lab3.txt $wr.lab3.ent -side left

label $wr.lab4.txt -text "Mass Units: "
entry $wr.lab4.ent -textvariable units -relief sunken -width 40
pack  $wr.lab4.txt $wr.lab4.ent -side left

label $wr.lab5.txt -text "    Volume: "
entry $wr.lab5.ent -textvariable volum -relief sunken -width 40
pack  $wr.lab5.txt $wr.lab5.ent -side left

label $wr.lab6.txt -text "   Release: "
entry $wr.lab6.ent -textvariable releas -relief sunken -width 40
pack  $wr.lab6.txt $wr.lab6.ent -side left

#-->termination

button $wr.exit.dismiss  -bg red -text "Quit" -width 12 -command "destroy $wr"
button $wr.exit.reset  -text "Reset" -width 6 -command "clear_labcfg"
button $wr.exit.help -text "Help" -width 6 \
       -command "load_html [file join $html_dir S418.htm ] "
button $wr.exit.cont  -bg green -text "Save" -width 12 -command "save_labcfg LABELS.CFG $wr"
pack  $wr.exit.dismiss $wr.exit.reset $wr.exit.help $wr.exit.cont \
      -side left -padx 4

if [file exists LABELS.CFG] {
   load_labcfg LABELS.CFG .dummy
   } else {
   reset_labcfg
   }
}

#--------------------------------------------------------------------------
proc clear_labcfg {} {
if [file exists LABELS.CFG] {file delete -force LABELS.CFG}
reset_labcfg
}

#--------------------------------------------------------------------------
proc reset_labcfg {} {

global title mapid layer units volum releas
set title "NOAA HYSPLIT MODEL"
set mapid "Air Concentration"
set layer "Average"
set units "Mass"
set volum "/m3"
set releas "enterlabel"
if [file exists LABELS.CFG] {file delete -force LABELS.CFG}
}


#--------------------------------------------------------------------------
proc load_labcfg {Fname Wname} {

global title mapid layer units volum releas
if [file exists $Fname] {
   set f [open "$Fname" r]
   while {[gets $f cline] >= 0} {
      set pline [string range $cline 1 5]
      set local [string tolower $pline]
      set value [string range $cline 10 end-2] 
      set $local $value
   }
}
close $f
if [winfo exists $Wname] {destroy $Wname}
}


#------------------------------------------------------------------------------
proc save_labcfg {Fname Wname} {

global title mapid layer units volum releas

set f [open "$Fname" w]

puts $f "'TITLE&','${title}&'"
puts $f "'MAPID&','${mapid}&'"
puts $f "'LAYER&','${layer}&'"
puts $f "'UNITS&','${units}&'"
puts $f "'VOLUM&','${volum}&'"
puts $f "'RELEASE&','${releas}&'"
close $f
destroy $Wname
}

