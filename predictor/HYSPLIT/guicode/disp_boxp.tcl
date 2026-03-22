proc boxp_disp {} {

#-----------------------------------------------------------------------------
# DISP_BOXP.TCL: box plot display for ensemble members 
# Last Revised: 07 Dec 2007 - initial version
#               18 Aug 2008
#               11 Dec 2008
#               28 Jul 2017 - intialize with boxp_init
#               03 Sep 2020 - edit description text
#               23 Jul 2021 - use svg instead of postscript
#-----------------------------------------------------------------------------

global Plat Plon 
global html_dir

if [winfo exists .boxdisp] {destroy .boxdisp}
set wr .boxdisp
toplevel $wr
wm title $wr " Ensemble Box Plot Probability Display  "
wm  geometry $wr +100+75

frame $wr.top  
frame $wr.mid5
frame $wr.bot

pack $wr.top -side top -pady 4
pack $wr.mid5  
pack $wr.bot -pady 10

label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "Creates a concentration probability box plot for a selected\
 location. Requires the previous creation of the probability files\
 from the display ensemble create files menu. Up to 12 time periods\
 can be shown."
pack $wr.top.lab -padx 5

label $wr.mid5.lab1 -text "Latitude:"
entry $wr.mid5.ent1 -textvariable Plat -relief sunken -width 10
label $wr.mid5.lab2 -text "    Longitude:"
entry $wr.mid5.ent2 -textvariable Plon -relief sunken -width 10
pack $wr.mid5.lab1 $wr.mid5.ent1 $wr.mid5.lab2 $wr.mid5.ent2 -side left 

button $wr.bot.dismiss  -bg red -text Quit -width 12 -command "destroy $wr"
button $wr.bot.help -text Help -width 8 \
       -command "load_html [file join $html_dir S333.htm ] "
button $wr.bot.save  -bg green -text "Execute Display " -width 24 -command {run_box_plot}
pack $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 
set_init_box
}

#------------------------------------------------------------------------------
# run plotting program

proc run_box_plot {} {

global Plat Plon
global tcl_platform exec_dir X_dir Grid_dir Grid_name

if [file exists boxplots.html] [file delete boxplots.html] 
if [file exists ensplots.html] [file delete ensplots.html] 

set arg0 -b  ; append arg0 [lindex $Grid_dir 0][lindex $Grid_name 0]
set arg1 -y  ; append arg1 $Plat
set arg2 -x  ; append arg2 $Plon

set arg_ext "+g1"

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
   exec $exec_dir/boxplots $arg_ext $arg1 $arg2
   exec $exec_dir/ensplots $arg_ext $arg0 $arg1 $arg2
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/boxplots $arg_ext $arg1 $arg2 
   exec $X_dir/xterm -fn fixed -e $exec_dir/ensplots $arg_ext $arg0 $arg1 $arg2 
   }
} else {
   exec $exec_dir/boxplots.exe $arg_ext $arg1 $arg2 
   exec $exec_dir/ensplots.exe $arg_ext $arg0 $arg1 $arg2 
}

if [file exists boxplots.html] {
   if [file exists ensplots.html] {svg_box ensplots.html}
   svg_box boxplots.html
} else {
   msg_box "Box plot not created ... try a different location" 
}
}


#------------------------------------------------------------------------------
# set initial defaults

proc set_init_box {} {
global Plat Plon boxp_init

if [ info exists boxp_init ] { } else {set boxp_init ""}

if {$boxp_init == ""} {
   set boxp_init "yes"
   if { $Plat == "" } {set Plat 0.0}
   if { $Plon == "" } {set Plon 0.0}
   }
}

