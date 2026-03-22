proc traj_ioapi {} {

#-----------------------------------------------------------------------------
# TRAJ_IOAPI.TCL: convert trajectory format to cmaq-ioapi
# Last Revised: 01 Dec 2005
#               16 Mar 2012
#-----------------------------------------------------------------------------

global html_dir Trajpts_file Trajapi_file 

if [winfo exists .trajapi] {destroy .trajapi}
set wr .trajapi
toplevel $wr
wm title $wr " Convert Trajectory File to CMAQ-IOAPI  "
wm  geometry $wr +25+25

frame $wr.top
frame $wr.mid0
frame $wr.mid1
frame $wr.mid5
frame $wr.bot
pack $wr.top $wr.mid0 $wr.mid1 $wr.mid5 $wr.bot -side top -pady 5 -padx 10

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "Converts the ASCII trajectory endpoints file to the IOAPI\
ID_referenced data format. Multiple trajectories and meteorological\
data values along the trajectory are supported."
pack $wr.top.lab

#-->input file
label $wr.mid0.lab -text "Input Endpoints:"
entry $wr.mid0.ent -textvariable Trajpts_file -relief sunken -width 15
button $wr.mid0.win  -text Browse -width 8 -command {
   set temp [tk_getOpenFile -title "File Selection"]
   if {[string length $temp] > 0} {set Trajpts_file $temp}}
pack $wr.mid0.lab $wr.mid0.ent $wr.mid0.win -side left -padx 5

#-->output file
label $wr.mid1.lab -text "Output CMAQ-IOAPI: "
entry $wr.mid1.ent -textvariable Trajapi_file -relief sunken -width 25
pack $wr.mid1.lab $wr.mid1.ent -side left -padx 5

#-->termination
label $wr.mid5.lab \
-text "_____________________________________________________________"
pack $wr.mid5.lab -side left

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 10 -command "destroy $wr"
button $wr.bot.help -text Help -width 8 \
       -command "load_html [file join $html_dir S243.htm ] "
button $wr.bot.save  -bg green -text "Execute Conversion" -width 20 -command {run_traj_api}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 
set_traj_api
}

################################################################
#-->run conversion program

proc run_traj_api {} {

global X_dir exec_dir Trajpts_file tcl_platform Trajapi_file 

if [file exists ${Trajapi_file}] {file delete ${Trajapi_file}}

set arg1 $Trajpts_file
set arg2 $Trajapi_file

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
   exec $exec_dir/traj2api $arg1 $arg2
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/traj2api $arg1 $arg2
   }
} else {
   msg_box  "IOAPI conversion - UNIX only!"
}
destroy .trajapi
}

################################################################
#-->set defaults

proc set_traj_api {} {

global html_dir Trajpts_file Trajapi_file 

if { $Trajpts_file == "" } {set Trajpts_file tdump}
if { $Trajapi_file == "" } {set Trajapi_file ${Trajpts_file}_api}
}
