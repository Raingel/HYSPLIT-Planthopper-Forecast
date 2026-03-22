proc conc_ioapi {} {

#------------------------------------------------------------------------------
# CONC_IOAPI.TCL: convert binary concentration file to CMAQ IOAPI format
# Last Revised: 02 Dec 2005
#------------------------------------------------------------------------------

global html_dir Grid_name Num_disp Concapi_file
 
if [winfo exists .concapi] {destroy .concapi}
set wr .concapi
toplevel $wr
wm title $wr " Convert Binary Concentration file to CMAQ-IOAPI "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.mid1
frame $wr.mid3
frame $wr.mid5
frame $wr.bot
pack $wr.top $wr.mid1 $wr.mid3 $wr.mid5 $wr.bot -side top -pady 5 -padx 5

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 7i -text \
"Converts the binary concentration output file to the IOAPI GRIDDED\
data format. Multiple species, levels, and time periods are supported."
pack $wr.top.lab

#-->select grid
label $wr.mid1.lab -text "Grid To convert:"
pack $wr.mid1.lab -side left -padx 5
set d 0
foreach item $Grid_name {
   radiobutton $wr.mid1.$d -variable Num_disp -text $item -value $d -background grey
   pack $wr.mid1.$d -side left -padx 2
   incr d
}

#-->output file
label $wr.mid3.lab -text "Output CMAQ-IOAPI: "
entry $wr.mid3.ent -textvariable Concapi_file -relief sunken -width 15
pack $wr.mid3.lab $wr.mid3.ent -side left -padx 5

#-->termination
label $wr.mid5.lab \
-text "_____________________________________________________________"
pack $wr.mid5.lab -side left

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help -width 8 \
       -command "load_html [file join $html_dir S343.htm ] "

button $wr.bot.save  -bg green -text "Execute Conversion" -width 20 -command {run_conc_api}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 

set_conc_api
}

#--------------------------------------------------------------------------
proc run_conc_api {} {

global X_dir Grid_dir Grid_name exec_dir
global Num_disp tcl_platform Concapi_file

set Grid_dir_n [lindex $Grid_dir $Num_disp]
set Grid_name_n  [lindex $Grid_name $Num_disp]

set arg1 $Grid_dir_n$Grid_name_n
set arg2 $Concapi_file          

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
   exec $exec_dir/conc2api $arg1 $arg2
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/conc2api $arg1 $arg2
   }
} else {
   msg_box  "IOAPI conversion - UNIX only!"   
}
destroy .concapi
}

#--------------------------------------------------------------------------
proc set_conc_api {} {

global Num_disp Grid_name Concapi_file
if [ info exists Concapi_file ] { } else {set Concapi_file ""}
if { $Concapi_file == "" } {set Concapi_file [lindex $Grid_name $Num_disp]_api}
}
