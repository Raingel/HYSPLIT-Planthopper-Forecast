proc pole_disp {} {

#------------------------------------------------------------------------------
# pole_DISP.TCL: color fill plot of polar coordinate concentration grid
# Last Revised: 25 Nov 2014 - initial version 
#               17 Jul 2017 - added message box for ESRI conversion
#               28 Jul 2017 - use Pkont for initialization test
#               26 Jul 2021 - use svg instead of postscript
#------------------------------------------------------------------------------

global Psout_file Pkont Plats Igis
global html_dir poleconc
global Grid_dir Grid_name Num_disp
if [winfo exists .poledisp] {destroy .poledisp}
set wr .poledisp
toplevel $wr
wm title $wr " Display Polar Coordinate Concentration Grid "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.mid1
frame $wr.mid2
frame $wr.mid3
frame $wr.mid5
frame $wr.mid6
frame $wr.bot
pack $wr.top $wr.mid1 $wr.mid2 $wr.mid3 $wr.mid5 $wr.mid6 \
     $wr.bot -side top -pady 5 -padx 2

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "Concentration graphic that color fills each cell of the polar\
concentration grid with one of 10 colors. Requires CPACK=3."
pack $wr.top.lab

#-->select binary hysplit input file and conversion factor

label $wr.mid1.lab -text "Input File: "
pack $wr.mid1.lab -side left
set d 0
foreach item $Grid_name {
   radiobutton $wr.mid1.$d -variable Num_disp -text $item -value $d -background grey
   pack $wr.mid1.$d -side left -padx 4
   incr d
}

#-->graphics options
label $wr.mid2.lab -text "Graphics:  "
radiobutton $wr.mid2.d0 -text "Color Fill  " -variable Pkont -value "0"
radiobutton $wr.mid2.d1 -text "Contour Lines  " -variable Pkont -value "1"
radiobutton $wr.mid2.d3 -text "Fill+Lines" -variable Pkont -value "2"
pack $wr.mid2.lab $wr.mid2.d0 $wr.mid2.d1 $wr.mid2.d3 -side left

#-->GIS options
label $wr.mid3.lab -text "GIS output:  "
radiobutton $wr.mid3.d0 -text "None" -variable Igis -value "0"
radiobutton $wr.mid3.d1 -text "ESRI" -variable Igis -value "1"
pack $wr.mid3.lab $wr.mid3.d0 $wr.mid3.d1 -side left

#-->output file
label $wr.mid5.txt -text "Map (deg): "
entry $wr.mid5.inp -textvariable Plats -relief sunken -width 5
label $wr.mid5.lab -text "    Output File: "
entry $wr.mid5.ent -textvariable Psout_file -relief sunken -width 15
pack  $wr.mid5.txt $wr.mid5.inp $wr.mid5.lab $wr.mid5.ent -side left -padx 2

#-->termination
label $wr.mid6.lab \
-text "________________________________________________________________"
pack $wr.mid6.lab -side left

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help -width 8 \
       -command "load_html [file join $html_dir S331.htm ] "
button $wr.bot.save  -bg green -text "Execute Display" -width 20 -command {run_pgrid}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10
poledisp_set
}

proc poledisp_set {} {
global Num_disp Psout_file Pkont Plats Igis

if [ info exists Pkont ] { } else {set Pkont ""}
if {$Pkont == ""} {
   set Psout_file poleplot
   set Pkont 0
   set Plats 0.50
   set Igis 0
}
set Num_disp 0
}


#------------------------------------------------------------
# run plotting program

proc run_pgrid {} {

global Psout_file Pkont Plats Igis Plats
global X_dir tcl_platform exec_dir
global Grid_dir Grid_name Num_disp

set Grid_dir_n  [lindex $Grid_dir $Num_disp]
set Grid_name_n [lindex $Grid_name $Num_disp]

set arg1 -b  ; append arg1 ..\graphics\arlmap
set arg2 -c  ; append arg2 $Grid_dir_n$Grid_name_n
set arg3 -l  ; append arg3 $Plats
set arg4 -g  ; append arg4 $Pkont
set arg5 -o  ; append arg5 ${Psout_file}.html
set arg6 -v  ; append arg6 $Igis
set arg_ext "+g1"

if [file exists ${Psout_file}.html] {file delete \
   -force ${Psout_file}.html}

if { "$tcl_platform(platform)" == "unix" } {

   if { "$X_dir" == "" } {
   exec $exec_dir/poleplot $arg_ext $arg1 $arg2 $arg3 $arg4 $arg5 $arg6
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/poleplot $arg_ext $arg1 $arg2 $arg3 $arg4 $arg5 $arg6
   }
} else {
   exec $exec_dir/poleplot.exe $arg_ext $arg1 $arg2 $arg3 $arg4 $arg5 $arg6
}

# -->ESRI Shapefiles
if { $Igis == 1 } {
   foreach f [glob -nocomplain GIS_??.txt] {
      msg_box "Created file: $f"
      tkwait window .msg_win
   }
}
svg_box ${Psout_file}.html
}
