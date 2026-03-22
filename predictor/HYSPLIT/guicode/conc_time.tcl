proc conc_time {} {

#------------------------------------------------------------------------------
# CONC_TIME.TCL: color fill plot of of plume arrival times
# Last Revised: 04 Nov 2008 - initial version 
#               13 Aug 2009 - browse button for map background file
#               08 Dec 2015 - renamed default output file from chron to toa
#               12 Sep 2016 - fixed browse button for shapefiles.txt
#               07 Nov 2016 - do not permit shapefile copy to itself
#               28 Jul 2017 - use Chronbase for initialization test
#               27 Jul 2021 - use svg instead of postscript
#------------------------------------------------------------------------------

global Chronbase Chrondelt Psout_file Map_file Frame Color Cnumb
global html_dir Grid_name Num_disp 
if [winfo exists .contime] {destroy .contime}
set wr .contime
toplevel $wr
wm title $wr " Plume Arrival Times "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.mid1
frame $wr.mid2
frame $wr.mid3
frame $wr.mid4
frame $wr.mid5
frame $wr.mid6
frame $wr.bot
pack $wr.top $wr.mid1 $wr.mid2 $wr.mid3 $wr.mid4 $wr.mid5 \
     $wr.mid6 $wr.bot -side top -pady 5 -padx 10

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 7i \
 -text "Time of arrival graphic (isochron) that shows the time after\
the start of the simulation that the concentration exceeds the given\
threshold concentration at each concentration grid cell. The default\
contour interval (-1) uses the concentration averaging period. Otherwise\
the time difference field should be set to hours."
pack $wr.top.lab

#-->select grid for input file

label $wr.mid1.lab -text "Grid To Display: "
pack $wr.mid1.lab -side left -padx 5
set d 0
foreach item $Grid_name {
   radiobutton $wr.mid1.$d -variable Num_disp -text $item -value $d -background grey
   pack $wr.mid1.$d -side left -padx 2
   incr d
}

#-->output file

label $wr.mid2.lab -text "Output Postscript File: "
entry $wr.mid2.ent -textvariable Psout_file -relief sunken -width 15
pack  $wr.mid2.lab $wr.mid2.ent -side left -padx 5

#-->map background file

label $wr.mid3.lab -text "Map Background:"
entry $wr.mid3.ent -textvariable Map_file -relief sunken -width 35
button $wr.mid3.win  -text Browse -width 8 -command {
   set temp [tk_getOpenFile -title "File Selection"]
   if {[string length $temp] > 0} {
      set cshp [string last shapefiles $temp]
      if {$cshp > 0} {
         if {[file dirname $temp] != [pwd]} {file copy -force $temp .}
         set Map_file [string range $temp $cshp end]
      } else {
         set Map_file $temp
      }
   }
}
pack $wr.mid3.lab $wr.mid3.ent $wr.mid3.win -side left -padx 5

#-->program options

label $wr.mid4.lab -text "Display Options:"
checkbutton $wr.mid4.box -variable Frame -text "Frames" -background grey
checkbutton $wr.mid4.kol -variable Color -text Color -background grey
pack $wr.mid4.lab $wr.mid4.box $wr.mid4.kol -side left

#-->contouring

label $wr.mid5.lab1 -text "  Threshold value:"
entry $wr.mid5.ent1 -textvariable Chronbase -relief sunken -width 8
label $wr.mid5.lab2 -text "  Time difference:"
entry $wr.mid5.ent2 -textvariable Chrondelt -relief sunken -width 4
label $wr.mid5.lab3 -text "  Number of contours:"
entry $wr.mid5.ent3 -textvariable Cnumb -relief sunken -width 4

pack $wr.mid5.lab1 $wr.mid5.ent1 $wr.mid5.lab2 $wr.mid5.ent2 \
     $wr.mid5.lab3 $wr.mid5.ent3 -side left -padx 5

#-->termination
label $wr.mid6.lab \
-text "_____________________________________________________________"
pack $wr.mid6.lab -side left

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help -width 8 \
       -command "load_html [file join $html_dir S339.htm ] "
button $wr.bot.save  -bg green -text "Execute Display" -width 20 -command {run_cgrid}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10
cgrid_set
}

proc cgrid_set {} {
global Chronbase Chrondelt Psout_file maps_dir Map_file Cnumb Frame Color

if [ info exists Chronbase ] { } else {set Chronbase ""}

if {$Chronbase == ""} {
   set Psout_file toa
   set Map_file ${maps_dir}/arlmap
   set Chronbase 0.0
   set Chrondelt -1
   set Cnumb 10
   set Frame 0
   set Color 1
}
}


#------------------------------------------------------------
# run plotting program

proc run_cgrid {} {

global Chronbase Chrondelt Psout_file Map_file Cnumb Frame Color
global X_dir Grid_dir Grid_name Num_disp tcl_platform exec_dir

set Grid_dir_n [lindex $Grid_dir $Num_disp]
set Grid_name_n  [lindex $Grid_name $Num_disp]

set arg1 -i  ; append arg1 $Grid_dir_n$Grid_name_n
set arg2 -l  ; append arg2 $Chronbase
set arg3 -d  ; append arg3 $Chrondelt
set arg4 -o  ; append arg4 $Psout_file
set arg5 -j  ; append arg5 $Map_file
set arg6 -f  ; append arg6 $Frame
set arg7 -c  ; append arg7 $Color
set arg8 -n  ; append arg8 $Cnumb

set arg_ext "+g1"

if { "$tcl_platform(platform)" == "unix" } {

   if { "$X_dir" == "" } {
   exec $exec_dir/isochron $arg_ext $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/isochron $arg_ext $arg1 $arg2 $arg3 \
                                         $arg4 $arg5 $arg6 $arg7 $arg8
   }
} else {
   exec $exec_dir/isochron.exe $arg_ext $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8
}

if [file exists $Psout_file.html] {
    svg_box $Psout_file.html
 } else {
   msg_box "$Psout_file.html not found!" 
 }
   
}
