proc traj_plot {} {

#-----------------------------------------------------------------------------
# TRAJ_DISP.TCL: trajectory display script
# Last Revised: 11 Mar 2002
#               13 Aug 2002
#               07 Oct 2002
#               17 Jun 2003 - option to view ps
#               20 Nov 2003 - meteo vertical display
#               05 Dec 2003 - fix map center and rings option
#               14 Apr 2004 - changed endpoints variable name
#               21 Sep 2004 - don't destroy widget after display
#               24 Oct 2004 - global postscript file name
#               10 Jan 2005 - revised xterm link
#               27 May 2005 - browse option
#               21 Dec 2005 - added argc source point flag
#               21 Mar 2006 - GIS/Google Earth option
#               30 Oct 2008 - no vertical display
#               13 Aug 2009 - browse button for map background
#               29 Apr 2010 - added balls to zip file for google earth
#               28 Oct 2010 - lines option to GIS Generate format
#               16 Mar 2012 - file deletes
#               12 Sep 2016 - fixed browse for shapefiles.txt
#               14 Sep 2016 - saved configuration default_tplot
#               07 Nov 2016 - do not permit shapefile copy to itself
#               17 Jul 2017 - added message box for ESRI conversion
#               28 Jul 2017 - revised initialization
#               13 Dec 2019 - replace trajplot and trajplot.exe with trajplot.py
#               16 Jan 2020 - add option to use FORTRAN or Python trajplot.
#               07 May 2020 - fix quick start examples.
#               21 Jul 2021 - use svg instead of postscript. compact Python tab.
#               31 May 2023 - correct GIS/KML filenames by replacing _ps with _html.
#                           - copy logocon.gif and noaa_google.gif when needed.
#               07 Nov 2023 - change street maps to OPEN_STREET and OPEN_TOPO.
#-----------------------------------------------------------------------------

global html_dir Trajpts_file zip_pgm
global qpnt Kagl Zoom Color Label Psout_file 
global Map_file Map_proj Igis View
global Ring Map Ring_num Ring_dis Map_lat Map_lon
global PyDebug PySource_time_zone PyStreet_map PyOutput_format
global PyPreferred

if [winfo exists .trajdisp] {destroy .trajdisp}
set wr .trajdisp
toplevel $wr
wm title $wr " Trajectory Display   "
wm geometry $wr +25+25

frame $wr.mid0
frame $wr.mid1
frame $wr.mid2
frame $wr.mid3
frame $wr.setm
frame $wr.mid4
frame $wr.mid5
frame $wr.mid6
frame $wr.mid7
frame $wr.bot

ttk::notebook $wr.lang
frame $wr.lang.py
frame $wr.lang.f
$wr.lang add $wr.lang.f -text "Fortran"
$wr.lang add $wr.lang.py -text "Python"

pack $wr.mid0 $wr.mid1 $wr.mid2 $wr.mid3 $wr.mid7 $wr.setm $wr.mid4 $wr.mid5 \
  $wr.mid6 $wr.lang $wr.bot -side top -pady 5 -padx 10
pack configure $wr.lang -expand true -fill x

#-->input file
label $wr.mid0.lab -text "Input Endpoints:"
entry $wr.mid0.ent -textvariable Trajpts_file -relief sunken -width 15
button $wr.mid0.win  -text Browse -width 8 -command {
   set temp [tk_getOpenFile -title "File Selection"]
   if {[string length $temp] > 0} {set Trajpts_file $temp}}
pack $wr.mid0.lab $wr.mid0.ent $wr.mid0.win -side left -padx 5

#-->output file
label $wr.mid1.lab -text "Output File: "
entry $wr.mid1.ent -textvariable Psout_file -relief sunken -width 15
checkbutton $wr.mid1.psv -variable View -text "View" -background grey
checkbutton $wr.mid1.kol -variable Color -text Color -background grey
pack $wr.mid1.lab $wr.mid1.ent $wr.mid1.psv $wr.mid1.kol -side left -padx 5

#-->map background file
label $wr.mid2.lab -text "Map Background: "
entry $wr.mid2.ent -textvariable Map_file -relief sunken -width 25
button $wr.mid2.win  -text Browse -width 8 -command {
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
pack $wr.mid2.lab $wr.mid2.ent $wr.mid2.win -side left -padx 5


#-->map projection options
label $wr.mid3.lab -text "   Projection:"
radiobutton $wr.mid3.d0 -text "Auto" -variable Map_proj -value "0" 
radiobutton $wr.mid3.d1 -text "Pole" -variable Map_proj -value "1" 
radiobutton $wr.mid3.d2 -text "Lamb" -variable Map_proj -value "2" 
radiobutton $wr.mid3.d3 -text "Merc" -variable Map_proj -value "3" 
radiobutton $wr.mid3.d4 -text "CylE" -variable Map_proj -value "4" 
pack $wr.mid3.lab $wr.mid3.d0 $wr.mid3.d1 $wr.mid3.d2 $wr.mid3.d3 $wr.mid3.d4 -side left

#-->GIS/Google Earth options
label $wr.mid7.lab -text "GIS Out:"
radiobutton $wr.mid7.d0 -text "None"      -variable Igis -value "0"  
radiobutton $wr.mid7.d1 -text "GIS-point" -variable Igis -value "1"
radiobutton $wr.mid7.d2 -text "GIS-lines" -variable Igis -value "5"  
radiobutton $wr.mid7.d3 -text "KML/KMZ"   -variable Igis -value "3"  
pack $wr.mid7.lab $wr.mid7.d0 $wr.mid7.d1 $wr.mid7.d2 $wr.mid7.d3 -side left

#-->optional map location set   
frame $wr.setm.rings
frame $wr.setm.centr
pack $wr.setm.rings $wr.setm.centr -side left -padx 10

label $wr.setm.rings.lab  -text " Rings: Number  Dist(km)"
checkbutton $wr.setm.rings.ent0 -variable Ring -text "Set" -background grey
entry $wr.setm.rings.ent1 -textvariable Ring_num -width 6
entry $wr.setm.rings.ent2 -textvariable Ring_dis -width 6
pack $wr.setm.rings.lab 
pack $wr.setm.rings.ent0 $wr.setm.rings.ent1 $wr.setm.rings.ent2 -side left -padx 5 

label $wr.setm.centr.lab  -text "Center:  Lat    Long"
checkbutton $wr.setm.centr.ent0 -variable Map -text "Set" -background grey
entry $wr.setm.centr.ent1 -textvariable Map_lat -width 6
entry $wr.setm.centr.ent2 -textvariable Map_lon -width 6 
pack $wr.setm.centr.lab 
pack $wr.setm.centr.ent0 $wr.setm.centr.ent1 $wr.setm.centr.ent2 -side left -padx 5

#-->select label interval
label $wr.mid4.lab -text "Label Source     Time Label Interval (UTC):" 
pack $wr.mid4.lab -side top

radiobutton $wr.mid4.on  -text "On"  -variable qpnt -value "1" 
radiobutton $wr.mid4.off -text "Off" -variable qpnt -value "0"
pack $wr.mid4.on $wr.mid4.off -side left 

label $wr.mid4.lab2 -text "  "
pack $wr.mid4.lab2 -side left

set d 0
foreach item [list 0 1 3 6 12 24] {
   radiobutton $wr.mid4.$d -variable Label -text $item -value $item
   pack $wr.mid4.$d -side left
   incr d
   }

#-->select height display options
label $wr.mid5.lab -text "Vertical Coordinate:"
pack $wr.mid5.lab -side top
radiobutton $wr.mid5.d0 -text "Pressure" -variable Kagl -value "0" 
radiobutton $wr.mid5.d1 -text "Meters-agl" -variable Kagl -value "1"
radiobutton $wr.mid5.d2 -text "Theta" -variable Kagl -value "2" 
radiobutton $wr.mid5.d3 -text "Meteo-varb" -variable Kagl -value "3" 
radiobutton $wr.mid5.d4 -text "None" -variable Kagl -value "4" 
pack $wr.mid5.d0 $wr.mid5.d1 $wr.mid5.d2 $wr.mid5.d3 $wr.mid5.d4 -side left

#-->zoom factor slider bar      
label $wr.mid6.lab -text "Least Zoom -------------------> Most Zoom"
scale $wr.mid6.d0 -orient horizontal -length 400 -from 0 -to 100 \
  -tickinterval 10 -variable Zoom -resolution 5 
pack $wr.mid6.lab $wr.mid6.d0 -side top -pady 3

#-->Python options
label $wr.lang.f.lab -text "Output will be in SVG"
pack $wr.lang.f.lab -expand true

frame $wr.lang.py.row1
label $wr.lang.py.row1.out_format_lab -text "Output Format:"
ttk::combobox $wr.lang.py.row1.out_format_cb -textvariable PyOutput_format \
    -values [list pdf ps jpg png svg tif] -width 5 -state readonly
$wr.lang.py.row1.out_format_cb set pdf
label $wr.lang.py.row1.street_map_lab -text "Street Map:"
ttk::combobox $wr.lang.py.row1.street_map_cb -textvariable PyStreet_map \
    -values [list "NOT_USED" OPEN_STREET OPEN_TOPO] -width 15 -state readonly
$wr.lang.py.row1.street_map_cb set "NOT_USED"
pack $wr.lang.py.row1.out_format_lab $wr.lang.py.row1.out_format_cb \
    $wr.lang.py.row1.street_map_lab $wr.lang.py.row1.street_map_cb \
    -side left -padx 5 -fill x

frame $wr.lang.py.misc
checkbutton $wr.lang.py.misc.source_tz -variable PySource_time_zone -text "Source time zone" -background grey
checkbutton $wr.lang.py.misc.debug -variable PyDebug -text "Debug messages" -background grey
pack $wr.lang.py.misc.source_tz $wr.lang.py.misc.debug -side left -padx 5

pack $wr.lang.py.row1 $wr.lang.py.misc -side top -pady 5 -padx 10

if { $PyPreferred } {
    $wr.lang select $wr.lang.py
} else {
    $wr.lang select $wr.lang.f
}

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 10 -command "destroy $wr"
button $wr.bot.help -text Help -width 8 \
       -command "load_html [file join $html_dir S230.htm ] "
button $wr.bot.save  -bg green -text "Execute Display " -width 20 -command {run_traj_plot ".trajdisp.lang"}
pack $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 
set_default
if [file exists default_tplot] {load_tdefault}
}

################################################################
#-->run plotting program

proc run_traj_plot {lang} {
global X_dir exec_dir Trajpts_file tcl_platform tcl_dir zip_pgm
global qpnt Kagl Zoom Color Label Psout_file
global Map_file Map_proj Igis View
global Ring Map Ring_num Ring_dis Map_lat Map_lon
global result log
global PyPreferred
global PyDebug PySource_time_zone PyStreet_map PyOutput_format

if {[winfo exists $lang]} {
   set PyPreferred [expr {[$lang select] == "${lang}.py"}]
}

if { $PyPreferred } {
    set out_file ${Psout_file}.${PyOutput_format}
} else {
    set out_file ${Psout_file}.html
}

if [file exists $out_file] {file delete $out_file}
#if [file exists logocon.gif] {file delete logocon.gif}
#if [file exists noaa_google.gif] {file delete noaa_google.gif}
if [file exists HYSPLITtraj.kmz] {file delete HYSPLITtraj.kmz}

# google option requires output in AGL
if { $Igis == 3 } {set Kagl 1}

set arg1 -i  ; append arg1 $Trajpts_file
set arg2 -o  ; append arg2 $out_file
set arg3 -j  ; append arg3 $Map_file
set arg4 -m  ; append arg4 $Map_proj
set arg5 -k  ; append arg5 $Color
set arg6 -l  ; append arg6 $Label
set arg7 -z  ; append arg7 $Zoom
set arg8 -v  ; append arg8 $Kagl
set arg9 -a  ; append arg9 $Igis 
set arga -s  ; append arga $qpnt

set argb "-:"; set argc "-:"
if { $Ring == 1 } {set argb -g ; append argb $Ring_num:$Ring_dis}
if { $Map  == 1 } {set argc -h ; append argc $Map_lat:$Map_lon}

if { $PyPreferred } {
    set arg_ext " "
    if { $View == 1 } { append arg_ext " --interactive" }
    if { $PyDebug } { append arg_ext " --debug" }
    if { $PySource_time_zone } { append arg_ext " --source-time-zone" }

    switch -nocase $PyStreet_map {
        OPEN_STREET { append arg_ext " --street-map=0" }
        OPEN_TOPO { append arg_ext " --street-map=1" }
        default { }
    }

    run_python_program "$exec_dir/trajplot.py" "$arg_ext \
                      $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 \
                      $arg7 $arg8 $arg9 $arga $argb $argc"
} else {
    set arg_ext "+g1"
    if { "$tcl_platform(platform)" == "unix" } {
        if { "$X_dir" == "" } {
            exec $exec_dir/trajplot $arg_ext \
                      $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 \
                      $arg7 $arg8 $arg9 $arga $argb $argc
        } else {
            exec $X_dir/xterm -fn fixed -e $exec_dir/trajplot $arg_ext \
                      $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 \
                      $arg7 $arg8 $arg9 $arga $argb $argc
        }
    } else {
            exec $exec_dir/trajplot.exe $arg_ext \
                      $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 \
                      $arg7 $arg8 $arg9 $arga $argb $argc
    }
}
save_tdefault

# -->zip Google Earth logocon.gif and kml file to kmz file
if { $Igis == 3 } {
    if { ![file exists logocon.gif] } {
       file copy $tcl_dir/logocon.gif logocon.gif
    }
    if { ![file exists noaa_google.gif] } {
       file copy $tcl_dir/noaa_google.gif noaa_google.gif
    }
    if [file exists HYSPLITtraj_html_01.kml] {
       if [ file exists $zip_pgm ] {
            exec $zip_pgm HYSPLITtraj.kmz HYSPLITtraj_html_01.kml logocon.gif noaa_google.gif \
                          redball.png blueball.png greenball.png
            file delete HYSPLITtraj_html_01.kml
            if [file exists HYSPLITtraj.kmz] {
               msg_box "KML/KMZ files created in working directory (HYSPLITtraj.kmz)"
            } else {
               msg_box "KML/KMZ files have not been created.  Make sure Infozip has been installed and defined."
            }
       } else {
            msg_box "Infozip has not been installed or correctly defined in HYSPLIT. KML/KMZ files not created."
            file delete HYSPLITtraj_html_01.kml
       }
    }
}

# -->ESRI Shapefiles
if { $Igis == 1 || $Igis == 5 } {
   foreach f [glob -nocomplain GIS_traj_html_??.txt] {
      msg_box "Created file: $f"
      tkwait window .msg_win
   }
}

if { !$PyPreferred && $View == 1 } {svg_box $out_file}
}

################################################################
#-->set display defaults

proc set_default {} {
global qpnt Kagl Zoom Color Label Psout_file 
global maps_dir Map_file Map_proj Igis View
global Ring Map Ring_num Ring_dis Map_lat Map_lon
global Start_locn
global PyDebug PySource_time_zone PyStreet_map PyOutput_format

set Igis 0
set View 1
set Psout_file trajplot

if { $Label == "" } {
   if [ info exists Map_file ] {
        if { $Map_file == "" } {set Map_file ${maps_dir}/arlmap} 
   } else {
        set Map_file ${maps_dir}/arlmap
   }
   set Map_proj 0
   set Label 12
   set Zoom 50
   set Color 1
   set Kagl 0
   set qpnt 1
   set Ring 0
   set Map 0
   set Ring_num 4 
   set Ring_dis 100
   set Map_lat [lindex $Start_locn(1) 0] 
   set Map_lon [lindex $Start_locn(1) 1] 
   set PyDebug false
   set PySource_time_zone false
   set PyStreet_map NOT_USED
   set PyOutput_format pdf
   }
}

################################################################
#-->save display defaults

proc save_tdefault {} {
global qpnt Kagl Zoom Color Label Psout_file 
global maps_dir Map_file Map_proj Igis View
global Ring Map Ring_num Ring_dis Map_lat Map_lon
global PyDebug PySource_time_zone PyStreet_map PyOutput_format

   set f [open "default_tplot" w]
   puts $f $Igis
   puts $f $View
   puts $f $Psout_file
   puts $f $Map_file
   puts $f $Map_proj
   puts $f $Label
   puts $f $Zoom
   puts $f $Color
   puts $f $Kagl
   puts $f $qpnt
   puts $f $Ring
   puts $f $Map
   puts $f $Ring_num
   puts $f $Ring_dis
   puts $f $Map_lat
   puts $f $Map_lon
   puts $f $PyDebug
   puts $f $PySource_time_zone
   puts $f $PyStreet_map
   puts $f $PyOutput_format
   close $f
}

################################################################
#-->load display defaults

proc load_tdefault {} {
global qpnt Kagl Zoom Color Label Psout_file 
global maps_dir Map_file Map_proj Igis View
global Ring Map Ring_num Ring_dis Map_lat Map_lon
global PyDebug PySource_time_zone PyStreet_map PyOutput_format

   set f [open "default_tplot" r]
   gets $f Igis
   gets $f View
   gets $f Psout_file
   gets $f Map_file
   gets $f Map_proj
   gets $f Label
   gets $f Zoom
   gets $f Color
   gets $f Kagl
   gets $f qpnt
   gets $f Ring
   gets $f Map
   gets $f Ring_num
   gets $f Ring_dis
   gets $f Map_lat
   gets $f Map_lon
   gets $f PyDebug
   gets $f PySource_time_zone
   gets $f PyStreet_map
   gets $f PyOutput_format
   close $f

   set PyDebug [string is true -strict $PyDebug]
   set PySource_time_zone [string is true -strict $PySource_time_zone]
   # for backward compatibility with files written by earlier versions
   if {[string trim $PyStreet_map] == ""} { set PyStreet_map NOT_USED }
   if {[string trim $PyOutput_format] == ""} { set PyOutput_format pdf }
}

