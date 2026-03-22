proc srm_plot {} {

#-----------------------------------------------------------------------------
# SRM_DISP.TCL: source receptor maps from matrix simulation 
# Last Revised: 11 Mar 2002
#               13 Aug 2002
#               26 Sep 2002
#               17 Jun 2003 - view option
#               05 Jan 2004 - level option
#               21 Sep 2004 - do not destroy widget after display
#               25 Oct 2004 - global postscript file name
#               10 Jan 2005 - revised xterm link
#               13 Aug 2009 - browse button for map background file
#               23 Sep 2009 - options on extracted binary file
#               01 Aug 2013 - revised file missing test
#               12 Sep 2016 - fixed browse for shapefiles.txt
#               07 Nov 2016 - do not permit shapefile copy to itself
#               28 Jul 2017 - renamed Ptype to SRM_type
#               13 Dec 2019 - replace concplot and concplot.exe with concplot.py
#               17 Jan 2020 - add option to use FORTRAN or Python trajplot.
#               02 Sep 2020 - default undefined Cscale = 1
#               26 Jul 2021 - use svg instead of postscript
#               04 Apr 2022 - add -81 option to concplot to create plot(s)
#                             even when all values are zero.
#               07 Nov 2023 - change street maps to OPEN_STREET and OPEN_TOPO.
#-----------------------------------------------------------------------------

global Grid_name Num_disp Zoom Color SRM_type Norm Plat Plon Level Cscale
global html_dir Psout_file Psbin_file Map_file Map_proj Frame Igis View
global PyDebug PySource_time_zone PyStreet_map PyOutput_format
global PyPreferred

if [winfo exists .srmdisp] {destroy .srmdisp}
set wr .srmdisp
toplevel $wr
wm title $wr " Matrix Source-Receptor Probability Display  "
wm  geometry $wr +100+25

frame $wr.top  
frame $wr.mid0
frame $wr.mid6
frame $wr.mid1
frame $wr.mid2
frame $wr.mid3
frame $wr.mid4
frame $wr.mid5
frame $wr.mid7
frame $wr.bot

ttk::notebook $wr.lang
frame $wr.lang.py
frame $wr.lang.f
$wr.lang add $wr.lang.f -text "Fortran"
$wr.lang add $wr.lang.py -text "Python"

pack $wr.top -side top -pady 4
pack $wr.mid0 -pady 10
pack $wr.mid6 $wr.mid1 $wr.mid2 $wr.mid3 -anchor w 
pack $wr.mid4 -pady 5 
pack $wr.mid5
pack $wr.mid7 $wr.lang $wr.bot -pady 10
pack configure $wr.lang -expand true -fill x -padx 60

#-->description

label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
-text "Extracts either a source or receptor map from the source-receptor\
concentration file created by the matrix simulation. Note that the CONTROL\
file must be preconfigured for multiple starting points prior to the run\
and the matrix checkbox enabled in the configuration menu." 
pack $wr.top.lab -padx 5

#-->select input binary data file

label $wr.mid0.lab -text "Input File:"
pack $wr.mid0.lab -side left
set d 0
foreach item $Grid_name {
   radiobutton $wr.mid0.$d -variable Num_disp -text $item -value $d  \
                           -background grey -command "prob_plot"
   pack $wr.mid0.$d -side left
   incr d
}
label $wr.mid0.lab1 -text "     Height Index: "
entry $wr.mid0.ent1 -textvariable Level -width 2
pack $wr.mid0.lab1  $wr.mid0.ent1 -side left

#-->extracted binary file

label $wr.mid6.lab1 -text "   Binary file:"
entry $wr.mid6.ent1 -textvariable Psbin_file -relief sunken -width 15
label $wr.mid6.lab2 -text "   Conversion Factor:"
entry $wr.mid6.ent2 -textvariable Cscale -relief sunken -width 12
pack  $wr.mid6.lab1 $wr.mid6.ent1 $wr.mid6.lab2 $wr.mid6.ent2 -side left -padx 4

#-->output postscript file

label $wr.mid1.lab -text "  Display File:"
entry $wr.mid1.ent -textvariable Psout_file -relief sunken -width 15
checkbutton $wr.mid1.box -variable Frame -text "Frames" -background grey
checkbutton $wr.mid1.gis -variable Igis -text "GIS" -background grey
checkbutton $wr.mid1.psv -variable View -text "View" -background grey
checkbutton $wr.mid1.kol -variable Color -text Color -background grey
pack $wr.mid1.lab $wr.mid1.ent $wr.mid1.box $wr.mid1.gis $wr.mid1.psv \
     $wr.mid1.kol -side left -padx 4

#-->map background file

label $wr.mid2.lab -text "Map Background:"
entry $wr.mid2.ent -textvariable Map_file -relief sunken -width 40
button $wr.mid2.win  -text Browse -width 9 -command {
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
pack $wr.mid2.lab $wr.mid2.ent $wr.mid2.win -side left -padx 4

#-->map projection options

label $wr.mid3.lab -text "   Projection:"
radiobutton $wr.mid3.d0 -text "Auto"     -variable Map_proj -value "0" 
radiobutton $wr.mid3.d1 -text "Polar"    -variable Map_proj -value "1" 
radiobutton $wr.mid3.d2 -text "Lambert"  -variable Map_proj -value "2" 
radiobutton $wr.mid3.d3 -text "Mercator" -variable Map_proj -value "3" 
pack $wr.mid3.lab -side left -padx 10
pack $wr.mid3.d0 $wr.mid3.d1 $wr.mid3.d2 $wr.mid3.d3 -side left

#-->output selection type

label $wr.mid4.lab -text "Extraction Method"
radiobutton $wr.mid4.d0 -text "Source"   -variable SRM_type -value "s" 
radiobutton $wr.mid4.d1 -text "Receptor" -variable SRM_type -value "r" 
checkbutton $wr.mid4.box -variable Norm -text "Normalization" -background grey
pack $wr.mid4.lab -side top
pack $wr.mid4.d0 $wr.mid4.d1 -side left 
pack $wr.mid4.box -side left -padx 10 

label $wr.mid5.lab1 -text "Latitude:"
entry $wr.mid5.ent1 -textvariable Plat -relief sunken -width 10
label $wr.mid5.lab2 -text "    Longitude:"
entry $wr.mid5.ent2 -textvariable Plon -relief sunken -width 10
pack $wr.mid5.lab1 $wr.mid5.ent1 $wr.mid5.lab2 $wr.mid5.ent2 -side left 

#-->zoom factor slider bar

label $wr.mid7.lab -text "Least Zoom -------------------> Most Zoom"
scale $wr.mid7.d0 -orient horizontal -length 400 -from 0 -to 100 \
  -tickinterval 10 -variable Zoom -resolution 5 
pack $wr.mid7.lab $wr.mid7.d0 -side top -pady 3

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

button $wr.bot.dismiss  -bg red -text Quit -width 12 -command "destroy $wr"
button $wr.bot.help -text Help -width 8 \
       -command "load_html [file join $html_dir S335.htm ] "
button $wr.bot.save  -bg green -text "Execute Display " -width 24 -command {run_srm_plot ".srmdisp.lang"}
pack $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 
set_defaultx
}

#------------------------------------------------------------------------------
# run plotting program

proc run_srm_plot {lang} {

global Grid_name Grid_dir exec_dir Num_disp Zoom Color SRM_type Norm Plat Plon
global X_dir tcl_platform Map_file Map_proj Frame Igis View Level Cscale
global Psout_file Psbin_file 
global PyPreferred
global PyDebug PySource_time_zone PyStreet_map PyOutput_format

set PyPreferred [expr {[$lang select] == "${lang}.py"}]

if { $PyPreferred } {
    set out_file ${Psout_file}.${PyOutput_format}
} else {
    set out_file ${Psout_file}.html
}

set Grid_dir_n [lindex $Grid_dir $Num_disp]
set Grid_name_n  [lindex $Grid_name $Num_disp]

set arg1 -i  ; append arg1 $Grid_dir_n$Grid_name_n
set arg2 -o  ; append arg2 $Psbin_file
set arg3 -y  ; append arg3 $Plat
set arg4 -x  ; append arg4 $Plon 
set arg7 -z  ; append arg7 $Level
set arg5 -m  ; append arg5 $SRM_type
set arg6 -n  ; if { $Norm == 0 } {set arg6 "-:"}

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
   exec $exec_dir/matrix $arg1 $arg2 $arg3 $arg4 $arg7 $arg5 $arg6
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/matrix $arg1 $arg2 $arg3 \
                                  $arg4 $arg7 $arg5 $arg6
   }
} else {
   exec $exec_dir/matrix.exe $arg1 $arg2 $arg3 $arg4 $arg7 $arg5 $arg6
}

if [file exists $Psbin_file] { } else {msg_box "Extracted file $Psbin_file not found!"}

set f [open "LABELS.CFG" w]
if { "$SRM_type" == "r"} {
   puts $f "'TITLE&','Sources Contributing to the selected Receptor&'"
   puts $f "'MAPID&',' Values &'"
   if { $Norm == 1 } {
      puts $f "'UNITS&',' %&'" 
      puts $f "'VOLUM&',' &'"
      }
   }
if { "$SRM_type" == "s"} {
   puts $f "'TITLE&','Contributions from the selected Source&'"
   puts $f "'MAPID&','Air Concentration &'"
   }
close $f

set arg1 -i  ; append arg1 $Psbin_file
set arg2 -o  ; append arg2 $out_file
set arg3 -j  ; append arg3 $Map_file
set arg4 -m  ; append arg4 $Map_proj
set arg5 -f  ; append arg5 $Frame
set arg6 -k  ; append arg6 $Color
set arg7 -z  ; append arg7 $Zoom
set arg8 -a  ; append arg8 $Igis
set arg9 -x  ; append arg9 $Cscale

if { $PyPreferred } {
    set arg_ext " "
    set arg_ext2 "-81"
    if { $View == 1 } { append arg_ext " --interactive" }
    if { $PyDebug } { append arg_ext " --debug" }
    if { $PySource_time_zone } { append arg_ext " --source-time-zone" }

    switch -nocase $PyStreet_map {
        OPEN_STREET { append arg_ext " --street-map=0" }
        OPEN_TOPO { append arg_ext " --street-map=1" }
        default { }
    }

    run_python_program "$exec_dir/concplot.py" "$arg_ext $arg_ext2 \
                 $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8 $arg9"
} else {
    set arg_ext "+g1"
    set arg_ext2 "-81"
    if { "$tcl_platform(platform)" == "unix" } {
        if { "$X_dir" == "" } {
            exec $exec_dir/concplot $arg_ext $arg_ext2 \
                 $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8 $arg9
        } else {
            exec $X_dir/xterm -fn fixed -e $exec_dir/concplot $arg_ext $arg_ext2 \
                 $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8 $arg9
        }
    } else {
            exec $exec_dir/concplot.exe $arg_ext $arg_ext2 \
                 $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8 $arg9
    }
}

if [file exists LABELS.CFG] {file delete -force LABELS.CFG}
if { !$PyPreferred && $View == 1 } {svg_box $out_file} 
}


#------------------------------------------------------------------------------
# set initial defaults

proc set_defaultx {} {

global Grid_name Num_disp Zoom Color SRM_type Norm Plat Plon
global maps_dir Map_file Map_proj Frame Igis View Level Cscale
global Psout_file Psbin_file srm_init
global PyDebug PySource_time_zone PyStreet_map PyOutput_format

set Grid_name_n  [lindex $Grid_name $Num_disp]
set Igis 0
set View 1

if [ info exists SRM_type ] { } else {set SRM_type ""}

if {$SRM_type == ""} {
   set SRM_type s
   set Num_disp 0
   if {$Map_file == ""} {set Map_file ${maps_dir}/arlmap}
   set Map_proj 0
   set Num_polid 1
   set Zoom 50
   set Color 1
   set Frame 0
   set Level 1
   set Norm 0   
   set Plat 0.0
   set Plon 0.0
   if {"$Cscale" == ""} {set Cscale 1.0}
   set Psout_file srmplot
   set Psbin_file SRM_$Grid_name_n
   set PyDebug false
   set PySource_time_zone false
   set PyStreet_map NOT_USED
   set PyOutput_format pdf
   }
}
