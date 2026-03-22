proc conc_plot {} {

#-----------------------------------------------------------------------------
# CONC_DISP.TCL: gridded concentration file display script
# Last Revised: 06 Feb 2002
#               13 Aug 2002
#               26 Sep 2002 - GIS option
#               17 Jun 2003 - View option
#               05 Dec 2003 - fix map center and rings option
#               20 Sep 2004 - turn off source location
#               25 Oct 2004 - global postscript output file
#               10 Jan 2005 - simplify xterm shell
#               27 May 2005 - reset bot/top heights for re-entry
#               21 Mar 2006 - GIS/Google Earth option
#               23 May 2006 - cleaned up look & feel
#               17 Jan 2007 - replace crop with trim
#               23 Mar 2009 - concplot error trap
#               13 Aug 2009 - browse button for map background
#               11 Dec 2009 - option to replace concentration file name
#               30 Dec 2010 - script/batch command line output
#               28 Apr 2011 - force frames option when selecting GIS ESRI
#               11 Jul 2011 - quick plot in DATEM format
#               16 Mar 2012 - pkzip for GELABEL now in a loop
#               21 Feb 2013 - test for length of DATEM file
#               02 Aug 2013 - message box for GE output
#               12 Sep 2016 - corrected browse for shapefiles.txt
#               14 Sep 2016 - added breaks and changed box sizes; save config
#               07 Nov 2016 - do not permit shapefile copy to itself
#               16 Mar 2017 - disabled bot and top for default_cplot
#               17 Jul 2017 - changed wording for Google Earth; reduced pady
#               19 Jul 2017 - added message box for ESRI conversion
#               13 Dec 2019 - replace concplot and concplot.exe with concplot.py
#               16 Jan 2020 - add option to use FORTRAN or Python trajplot.
#               07 May 2020 - fix quick start examples.
#               08 May 2020 - add vertical scrollbar.
#               26 Jul 2021 - use svg instead of postscript
#               27 Jul 2021 - remove ghostscript and ghostview dependencies.
#               04 Apr 2022 - add -81 option to concplot to create plot(s)
#                             even when all values are zero.
#               31 May 2023 - correct GIS filenames by replacing _ps with _html.
#                           - copy logocon.gif and noaa_google.gif when needed.
#               07 Nov 2023 - change street maps to OPEN_STREET and OPEN_TOPO.
#-----------------------------------------------------------------------------

global html_dir Grid_name Hei_agl Num_disp Bot Top Lev Identis Num_polid
global Mass Cval Zoom Color Fixed Smooth Cscale Dscale Igis View
global Psout_file Map_file Map_proj Frame Expose Remove zip_pgm name_datem
global Ring Map Ring_num Ring_dis Map_lat Map_lon qpnt Grid_name_n 
global PyDebug PySource_time_zone PyStreet_map PyOutput_format
global PyPreferred

if [winfo exists .concdisp] {destroy .concdisp}
set topwin .concdisp
toplevel $topwin
wm title $topwin " Concentration Display   "
wm  geometry $topwin +150+0

# create a scrollable frame (wr).
frame $topwin.outerframe
canvas $topwin.outerframe.canvas -yscrollcommand "$topwin.outerframe.yscroll set"
scrollbar $topwin.outerframe.yscroll -command "$topwin.outerframe.canvas yview"
frame $topwin.outerframe.canvas.innerframe
set wr $topwin.outerframe.canvas.innerframe

frame $wr.mid0
frame $wr.mid1

frame $wr.div2
frame $wr.mid2
frame $wr.mid3
frame $wr.midc
frame $wr.setm

frame $wr.div1
frame $wr.mid4
frame $wr.mid5
frame $wr.mid6
frame $wr.mid7

frame $wr.div0
frame $wr.mida
frame $wr.mid9
frame $wr.midd
frame $wr.mid8
frame $wr.mide

frame $wr.midq
frame $wr.midb
frame $wr.bot

ttk::notebook $wr.lang
frame $wr.lang.py
frame $wr.lang.f
$wr.lang add $wr.lang.f -text "Fortran"
$wr.lang add $wr.lang.py -text "Python"

pack $wr.mid0 -anchor w -pady 1 -padx 10 -pady 10
pack $wr.mid1 -anchor w -padx 10

pack $wr.div2 -pady 1
pack $wr.mid2 $wr.mid3 $wr.midc $wr.setm -anchor w -padx 10

pack $wr.div1 -pady 1
pack $wr.mid4 $wr.mid5 $wr.mid6 $wr.mid7 -anchor w  -padx 10

pack $wr.div0 -pady 1
pack $wr.mida $wr.mid9 $wr.midd $wr.mid8 $wr.mide -padx 10

pack $wr.midq $wr.midb $wr.lang $wr.bot -pady 5 -padx 10
pack configure $wr.lang -expand true -fill x

#-->select input binary data file
label $wr.mid0.lab -text "   Input File:"
pack $wr.mid0.lab -side left
set d 0
foreach item $Grid_name {
   radiobutton $wr.mid0.$d -variable Num_disp -text $item -value $d  \
                           -background grey -command "conc_plot"
   pack $wr.mid0.$d -side left
   incr d
}
entry $wr.mid0.ent -textvariable Grid_name_n -relief sunken -width 15
label $wr.mid0.txt -text "        "
pack  $wr.mid0.txt $wr.mid0.ent -side left 

#-->output postscript file
label $wr.mid1.lab -text "  Output File:"
entry $wr.mid1.ent -textvariable Psout_file -relief sunken -width 15
checkbutton $wr.mid1.box -variable Frame -text "Frames" -background grey
checkbutton $wr.mid1.psv -variable View -text "View On" -background grey
pack $wr.mid1.lab $wr.mid1.ent $wr.mid1.box $wr.mid1.psv -padx 2 -side left 

label $wr.div2.lab -fg blue -text "MAPPING ____________________________________________________"
pack  $wr.div2.lab -pady 1 -side left

#-->map background file
label $wr.mid2.lab -text "Map Background:"
entry $wr.mid2.ent -textvariable Map_file -relief sunken -width 32
button $wr.mid2.win  -text Browse -width 12 -command {
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
pack $wr.mid2.lab $wr.mid2.ent $wr.mid2.win -side left -padx 6

#-->map projection options
label $wr.mid3.lab -text "Map Projection:"
radiobutton $wr.mid3.d0 -text "Auto" -variable Map_proj -value "0"
radiobutton $wr.mid3.d1 -text "Polar" -variable Map_proj -value "1"
radiobutton $wr.mid3.d2 -text "Lambert" -variable Map_proj -value "2"
radiobutton $wr.mid3.d3 -text "Mercator" -variable Map_proj -value "3"
radiobutton $wr.mid3.d4 -text "CylEquid" -variable Map_proj -value "4"
pack $wr.mid3.lab -side left 
pack $wr.mid3.d0 $wr.mid3.d1 $wr.mid3.d2 $wr.mid3.d3 $wr.mid3.d4 -side left

#-->GIS/Google Earth options
label $wr.midc.lab -text "    GIS Output:"
radiobutton $wr.midc.d0 -text "None" -variable Igis -value "0"
radiobutton $wr.midc.d1 -text "ESRI Generate   "   -variable Igis -value "1"
radiobutton $wr.midc.d3 -text "KML/KMZ"  -variable Igis -value "3"
pack $wr.midc.lab $wr.midc.d0 $wr.midc.d1 $wr.midc.d3 -side left

#-->optional map location set
frame $wr.setm.source
frame $wr.setm.rings
frame $wr.setm.centr
pack $wr.setm.source $wr.setm.rings $wr.setm.centr -side left -padx 10

label $wr.setm.source.lab -text "Label Source" 
radiobutton $wr.setm.source.on  -text "On"  -variable qpnt -value "1" -bg grey
radiobutton $wr.setm.source.off -text "Off" -variable qpnt -value "0" -bg grey
pack $wr.setm.source.lab
pack $wr.setm.source.on $wr.setm.source.off -side left

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

label $wr.div1.lab -fg blue -text "DATA ________________________________________________________"
pack  $wr.div1.lab -pady 1 -side left

#-->select pollutant
label $wr.mid4.lab -text "       Select Pollutant:"
pack $wr.mid4.lab -side left
set d 0
foreach item [concat "All" $Identis] {
   radiobutton $wr.mid4.$d -variable Num_polid -text $item -value $d \
                           -background grey
   pack $wr.mid4.$d -side left
   incr d
}

#-->height display options
label $wr.mid5.lev -text "       Vertical Display:"
pack $wr.mid5.lev -side left
set d 0
foreach item [list "Show Each Level" "Average Levels"] {
   radiobutton $wr.mid5.$d -variable Lev -text $item -value [expr $d+1] \
                           -background grey 
   pack $wr.mid5.$d -side left
   incr d
   }

#-->select bottom heights
label $wr.mid6.bot -text "      From Bottom Level:"
pack $wr.mid6.bot -side left
set Hgt_grid [lindex $Hei_agl $Num_disp]
set Hgt_list [split $Hgt_grid]
set d 0
foreach item $Hgt_list {
   radiobutton $wr.mid6.$d -variable Bot -text $item -value $item \
                           -background grey 
   pack $wr.mid6.$d -side left
   incr d
   }

#-->select top heights
label $wr.mid7.top -text "      Through Top Level:" 
pack $wr.mid7.top -side left
set Hgt_grid [lindex $Hei_agl $Num_disp]
set Hgt_list [split $Hgt_grid]
set d 0
foreach item $Hgt_list {
   radiobutton $wr.mid7.$d -variable Top -text $item -value $item \
                           -background grey 
   pack $wr.mid7.$d -side left
   incr d
   }

label $wr.div0.lab -fg blue -text "CONTOURS ____________________________________________________"
pack  $wr.div0.lab -pady 1 -side left

#-->deposition contour scaling
label $wr.mida.lab -text "   Deposition Multiplier:"
entry $wr.mida.ent -textvariable Dscale -relief sunken -width 10
radiobutton $wr.mida.d0 -text "None"  -variable Remove -value "0"
radiobutton $wr.mida.d1 -text "Time"  -variable Remove -value "1"
radiobutton $wr.mida.d2 -text "Sum"   -variable Remove -value "2"
radiobutton $wr.mida.d3 -text "Total" -variable Remove -value "3"
pack $wr.mida.lab $wr.mida.ent -side left 
pack $wr.mida.d0 $wr.mida.d1 $wr.mida.d2 $wr.mida.d2 $wr.mida.d3 -side left 

#-->concentration contour scaling
label $wr.mid9.lab -text "Concentration Multiplier:"
entry $wr.mid9.ent -textvariable Cscale -relief sunken -width 10
checkbutton $wr.mid9.box -variable Expose -text "Exposure" -background grey
pack $wr.mid9.lab $wr.mid9.ent -side left 
pack $wr.mid9.box -side left 

#-->optional mass label
label $wr.mid9.txt -text " Label:"
entry $wr.mid9.val -textvariable Mass -relief sunken -width 10
pack $wr.mid9.txt $wr.mid9.val -side left 

#-->contour lines
label $wr.midd.lab -text "Contour drawing options: " 
radiobutton $wr.midd.d0 -text "B & W " -variable Color -value "0"
radiobutton $wr.midd.d1 -text "Color " -variable Color -value "1"
radiobutton $wr.midd.d2 -text "None  " -variable Color -value "2"
pack $wr.midd.lab $wr.midd.d0 $wr.midd.d1 $wr.midd.d2 -side left 

#-->fixed concentration units maximum contour
radiobutton $wr.mid8.d0 -text "Dyn-Exp" -variable Fixed -value "0" 
radiobutton $wr.mid8.d1 -text "Fix-Exp" -variable Fixed -value "1" 
radiobutton $wr.mid8.d2 -text "Dyn-Lin" -variable Fixed -value "2"
radiobutton $wr.mid8.d3 -text "Fix-Lin" -variable Fixed -value "3"
radiobutton $wr.mid8.d4 -text "User Set" -variable Fixed -value "4"
pack $wr.mid8.d0 $wr.mid8.d1 $wr.mid8.d2 $wr.mid8.d3 -side left -padx 2
pack $wr.mid8.d4 -side left -padx 6

#-->user set contour values
label $wr.mide.lab -text "User Set Values:"
entry $wr.mide.ent -textvariable Cval -relief sunken -width 40
pack $wr.mide.lab $wr.mide.ent -side left -padx 2

#-->quick data plot DATEM format
label $wr.midq.lab -text "DATEM plot file:"
entry $wr.midq.ent -textvariable name_datem -relief sunken -width 30
button $wr.midq.win  -text Browse -width 12 -command {
   set temp [tk_getOpenFile -title "File Selection"]
   if {[string length $temp] > 0} {set name_datem $temp}}
pack $wr.midq.lab $wr.midq.ent $wr.midq.win -side left -padx 6

#-->zoom factor slider bar      
label $wr.midb.lab -bg white -text "Least Zoom ----------------------------------------> Most Zoom"
scale $wr.midb.d0 -bg white -orient horizontal -length 500 -from 0 -to 100 \
  -tickinterval 10 -variable Zoom -resolution 5 
pack $wr.midb.lab $wr.midb.d0 -side top

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
button $wr.bot.dismiss  -bg red -text Quit -width 12 -command "destroy $topwin"
button $wr.bot.help -text Help  -width 8 \
       -command "load_html [file join $html_dir S330.htm ] "
button $wr.bot.save  -bg green -text "Execute Display " -width 34 -command {run_conc_plot ".concdisp.outerframe.canvas.innerframe.lang"}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 -pady 2

# scrollable frame business
pack $topwin.outerframe.canvas.innerframe -expand yes -fill both -side top
pack $topwin.outerframe.yscroll -side right -fill y
$topwin.outerframe.canvas create window 0 0 -anchor nw -window $topwin.outerframe.canvas.innerframe
$topwin.outerframe.canvas configure -scrollregion [$topwin.outerframe.canvas bbox all]
pack $topwin.outerframe.canvas -expand yes -fill both -side top
pack $topwin.outerframe -expand yes -fill both -side top
bind $topwin.outerframe <Map> {
   set topwin .concdisp
   # assume only 80% of the screen height is available.
   set content_height [winfo height $topwin.outerframe.canvas.innerframe]
   set avail_scr_height [expr 0.80 * [winfo screenheight .]]
   if {$content_height >= $avail_scr_height} {
      set view_height $avail_scr_height
   } else {
      set view_height $content_height
   }
   $topwin.outerframe.canvas.innerframe configure -height $content_height
   $topwin.outerframe.canvas configure -width 650 -height $view_height
   $topwin.outerframe.canvas configure -scrollregion [$topwin.outerframe.canvas bbox all]
}

set_defaultc
if [file exists default_cplot] {load_defaultc}
}

###################################################################
#-->run plotting program

proc run_conc_plot {lang} {
global X_dir Grid_dir Grid_name exec_dir tcl_platform zip_pgm
global Num_polid Num_disp Bot Top Lev Hei_agl Igis View tcl_dir
global Mass Cval Zoom Color Fixed Smooth Cscale Dscale
global Psout_file Map_file Map_proj Frame Expose Remove 
global Ring Map Ring_num Ring_dis Map_lat Map_lon qpnt
global magick_pgm Grid_name_n name_datem
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

# set Grid_name_n  [lindex $Grid_name $Num_disp]  -- set in "proc set_defaultc"

set Grid_dir_n [lindex $Grid_dir $Num_disp]
set input "$Num_polid\n"

if [file exists $out_file] {file delete $out_file}
#if [file exists logocon.gif] {file delete logocon.gif}
#if [file exists noaa_google.gif] {file delete noaa_google.gif}
if [file exists HYSPLITconc.kmz] {file delete HYSPLITconc.kmz}

set arg1 -i  ; append arg1 $Grid_dir_n$Grid_name_n
set arg2 -o  ; append arg2 $out_file
set arg3 -j  ; append arg3 $Map_file
set arg4 -f  ; append arg4 $Frame
set arg5 -b  ; append arg5 $Bot
set arg6 -t  ; append arg6 $Top
set arg7 -e  ; append arg7 $Expose
set arg8 -d  ; append arg8 $Lev
set arg9 -r  ; append arg9 $Remove
set arga -c  ; append arga $Fixed
set argb -k  ; append argb $Color
set argc -m  ; append argc $Map_proj
set argd -s  ; append argd $Num_polid
set arge -x  ; append arge $Cscale 
set argf -y  ; append argf $Dscale 
set argg -z  ; append argg $Zoom
set argh -u  ; append argh $Mass
set argi -a  ; append argi $Igis
set argq -q  ; append argq $name_datem

set argj "-:"; set argk "-:"; set argl "-:"
if { $Ring == 1 } {set argj -g  ; append argj $Ring_num:$Ring_dis}
if { $Map  == 1 } {set argk -h  ; append argk $Map_lat:$Map_lon}
if { $qpnt == 0 } {set argl -l32}
if { $Igis == 1 } {set argb -k1 ; set arg4 -f1 ; set Frame 1}

if { $name_datem == "" } {
   set argq "-:"
} else {
   set argq -q ; append argq $name_datem
}

if { $Fixed == 4 } {set argm "-v$Cval"} else {set argm "-:"}

if [file exists ${Grid_dir_n}${Grid_name_n}] {
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
                         $arg1 $arg2 $arg3 $arg4 \
                         $arg5 $arg6 $arg7 $arg8 $arg9 $arga $argb $argc $argd $arge $argf \
                         $argg $argh $argi $argj $argk $argl $argm $argq"
    } else {
        set arg_ext "+g1"
        set arg_ext2 "-81"

        if { "$tcl_platform(platform)" == "unix" } {
            if { "$X_dir" == "" } {
                exec $exec_dir/concplot $arg_ext $arg_ext2 \
                         $arg1 $arg2 $arg3 $arg4 \
                         $arg5 $arg6 $arg7 $arg8 $arg9 $arga $argb $argc $argd $arge $argf \
                         $argg $argh $argi $argj $argk $argl $argm $argq
            } else {
                exec $X_dir/xterm -fn fixed -e $exec_dir/concplot $arg_ext $arg_ext2 \
                         $arg1 $arg2 $arg3 $arg4 \
                         $arg5 $arg6 $arg7 $arg8 $arg9 $arga $argb $argc $argd $arge $argf \
                         $argg $argh $argi $argj $argk $argl $argm $argq
            }

            set f [open "concplot.sh" w]
            puts $f "#!/bin/sh"
            puts $f "$exec_dir/concplot $arg_ext $arg_ext2 \
                         $arg1 $arg2 $arg3 $arg4 \
                         $arg5 $arg6 $arg7 $arg8 $arg9 $arga $argb $argc $argd $arge $argf \
                         $argg $argh $argi $argj $argk $argl $argm $argq"
            close $f
        } else {
            if [catch {
                exec $exec_dir/concplot.exe $arg_ext $arg_ext2 \
                         $arg1 $arg2 $arg3 $arg4 \
                         $arg5 $arg6 $arg7 $arg8 $arg9 $arga $argb $argc $argd $arge $argf \
                         $argg $argh $argi $argj $argk $argl $argm $argq} result] {
                msg_box "ERROR: concentration display file not created!"
            }

            set f [open "concplot.bat" w]
            puts $f "echo off"
            puts $f "$exec_dir/concplot.exe $arg_ext $arg_ext2 \
                         $arg1 $arg2 $arg3 $arg4 \
                         $arg5 $arg6 $arg7 $arg8 $arg9 $arga $argb $argc $argd $arge $argf \
                         $argg $argh $argi $argj $argk $argl $argm $argq"
            close $f
        }
    }
} else {
    msg_box "ERROR: concentration dump file not created!"
}
save_defaultc

# -->zip Google Earth logocon.gif and kml file to kmz file
if { $Igis == 3 } {
   if { ![file exists logocon.gif] } {
      file copy $tcl_dir/logocon.gif logocon.gif
   }
   if { ![file exists noaa_google.gif] } {
      file copy $tcl_dir/noaa_google.gif noaa_google.gif
   }

   if [ file exists $zip_pgm ] {
      exec $zip_pgm HYSPLITconc.kmz HYSPLIT_html.kml icon63.png logocon.gif noaa_google.gif
      file delete HYSPLIT_html.kml
      if [ file exists HYSPLITconc.kmz ] {msg_box "KML/KMZ file created in working directory (HYSPLITconc.kmz)"}

   } else {
      msg_box "Infozip has not been installed or correctly defined in HYSPLIT.  KML/KMZ file not created."
   }

  if [ file exists $magick_pgm ] {
      if { $Color == 2 } {
         msg_box "Contours must be color or black/white for Google Earth output"
      } else {
         if [ file exists GELABEL_html.txt] {

            exec $exec_dir/gelabel +g1 -phtml
            set ii "01"
            set i 1
            set Filein  "GELABEL_${ii}_html.html"

#           create contour legends
            while { [file exists $Filein] } {
               set Fileout "GELABEL_${ii}_html.gif"

               if { "$tcl_platform(platform)" == "unix" } {
                  set Tempfile "tempfile.svg"
                  if [file exists $Tempfile] {file delete $Tempfile}
                  exec $exec_dir/splitsvg -f1 -i$Filein -o$Tempfile
                  exec $magick_pgm +adjoin -trim +page "F01-${Tempfile}" $Fileout
                  if [file exists $Tempfile] {file delete $Tempfile}

               } else {
#                 most windows PC installations
                  set Tempfile "tempfile.svg"
                  if [file exists $Tempfile] {file delete $Tempfile}
                  exec $exec_dir/splitsvg -f1 -i$Filein -o$Tempfile
                  exec $magick_pgm +adjoin -trim +page "F01-${Tempfile}" $Fileout
                  if [file exists $Tempfile] {file delete $Tempfile}
               }
               if [file exists $zip_pgm] {exec $zip_pgm -u HYSPLITconc.kmz $Fileout} 
               if [file exists $Filein]   {file delete $Filein}
               if [file exists $Fileout]  {file delete $Fileout}
           
               incr i
               if {$i<10} {set ii "0$i"} else {set ii $i}
               set Filein "GELABEL_${ii}_html.html"
            }
            if [ file exists gistmp_html.txt] {
               file delete gistmp_html.txt
               file delete GELABEL_html.txt
            }
         }
      }
  } else { 
     msg_box "ImageMagick Required: $magick_pgm"}
}

# -->ESRI Shapefiles
if { $Igis == 1 } {
   if { $Color == 2 } {
      msg_box "Contours must be color or black/white for ESRI Shapefile output"
   }

   foreach f [glob -nocomplain GIS_?????_html_??.txt] {
      msg_box "Created file: $f"
      tkwait window .msg_win
   }
}

if { !$PyPreferred && $View == 1 } {
   if { $Frame == 1 } {

      if [file exists ${Psout_file}0000.html] {
         set i 0
         set Temp_file ${Psout_file}0000.html
      } else {
         set i 1
         set Temp_file ${Psout_file}0001.html
      }

      while { [file exists $Temp_file] } {
         svg_box $Temp_file
         incr i
         set ii [format "%4.4u" $i]
         set Temp_file ${Psout_file}${ii}.html
      }

   } else {

     if [file exists ${Psout_file}.html] {
         svg_box ${Psout_file}.html
     } else {
        msg_box "${Psout_file}.html not found, check working directory for other output files" 
     }
   }
}
}

###################################################################
#-->set initial defaults

proc set_defaultc {} {
global Num_polid Smooth Num_disp Bot Top Lev Hei_agl Identis
global Mass Cval Zoom Color Fixed Smooth Cscale Dscale Igis View
global Psout_file Map_file Map_proj Frame Expose Remove 
global Ring Map Ring_num Ring_dis maps_dir Map_lat Map_lon qpnt
global Start_locn Grid_name Grid_name_n name_datem
global PyDebug PySource_time_zone PyStreet_map PyOutput_format

set Igis 0
set View 1
set Psout_file concplot
set Grid_name_n [lindex $Grid_name $Num_disp]

if [ info exists name_datem ] { } else {
   set name_datem ""
}

if { $Lev == "" } {

   set Map_file ${maps_dir}/arlmap 
   set Map_proj 0
   set Num_polid 1
   set Lev 1
   set Hgt_grid [lindex $Hei_agl $Num_disp]
   set Hgt_list [split $Hgt_grid]
   set Bot [lindex $Hgt_list 0]
   set Top [lindex $Hgt_list 0]
   set Zoom 50
   set Color 1
   set Cval "10+5+2+1"
   set Fixed 0
   set Cscale 1.0
   set Dscale 1.0
   set Smooth 1.0
   set Remove 1
   set Expose 0
   set Frame 0
   set Mass ""
   set Ring 0
   set Map 0
   set Ring_num 4
   set Ring_dis 100
   set qpnt 1
   set Map_lat [lindex $Start_locn(1) 0]
   set Map_lon [lindex $Start_locn(1) 1]
   set PyDebug false
   set PySource_time_zone false
   set PyStreet_map NOT_USED
   set PyOutput_format pdf

} else {

   set Hgt_grid [lindex $Hei_agl $Num_disp]
   set Hgt_list [split $Hgt_grid]

   set d 0
   foreach item $Hgt_list {
      if { $Bot == $item } {incr d}
      if { $Top == $item } {incr d}
   }

   if { $d < 2 } {
      set Bot [lindex $Hgt_list 0]
      set Top [lindex $Hgt_list 0]
   }
}
}

###################################################################
#-->save menu defaults

proc save_defaultc {} {
global Num_polid Smooth Num_disp Hei_agl Identis
global Mass Cval Zoom Color Fixed Smooth Cscale Dscale Igis View
global Psout_file Map_file Map_proj Frame Expose Remove 
global Ring Map Ring_num Ring_dis maps_dir Map_lat Map_lon qpnt
global Start_locn Grid_name Grid_name_n name_datem
global PyDebug PySource_time_zone PyStreet_map PyOutput_format

   set f [open "default_cplot" w]
   puts $f $Map_file
   puts $f $Map_proj
   puts $f $Num_polid
   puts $f $Zoom
   puts $f $Color
   puts $f $Cval
   puts $f $Fixed
   puts $f $Cscale
   puts $f $Dscale
   puts $f $Smooth
   puts $f $Remove
   puts $f $Expose
   puts $f $Frame
   puts $f $Mass
   puts $f $Ring
   puts $f $Map
   puts $f $Ring_num
   puts $f $Ring_dis
   puts $f $qpnt
   puts $f $Map_lat
   puts $f $Map_lon
   puts $f $PyDebug
   puts $f $PySource_time_zone
   puts $f $PyStreet_map
   puts $f $PyOutput_format
   close $f
}

###################################################################
#-->load menu defaults

proc load_defaultc {} {
global Num_polid Smooth Num_disp Hei_agl Identis
global Mass Cval Zoom Color Fixed Smooth Cscale Dscale Igis View
global Psout_file Map_file Map_proj Frame Expose Remove 
global Ring Map Ring_num Ring_dis maps_dir Map_lat Map_lon qpnt
global Start_locn Grid_name Grid_name_n name_datem
global PyDebug PySource_time_zone PyStreet_map PyOutput_format

   set f [open "default_cplot" r]
   gets $f Map_file
   gets $f Map_proj
   gets $f Num_polid
   gets $f Zoom
   gets $f Color
   gets $f Cval
   gets $f Fixed
   gets $f Cscale
   gets $f Dscale
   gets $f Smooth
   gets $f Remove
   gets $f Expose
   gets $f Frame
   gets $f Mass
   gets $f Ring
   gets $f Map
   gets $f Ring_num
   gets $f Ring_dis
   gets $f qpnt
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
