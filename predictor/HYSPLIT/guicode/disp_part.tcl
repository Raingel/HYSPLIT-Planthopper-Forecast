proc part_plot {} {

#-----------------------------------------------------------------------------
# DISP_PART.TCL: particle position display script
# Last Revised: 03 Jun 2003
#               21 Sep 2004 - do not close widget after display
#               25 Oct 2004 - global postscript output file
#               10 Jan 2005 - simplify xterm shell
#               03 Jun 2005 - force cross-section
#               19 Oct 2005 - initialization fix
#               06 Nov 2008 - version 4.9 update
#               23 Mar 2009 - error trap par{}plot
#               13 Aug 2009 - browse button for map background file
#               16 Mar 2012 - file exists delete
#               08 Jun 2012 - display every Nth particle
#               12 Aug 2013 - particle age option
#               12 Sep 2016 - fixed browse button for shapefiles.txt
#               07 Nov 2016 - do not permit shapefile copy to itself
#               17 Jul 2017 - changed wording for Google Earth
#               17 Jul 2017 - added message box for ESRI conversion
#               28 Jul 2017 - use part_prog for initialization
#               26 Jul 2021 - use svg instead of postscript
#               31 May 2023 - correct GIS filenames by replacing _ps with _html.
#                           - create KMZ when selected
#-----------------------------------------------------------------------------

global Map_file part_numb
global poutf Zoom Psout_file part_prog html_dir 
global part_age part_size part_kolor part_gis

if [winfo exists .partdisp] {destroy .partdisp}
set wr .partdisp
toplevel $wr
wm title $wr " Particle Position Display   "
wm  geometry $wr +100+50 

frame $wr.top
frame $wr.mid0
frame $wr.mid1
frame $wr.mid2
frame $wr.mid3
frame $wr.mid4
frame $wr.mid5
frame $wr.mid6
frame $wr.bot
pack $wr.top  $wr.mid0 $wr.mid1 $wr.mid2 $wr.mid3 $wr.mid4 \
     $wr.mid5 $wr.mid6 $wr.bot -side top -pady 10 -padx 10

#-->information

label $wr.top.lab1 -fg blue -relief raised -justify left -wraplength 6i \
 -text "Display options for particle dump files. These are special\
files created through the Advanced Menu Tab that can also be used to\
initialize the model. Options exist to plot horizontal or vertical\
displays, or create a combination cross-section."
pack  $wr.top.lab1

#-->input file

label $wr.mid0.lab -text "Particle Position File: "
entry $wr.mid0.ent -textvariable poutf -relief sunken -width 15
pack  $wr.mid0.lab $wr.mid0.ent -side left

#-->output file

label $wr.mid1.lab -text "Output Postscript File: "
entry $wr.mid1.ent -textvariable Psout_file -relief sunken -width 15
pack  $wr.mid1.lab $wr.mid1.ent -side left -padx 5

#-->map background file

label $wr.mid2.lab -text "Map Background:"
entry $wr.mid2.ent -textvariable Map_file -relief sunken -width 30
button $wr.mid2.win  -text Browse -width 10 -command {
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
pack $wr.mid2.lab $wr.mid2.ent $wr.mid2.win -side left -padx 2

#-->program options

label $wr.mid3.lab -text "View:"
radiobutton $wr.mid3.d0 -text "Plane"     -variable part_prog -value "0" 
radiobutton $wr.mid3.d1 -text "Vertical"  -variable part_prog -value "1" 
radiobutton $wr.mid3.d2 -text "Global"    -variable part_prog -value "2" 
radiobutton $wr.mid3.d3 -text "X-Sect"    -variable part_prog -value "3" 
button $wr.mid3.d4  -text "Set Cross" -width 10 -command {set_cross}
pack $wr.mid3.lab $wr.mid3.d0 $wr.mid3.d1 $wr.mid3.d2 $wr.mid3.d3 $wr.mid3.d4 -padx 2 -side left

label $wr.mid4.lab -text "Display:"
checkbutton $wr.mid4.d1 -text "Mass" -variable part_size  -background grey 
checkbutton $wr.mid4.d2 -text "Color" -variable part_kolor -background grey
label $wr.mid4.lab1 -text "Age (h)"
entry $wr.mid4.ent1 -textvariable part_age -relief sunken -width 2
label $wr.mid4.lab2 -text "Every"
entry $wr.mid4.ent2 -textvariable part_numb -relief sunken -width 3
label $wr.mid4.lab3 -text "Nth part"
pack $wr.mid4.lab $wr.mid4.d1 $wr.mid4.d2 $wr.mid4.lab1 $wr.mid4.ent1 $wr.mid4.lab2 \
      $wr.mid4.ent2 $wr.mid4.lab3 -side left -padx 5

#-->GIS options

label $wr.mid5.lab -text "GIS output:"
radiobutton $wr.mid5.d1 -text "None (SVG only)" -variable part_gis -value 0
radiobutton $wr.mid5.d2 -text "ESRI Generate" -variable part_gis -value 1
radiobutton $wr.mid5.d3 -text "KML/KMZ" -variable part_gis -value 3
pack $wr.mid5.lab $wr.mid5.d1 $wr.mid5.d2 $wr.mid5.d3 -padx 2 -side left

#-->zoom factor slider bar      
label $wr.mid6.lab -text "Least Zoom -------------------> Most Zoom"
scale $wr.mid6.d0 -orient horizontal -length 400 -from 0 -to 100 \
  -tickinterval 10 -variable Zoom -resolution 5 
pack $wr.mid6.lab $wr.mid6.d0 -side top -pady 3

#-->bottom action buttons

button $wr.bot.dismiss  -bg red -text Quit -width 10 -command "destroy $wr"
button $wr.bot.help -text Help -width 8 \
       -command "load_html [file join $html_dir S332.htm ] "
button $wr.bot.save  -bg green -text "Execute Display " -width 20 -command {run_part_plot}
pack   $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 
set_default1
}

#----------------------------------------------------------------
# run plotting program

proc run_part_plot {} {

global Map_file part_numb
global Zoom poutf Psout_file part_prog 
global part_age part_size part_kolor part_gis
global exec_dir tcl_platform X_dir tcl_dir zip_pgm
global X_lat1 X_lon1 X_lat2 X_lon2

if [file exists ${Psout_file}.html] {file delete ${Psout_file}.html}
#if [file exists logocon.gif] {file delete logocon.gif}
#if [file exists noaa_google.gif] {file delete noaa_google.gif}
if [file exists HYSPLITpart.kmz] {file delete HYSPLITpart.kmz}
if {[string length ${Psout_file}] == 0} {
   set arg2 -:
} else {
   set arg2 -o  ; append arg2 ${Psout_file}.html
}

set arg0 -j  ; append arg0 $Map_file
set arg1 -i  ; append arg1 $poutf
set arg3 -m  ; append arg3 $part_size
set arg4 -k  ; append arg4 $part_kolor
set arg8 -n  ; append arg8 $part_numb 
set arg9 -t  ; append arg9 $part_age

if {$X_lat1 == ""} {
   set arg5 "-:"
   } else {
   set arg5 "-x$X_lat1,$X_lon1,$X_lat2,$X_lon2"
}

set arg6 -a  ; append arg6 $part_gis
set arg7 -z  ; append arg7 $Zoom

set arg_ext "+g1"

if { $part_prog == 0 } {set prog parhplot}
if { $part_prog == 1 } {set prog parvplot}
if { $part_prog == 2 } {set prog parsplot}
if { $part_prog == 3 } {set prog parxplot}

if { $part_gis > 0 } {set prog parhplot} 

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
   exec $exec_dir/$prog $arg_ext $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8 $arg9
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/$prog $arg_ext $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8 $arg9
   }
} else {
   if [catch {
      exec $exec_dir/$prog.exe $arg_ext $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8 $arg9} result] {
      msg_box "ERROR: particle display file not created!"
   }
}

# -->zip Google Earth logocon.gif and kml file to kmz file
if { $part_gis == 3 } {
    if { ![file exists logocon.gif] } {
       file copy $tcl_dir/logocon.gif logocon.gif
    }
    if { ![file exists noaa_google.gif] } {
       file copy $tcl_dir/noaa_google.gif noaa_google.gif
    }
    if [file exists HYSPLITpart_html.kml] {
       if [ file exists $zip_pgm ] {
            exec $zip_pgm HYSPLITpart.kmz HYSPLITpart_html.kml logocon.gif noaa_google.gif \
                          icon63.png particle.png
            file delete HYSPLITpart_html.kml
            if [file exists HYSPLITpart.kmz] {
               msg_box "KML/KMZ files created in working directory (HYSPLITpart.kmz)"
            } else {
               msg_box "KML/KMZ files have not been created.  Make sure Infozip has been installed and defined."
            }
       } else {
            msg_box "Infozip has not been installed or correctly defined in HYSPLIT. KML/KMZ files not created."
            file delete HYSPLITpart_html.kml
       }
    }
}


# -->ESRI Shapefiles
if { $part_gis == 1 } {
   foreach f [glob -nocomplain GIS_part_???_html.txt] {
      msg_box "Created file: $f"
      tkwait window .msg_win
   }
}
svg_box ${Psout_file}.html

}

#----------------------------------------------------------------
# set display defaults

proc set_default1 {} {

global maps_dir Map_file part_numb
global Zoom poutf Psout_file part_prog 
global part_age part_size part_kolor part_gis
global X_lat1 X_lon1 X_lat2 X_lon2

if [ info exists part_prog ] { } else {set part_prog ""}

if {$part_prog == ""} {
   set part_prog 3
   set X_lat1 ""
   set X_lon1 ""
   set X_lat2 ""
   set X_lon2 ""

   set part_age 0
   set part_numb 1
   set part_size 0
   set part_kolor 1
   set part_gis 0
   set Zoom 50
   if {$Map_file == ""} {set Map_file ${maps_dir}/arlmap}
   if {$poutf == ""} {set poutf "PARDUMP"}
}
set Psout_file "partplot"
}

#----------------------------------------------------------------
# force cross-section 

proc set_cross {} {
global X_lat1 X_lon1 X_lat2 X_lon2
if [winfo exists .setcross] {destroy .setcross}
set wx .setcross
toplevel $wx
wm title $wx "Set Cross Section Line"
wm  geometry $wx +100+100

frame $wx.title
frame $wx.left
frame $wx.right
frame $wx.end
pack $wx.title $wx.left $wx.right $wx.end -side top -pady 5

label $wx.title.lab1 -text "Set Left and Right Corner Points"
label $wx.title.lab2 -text "              Latitude   Longitude"
pack $wx.title.lab1 $wx.title.lab2 -side top 

label $wx.left.lab -text " Left point:"
entry $wx.left.lat -textvariable X_lat1 -width 10
entry $wx.left.lon -textvariable X_lon1 -width 10
pack $wx.left.lab $wx.left.lat $wx.left.lon -padx 2 -side left

label $wx.right.lab -text "Right point:"
entry $wx.right.lat -textvariable X_lat2 -width 10
entry $wx.right.lon -textvariable X_lon2 -width 10
pack $wx.right.lab $wx.right.lat $wx.right.lon -padx 2 -side left

button $wx.end.quit -text Reset -width 8 -command {
   set X_lat1 ""
   set X_lon1 ""
   set X_lat2 ""
   set X_lon2 ""
   }
button $wx.end.exit -text OK   -width 8 -command "destroy $wx"
pack $wx.end.quit $wx.end.exit -side left -padx 20
}
