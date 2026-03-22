proc conc_grid {} {

#------------------------------------------------------------------------------
# CONC_GRID.TCL: color fill plot of concentration grid which is especially
# good to display results on global spanning grids
# Last Revised: 27 Nov 2007 - initial version 
#               06 Nov 2008 - version 4.9 compatibility
#               07 Aug 2017 - DOS CR/LF incompatible on unix
#               26 Jul 2021 - use svg instead of postscript
#               27 Jul 2021 - remove ghostscript and ghostview dependencies.
#               31 May 2023 - copy logocon.gif and noaa_google.gif when needed.
#------------------------------------------------------------------------------

global Conbase Condelt Psout_file Frame Num_polid Log_scale Map_delta Map_zoom
global Map_lat Conmult Conunits Igis html_dir Grid_name Num_disp zip_pgm 
if [winfo exists .congrid] {destroy .congrid}
set wr .congrid
toplevel $wr
wm title $wr " Color Fill of the Concentration Grid "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.mid1
frame $wr.mid2
frame $wr.mid3
frame $wr.mid4
frame $wr.mid5
frame $wr.mid6
frame $wr.mid7
frame $wr.mid8
frame $wr.mid9
frame $wr.bot
pack $wr.top $wr.mid1 $wr.mid2 $wr.mid3 $wr.mid4 $wr.mid5 $wr.mid6 \
     $wr.mid7 $wr.mid8 $wr.mid9 $wr.bot -side top -pady 5 -padx 10

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "Graphic that color fills each cell of the concentration grid\
with one of 12 colors. The current version is restricted to one pollutant\
species. A -1 for the Contour base will allow the program to determine the\
maximum contour and then increment down by the contour delta."
pack $wr.top.lab

#-->select grid

label $wr.mid1.lab -text "Grid To Display: "
pack $wr.mid1.lab -side left -padx 5
set d 0
foreach item $Grid_name {
   radiobutton $wr.mid1.$d -variable Num_disp -text $item -value $d -background grey
   pack $wr.mid1.$d -side left -padx 2
   incr d
}

#-->output file

label $wr.mid2.lab -text "Output SVG File: "
entry $wr.mid2.ent -textvariable Psout_file -relief sunken -width 15
pack  $wr.mid2.lab $wr.mid2.ent -side left -padx 5

#-->contouring

label $wr.mid3.lab1 -text "Contour base:"
entry $wr.mid3.ent1 -textvariable Conbase -relief sunken -width 10
label $wr.mid3.lab2 -text "   Contour delta:"
entry $wr.mid3.ent2 -textvariable Condelt -relief sunken -width 10
pack $wr.mid3.lab1 $wr.mid3.ent1 $wr.mid3.lab2 $wr.mid3.ent2 -side left -padx 1

#-->display options

label $wr.mid4.lab -text "Display Options:"
checkbutton $wr.mid4.box -variable Frame     -text "Frames   " -background grey
checkbutton $wr.mid4.kol -variable Log_scale -text "Log Scale" -background grey
pack $wr.mid4.lab $wr.mid4.box $wr.mid4.kol -side left -padx 5

label $wr.mid5.lab1 -text "Pollutant:"
entry $wr.mid5.ent1 -textvariable Num_polid -relief sunken -width 2
label $wr.mid5.lab2 -text "   Units:"
entry $wr.mid5.ent2 -textvariable Conunits  -relief sunken -width 8
pack $wr.mid5.lab1 $wr.mid5.ent1 $wr.mid5.lab2 $wr.mid5.ent2 -side left -padx 5

label $wr.mid6.lab1 -text "   Center latitude:"
entry $wr.mid6.ent1 -textvariable Map_lat -relief sunken -width 8
label $wr.mid6.lab2 -text "   Offset longitude:"
entry $wr.mid6.ent2 -textvariable Map_delta  -relief sunken -width 8
pack $wr.mid6.lab1 $wr.mid6.ent1 $wr.mid6.lab2 $wr.mid6.ent2 -side left -padx 5

label $wr.mid7.lab1 -text "Zoom:"
entry $wr.mid7.ent1 -textvariable Map_zoom -relief sunken -width 5
label $wr.mid7.lab2 -text "   Conc multiplier:"
entry $wr.mid7.ent2 -textvariable Conmult  -relief sunken -width 8
pack $wr.mid7.lab1 $wr.mid7.ent1 $wr.mid7.lab2 $wr.mid7.ent2 -side left -padx 5

#-->GIS/Google Earth options
label $wr.mid8.lab -text "    GIS Output:"
radiobutton $wr.mid8.d0 -text "None" -variable Igis -value "0"
radiobutton $wr.mid8.d1 -text "ESRI Generate   "   -variable Igis -value "1"
radiobutton $wr.mid8.d3 -text "Google Earth"  -variable Igis -value "3"
pack $wr.mid8.lab $wr.mid8.d0 $wr.mid8.d1 $wr.mid8.d3 -side left

#-->termination
label $wr.mid9.lab \
-text "________________________________________________________________"
pack $wr.mid9.lab -side left

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help -width 8 \
       -command "load_html [file join $html_dir S338.htm ] "
button $wr.bot.save  -bg green -text "Execute Display" -width 20 -command {run_cgrid}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10
gbldisp_set
}

proc gbldisp_set {} {
global Conbase Condelt Psout_file Frame Num_polid Log_scale Map_delta Map_zoom
global Map_lat Conmult Conunits Igis

set Psout_file gridplot

set Igis 0
if {$Conbase == ""} {
   set Conbase -1
   set Condelt 10.0
   set Conmult 1.0
   set Conunits mass
   set Frame 0
   set Num_polid 1
   set Log_scale 1
   set Map_delta 0.0
   set Map_lat 0.0
   set Map_zoom 0
}
}


#------------------------------------------------------------
# run plotting program

proc run_cgrid {} {

global Conbase Condelt Psout_file Frame Num_polid Log_scale Map_delta Map_zoom
global Map_lat Conmult Conunits X_dir Grid_dir Grid_name Num_disp tcl_platform
global exec_dir Igis tcl_dir zip_pgm magick_pgm

set Grid_dir_n [lindex $Grid_dir $Num_disp]
set Grid_name_n  [lindex $Grid_name $Num_disp]

#if [file exists logocon.gif] {file delete logocon.gif}
#if [file exists noaa_google.gif] {file delete noaa_google.gif}
if [file exists gridplot.kmz] {file delete gridplot.kmz}
if [file exists gridplot.kml] {file delete gridplot.kml}

set arg1 -i  ; append arg1 $Grid_dir_n$Grid_name_n
set arg2 -l  ; append arg2 $Conbase
set arg3 -d  ; append arg3 $Condelt
set arg4 -o  ; append arg4 $Psout_file
set arg5 -m  ; append arg5 $Frame
set arg6 -s  ; append arg6 $Num_polid
set arg7 -a  ; append arg7 $Log_scale
set arg8 -x  ; append arg8 $Map_delta
set arg9 -z  ; append arg9 $Map_zoom
set arg10 -c ; append arg10 $Conmult
set arg11 -u ; append arg11 $Conunits
set arg12 -y  ; append arg12 $Map_lat
set arg13 -g  ; append arg13 $Igis

set arg_ext "+g1"

if { $Igis == 1 } {set arg5 -m1 ; set Frame 1}
if { $Igis == "" } {set arg13 -g0 ; set Igis 0}

if { "$tcl_platform(platform)" == "unix" } {

   if { "$X_dir" == "" } {
   exec $exec_dir/gridplot $arg_ext \
        $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8 $arg9  \
        $arg10 $arg11 $arg12 $arg13
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/gridplot $arg_ext \
        $arg1 $arg2 $arg3 \
        $arg4 $arg5 $arg6 $arg7 $arg8 $arg9 $arg10 $arg11 $arg12 $arg13
   }
} else {
   exec $exec_dir/gridplot.exe $arg_ext \
        $arg1 $arg2 $arg3 \
        $arg4 $arg5 $arg6 $arg7 $arg8 $arg9 $arg10 \
        $arg11 $arg12 $arg13
}

# -->zip Google Earth logocon.gif and kml file to kmz file
if { $Igis == 3 } {
   if { ![file exists logocon.gif] } {
      file copy $tcl_dir/logocon.gif logocon.gif
   }
   if { ![file exists noaa_google.gif] } {
      file copy $tcl_dir/noaa_google.gif noaa_google.gif
   }

   if [ file exists $zip_pgm ] {
      exec $zip_pgm gridplot.kmz gridplot.kml icon63.png logocon.gif noaa_google.gif
      file delete gridplot.kml
   } else {
      msg_box "Infozip has not been installed or correctly defined in HYSPLIT.  Google Earth kmz file not created."
   }

  if [ file exists $magick_pgm ] {
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
               if [file exists $zip_pgm] {exec $zip_pgm -u gridplot.kmz $Fileout}
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
  } else {
     msg_box "ImageMagick Required: $magick_pgm"}
}

if { $Frame == 1 } {
   if [file exists ${Psout_file}_001.html] {
      foreach f [glob ${Psout_file}_???.html] {
         svg_box $f
      }
   } else {
      msg_box "${Psout_file}_001.html not found!" 
   }
} else {
   if [file exists ${Psout_file}.html] {
      svg_box ${Psout_file}.html
   } else {
      msg_box "${Psout_file}.html not found!" 
   }
}
}
