proc traj_freq {} {

#-----------------------------------------------------------------------------
# TRAJ_FREQ.TCL: create trajectory frequency plots  
# Last Revised: 03 May 2007 - initial version
#               24 Nov 2008 - more grid size options
#               13 Aug 2009 - browse button for map background file
#               23 Sep 2009 - minor correction to window name
#               05 Nov 2010 - save processing steps to stdout window
#               22 Nov 2010 - residence time and layer select
#               08 Sep 2016 - button text color changed to white
#               12 Sep 2016 - corrected browse for shapefiles.txt
#               07 Nov 2016 - do not permit shapefile copy to itself
#               28 Jul 2017 - revised initialization test
#               14 Aug 2018 - automatically open plot; force dates to zero
#               13 Dec 2019 - replace concplot and concplot.exe with concplot.py
#               16 Jan 2020 - add option to use FORTRAN or Python concplot.
#               26 Jul 2021 - use svg instead of postscript
#               04 Apr 2022 - add -81 option to concplot to create plot(s)
#                             even when all values are zero.
#               07 Nov 2023 - change street maps to OPEN_STREET and OPEN_TOPO.
#-----------------------------------------------------------------------------

global html_dir
global Tfreq_file Gsize Infile
global Psout_file Frame Igis View Resid Bot Top
global Map_file Color Map_proj Zoom 
global PyDebug PySource_time_zone PyStreet_map PyOutput_format
global PyPreferred

if [winfo exists .tfreq] {destroy .tfreq}
set wr .tfreq
toplevel $wr
wm title $wr " Create a Trajectory Frequency Plot  "
wm  geometry $wr +100+25

frame $wr.top  
frame $wr.mid0
frame $wr.mid1
frame $wr.mid2
frame $wr.mid3
frame $wr.mid4
frame $wr.mid6
frame $wr.mid7
frame $wr.mid8
frame $wr.mid9
frame $wr.end
frame $wr.bot

ttk::notebook $wr.lang
frame $wr.lang.py
frame $wr.lang.f
$wr.lang add $wr.lang.f -text "Fortran"
$wr.lang add $wr.lang.py -text "Python"

pack $wr.top  $wr.mid0 -pady 4
pack $wr.mid1 $wr.mid2 $wr.mid3 $wr.mid4 -pady 2
pack $wr.mid6 $wr.mid7 $wr.mid8 -pady 2 
pack $wr.mid9 $wr.lang $wr.end $wr.bot -pady 2
pack configure $wr.lang -expand true -fill x -pady 2 -padx 20

#-->description

label $wr.top.lab -fg blue -relief raised -justify left -wraplength 5i \
 -text "Reads multiple trajectory output files as defined by INFILE and\
 creates a binary gridded file that represents the trajectory frequency\
 at each grid point. Trajectories can be created from the Run Daily menu."
pack $wr.top.lab

#-->input file of file names

button $wr.mid0.mak -text "Create file of trajectory filenames" -width 38 \
       -bg yellow -command Make_FILE
label  $wr.mid0.lab -text " Base Name:"
entry  $wr.mid0.ent -textvariable Infile -relief sunken -width 15
pack   $wr.mid0.mak $wr.mid0.lab $wr.mid0.ent -side left -padx 2

#-->gridded binary output data file

label $wr.mid1.lab -text "                      Frequency binary output file:"
entry $wr.mid1.ent -textvariable Tfreq_file -relief sunken -width 15
pack  $wr.mid1.lab $wr.mid1.ent -side left -padx 2 

label $wr.mid2.lab -text " Set Grid Resolution (deg): " 
radiobutton $wr.mid2.d1 -text ".25" -variable Gsize -value "0.25" 
radiobutton $wr.mid2.d2 -text ".50" -variable Gsize -value "0.50" 
radiobutton $wr.mid2.d3 -text "1.0" -variable Gsize -value "1.0" 
radiobutton $wr.mid2.d4 -text "2.0" -variable Gsize -value "2.0" 
radiobutton $wr.mid2.d5 -text "4.0" -variable Gsize -value "4.0" 
pack $wr.mid2.lab -side left -padx 10
pack $wr.mid2.d1 $wr.mid2.d2 $wr.mid2.d3 $wr.mid2.d4 $wr.mid2.d5 -side left

#-->gridding method

label $wr.mid3.lab1 -text "Residence time:"
radiobutton $wr.mid3.d0 -text "No" -variable Resid -value "0" 
radiobutton $wr.mid3.d1 -text "Yes" -variable Resid -value "1" 
label $wr.mid3.lab2 -text "     Layer Bottom:"
entry $wr.mid3.ent1 -textvariable Bot -relief sunken -width 6
label $wr.mid3.lab3 -text "  Top:"
entry $wr.mid3.ent2 -textvariable Top -relief sunken -width 6
pack $wr.mid3.lab1 $wr.mid3.d0 $wr.mid3.d1 $wr.mid3.lab2 $wr.mid3.ent1 \
     $wr.mid3.lab3 $wr.mid3.ent2 -side left

label $wr.mid4.lab -text "-------------------------------------------------------------"
pack  $wr.mid4.lab

#-->output postscript file

label $wr.mid6.lab -text "Output File: "
entry $wr.mid6.ent -textvariable Psout_file -relief sunken -width 15
checkbutton $wr.mid6.box -variable Frame -text "Frames" -background grey
checkbutton $wr.mid6.gis -variable Igis -text "GIS" -background grey
checkbutton $wr.mid6.psv -variable View -text "View" -background grey
checkbutton $wr.mid6.kol -variable Color -text Color -background grey
pack $wr.mid6.lab $wr.mid6.ent $wr.mid6.box $wr.mid6.gis $wr.mid6.psv \
     $wr.mid6.kol -side left -padx 5 

#-->map background file

label  $wr.mid7.lab -text "Map Background: "
entry  $wr.mid7.ent -textvariable Map_file -relief sunken -width 35
button $wr.mid7.win  -text Browse -width 8 -command {
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
pack $wr.mid7.lab $wr.mid7.ent $wr.mid7.win -side left -padx 5

#-->map projection options

label $wr.mid8.lab -text "   Projection: "
radiobutton $wr.mid8.d0 -text "Auto"     -variable Map_proj -value "0" 
radiobutton $wr.mid8.d1 -text "Polar"    -variable Map_proj -value "1" 
radiobutton $wr.mid8.d2 -text "Lambert"  -variable Map_proj -value "2" 
radiobutton $wr.mid8.d3 -text "Mercator" -variable Map_proj -value "3" 
pack $wr.mid8.lab -side left -padx 10
pack $wr.mid8.d0 $wr.mid8.d1 $wr.mid8.d2 $wr.mid8.d3 -side left

#-->zoom factor slider bar

label $wr.mid9.lab -text "Least Zoom -------------------> Most Zoom"
scale $wr.mid9.d0 -orient horizontal -length 400 -from 0 -to 100 \
  -tickinterval 10 -variable Zoom -resolution 5 
pack $wr.mid9.lab $wr.mid9.d0 -side top -pady 1

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

label $wr.end.lab -text "-------------------------------------------------------------"
pack  $wr.end.lab

button $wr.bot.dismiss  -bg red -text Quit -width 14 -command "destroy $wr"
button $wr.bot.help -text Help -width 10 \
       -command "load_html [file join $html_dir S231.htm ] "
button $wr.bot.save  -bg green -text "Execute Display " -width 26 -command {run_freq_plot ".tfreq.lang"}
pack $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 
set_tfreq
}

#------------------------------------------------------------------------------
# run plotting program

proc run_freq_plot {lang} {

global X_dir tcl_platform exec_dir 
global Tfreq_file Gsize
global Psout_file Frame Igis View Resid Bot Top
global Map_file Color Map_proj Zoom 
global PyPreferred
global PyDebug PySource_time_zone PyStreet_map PyOutput_format

set PyPreferred [expr {[$lang select] == "${lang}.py"}]

if { $PyPreferred } {
    set out_file ${Psout_file}.${PyOutput_format}
} else {
    set out_file ${Psout_file}.html
}

set arg1 -f  ; append arg1 $Tfreq_file
set arg2 -g  ; append arg2 $Gsize
set arg3 -i  ; append arg3 INFILE
set arg4 -r  ; append arg4 $Resid
set arg5 -s  ; append arg5 ${Bot}:${Top}
set arg6 -b  ; append arg6 000000000001

set log [ScrollText .msg]
$log configure -cursor watch
$log insert end "Gridding: trajfreq $arg1 $arg2 $arg3 $arg4 $arg5 $arg6\n"
update

if [ file exists STDOUT ] {file delete STDOUT} 
if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
   exec $exec_dir/trajfreq $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 >STDOUT
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/trajfreq $arg1 $arg2 $arg3 $arg4 $arg5 $arg6
   }
} else {
   exec $exec_dir/trajfreq.exe $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 >STDOUT
}

if [file exists STDOUT] {
   set fid [open "STDOUT" r]
   while {[eof $fid] != 1} {
      gets $fid cline
      $log insert end $cline\n
   }
   close $fid
   file delete STDOUT
}
# tkwait window .msg

if [file exists $Tfreq_file] { } else {
   destroy .tfreq
   return}

set f [open "LABELS.CFG" w]
puts $f "'TITLE&','Trajectory Frequency&'"
puts $f "'MAPID&',' Values &'"
puts $f "'UNITS&',' %&'" 
puts $f "'VOLUM&',' &'"
close $f

set arg1 -i  ; append arg1 $Tfreq_file
set arg2 -o  ; append arg2 $out_file
set arg3 -j  ; append arg3 $Map_file
set arg4 -m  ; append arg4 $Map_proj
set arg5 -k  ; append arg5 $Color
set arg6 -z  ; append arg6 $Zoom
set arg7 -a  ; append arg7 $Igis

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
                        $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7"
} else {
    set arg_ext "+g1"
    set arg_ext2 "-81"
    if { "$tcl_platform(platform)" == "unix" } {
        if { "$X_dir" == "" } {
            exec $exec_dir/concplot $arg_ext $arg_ext2 \
                        $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7
        } else {
            exec $X_dir/xterm -fn fixed -e $exec_dir/concplot $arg_ext $arg_ext2 \
                        $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7
        }
    } else {
        exec $exec_dir/concplot.exe $arg_ext $arg_ext2 \
                        $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7
    }
}

if [file exists LABELS.CFG] {file delete -force LABELS.CFG}
if { !$PyPreferred && $View == 1 } {svg_box $out_file}
}


#------------------------------------------------------------------------------
# set initial defaults

proc set_tfreq {} {

  global Tfreq_file Gsize Infile Trajpts_file
  global Psout_file Frame Igis View Resid Bot Top
  global maps_dir Map_file Color Map_proj Zoom 
  global PyDebug PySource_time_zone PyStreet_map PyOutput_format

  set Igis 0
  set View 1
  set Color 1
  set Psout_file freqplot
  set Infile [file tail $Trajpts_file]

  if [ info exists Tfreq_file ] { } else {set Tfreq_file ""}
  if { $Tfreq_file == "" } {
     set Tfreq_file tfreq.bin
     set Gsize 1.0
     if { $Map_file == "" } {set Map_file ${maps_dir}/arlmap} 
     set Map_proj 0
     set Zoom 50
     set Frame 0
     set Resid 0
     set Bot 0
     set Top 99999
     set PyDebug false
     set PySource_time_zone false
     set PyStreet_map NOT_USED
     set PyOutput_format pdf
  }
}

#------------------------------------------------------------------------------
# create file listing

proc Make_FILE {} {

  global Infile

  if [file exists INFILE] {file delete -force INFILE}
  set In_file INFILE
  set g [open $In_file w]
  foreach f [glob *${Infile}*] {puts $g $f}
  close $g
  msg_box " ${In_file} created "

}
