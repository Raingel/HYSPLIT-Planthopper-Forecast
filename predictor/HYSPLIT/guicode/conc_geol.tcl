proc conc_geol { } {

#-----------------------------------------------------------------------------
# CONC_GEOL.TCL: Run multiple dispersion simulations by looping through
#                sequentially numbered CONTROL files created from a DATEM
#                formatted measured data file
#-----------------------------------------------------------------------------
# Last Revised: 31 Jul 2007 - initial version from auto_conc.tcl
#               14 Nov 2008 - renamed executables
#               07 Jan 2011 - replaced fixed binary input with variable name
#               26 Jul 2012 - option to create control with fixed emissions
#               28 Jul 2017 - added INFO on Timesum initialization
#               13 Dec 2019 - replace concplot and concplot.exe with concplot.py
#               17 Jan 2020 - add option to use FORTRAN or Python concplot.
#               26 Jul 2021 - use svg instead of postscript
#               04 Apr 2022 - add -81 option to concplot to create plot(s)
#                             even when all values are zero.
#               07 Nov 2023 - change street maps to OPEN_STREET and OPEN_TOPO.
#-----------------------------------------------------------------------------

global html_dir Month Day Timesum Zero Unit Wght
global PyDebug PySource_time_zone PyStreet_map PyOutput_format PyView
global PyPreferred

if [winfo exists .concgeol] {destroy .concgeol}
set wr .concgeol
toplevel $wr
wm title $wr " Automated Upwind Dispersion Simulations for GeoLocation "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.step1
frame $wr.stepa
frame $wr.step2
frame $wr.stepc
frame $wr.stepd
frame $wr.step3
frame $wr.step4
frame $wr.stepb
frame $wr.bot

ttk::notebook $wr.lang
frame $wr.lang.py
frame $wr.lang.f
$wr.lang add $wr.lang.f -text "Fortran"
$wr.lang add $wr.lang.py -text "Python"

pack $wr.top $wr.step1 $wr.stepa $wr.step2 $wr.stepc $wr.stepd $wr.step3 \
     $wr.step4 $wr.lang $wr.stepb $wr.bot -side top -pady 5 -padx 5
pack configure $wr.lang -expand true -fill x -padx 60

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 7i \
 -text "Execute a script to run multiple iterations of the upwind\
dispersion calculation for individual measured sampling data. The\
results are then overlaid to indicate the most probable source\
region. The base CONTROL file should have been previously defined\
as a forward calculation to encompass the sampling period. The\
measured data file must be in the DATEM format. The output is\
written to source.ps."
pack $wr.top.lab

#----------------------------------------------------------
# Step1 define the measured data file

label $wr.step1.lab -width 50 -bg Gray65 -anchor w \
      -text "Step 1: Define the measured data input file"
button $wr.step1.but  -text Browse -width 8 -command {
   set temp [tk_getOpenFile -title "File Selection"]
   if {[string length $temp] > 0} {set name_datem $temp}}
pack  $wr.step1.lab $wr.step1.but -side left -padx 5

entry $wr.stepa.ent -textvariable name_datem -relief sunken -width 60 
pack  $wr.stepa.ent


#-------------------------------------------------
# Step2 Create a control file for each measurement

label $wr.step2.lab -width 50 -bg Gray65 -anchor w \
      -text "Step 2: Create all the CONTROL files"
label $wr.step2.rgt -width 8 -text " "
pack  $wr.step2.lab $wr.step2.rgt -side left -padx 5

label  $wr.stepc.lab -width 23 -text "Unit Conversion:"
entry  $wr.stepc.ent -textvariable Unit -width 15
pack $wr.stepc.lab $wr.stepc.ent -side left -padx 2

radiobutton $wr.stepd.aa -text "Numerator    "  -variable Wght -value "0" -background grey
radiobutton $wr.stepd.bb -text "Inverse     "  -variable Wght -value "1" -background grey
radiobutton $wr.stepd.cc -text "Constant    "  -variable Wght -value "2" -background grey
button $wr.stepd.but  -text Execute -width 8 -command {make_input $name_datem}
pack $wr.stepd.aa $wr.stepd.bb $wr.stepd.cc $wr.stepd.but -side left -padx 5


#-------------------------------------
# Step3 Run the dispersion simulations

label  $wr.step3.lab -width 50 -bg Gray65 -anchor w \
       -text "Step 3: Run the dispersion simulations"
button $wr.step3.but  -text Execute -width 8 -command {run_geol}
pack   $wr.step3.lab $wr.step3.but -side left -padx 5


#-------------------------------------
# Step4 Display the simulation results

label $wr.step4.top -width 50 -bg Gray65 -anchor w \
      -text "Step 4: Show results from values set below"
label $wr.step4.rgt -width 8 -text " "
pack  $wr.step4.top $wr.step4.rgt -side left -padx 5

#-->Python options
label $wr.lang.f.lab -text "Output will be in Postscript"
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
checkbutton $wr.lang.py.misc.view -variable PyView -text "View" -background grey
checkbutton $wr.lang.py.misc.source_tz -variable PySource_time_zone -text "Source time zone" -background grey
checkbutton $wr.lang.py.misc.debug -variable PyDebug -text "Debug messages" -background grey
pack $wr.lang.py.misc.view $wr.lang.py.misc.source_tz $wr.lang.py.misc.debug -side left -padx 5

pack $wr.lang.py.row1 $wr.lang.py.misc -side top -pady 5 -padx 10

if { $PyPreferred } {
    $wr.lang select $wr.lang.py
} else {
    $wr.lang select $wr.lang.f
}
#-->end of Python options

label  $wr.stepb.top -width 18 -text "Time aggregate:"
entry  $wr.stepb.ent -width 2  -textvariable Timesum -relief sunken
label  $wr.stepb.lab -width 18 -text "Zero threshold:"
entry  $wr.stepb.val -width 9  -textvariable Zero -relief sunken
button $wr.stepb.but  -text Execute -width 8 -command {plot_geol ".concgeol.lang"}
pack   $wr.stepb.top $wr.stepb.ent $wr.stepb.lab $wr.stepb.val -side left -padx 2
pack   $wr.stepb.but -side left -padx 8


#----------------------
# bottom action buttons

button $wr.bot.dismiss  -bg red -text Quit -width 24 -command "destroy $wr"
button $wr.bot.help -text Help -width 24 \
       -command "load_html [file join $html_dir S357.htm ] "
label  $wr.bot.blank -text "   " -width 8
pack   $wr.bot.dismiss $wr.bot.help $wr.bot.blank -side left -padx 5

set_defaultg
}

#======================================================
proc make_input {fname} {

global X_dir exec_dir tcl_platform Grid_name
global Unit Wght

file copy -force default_conc CONTROL
if [file exists CONC.CFG] {file copy -force CONC.CFG SETUP.CFG}

set ctmp [lindex $Grid_name 0]

set d1 1
set d2 0
while {$d1 <= 999} {
   set DD [format "%3.3u" $d1]
   if [ file exists CONTROL.${DD} ] {file delete CONTROL.${DD}}
   if [ file exists MESSAGE.${DD} ] {file delete MESSAGE.${DD}}
   if [ file exists VMSDIST.${DD} ] {file delete VMSDIST.${DD}}
   if [ file exists ${ctmp}.${DD} ] {file delete ${ctmp}.${DD}}
   incr d1
}

set arg1 -i  ; append arg1 $fname
set arg2 -c  ; append arg2 $Unit
set arg3 -d  ; append arg3 $Wght

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
      exec $exec_dir/dat2cntl $arg1 $arg2 $arg3
   } else {
      exec $X_dir/xterm -fn fixed -e $exec_dir/dat2cntl $arg1 $arg2 $arg3
   }
} else {
  exec $exec_dir/dat2cntl.exe $arg1 $arg2 $arg3
  update
  msg_box " Calculations completed for file: $fname "
}

}

#======================================================
proc run_geol {} {

global X_dir exec_dir tcl_platform

set d1 1
set d2 0
while {$d1 <= 999} {
   set DD [format "%3.3u" $d1]
   if [ file exists CONTROL.${DD} ] {set d2 $d1}
   incr d1
}

if {$d2 == 0} {
   msg_box " No input files found between 001 and 999"   
   return
}

set log [ScrollText .f]
$log configure -cursor watch
$log insert end "Geolocation calculations started ...\n"
update  

set d1 1
while {$d1 <= $d2} {
   set DD [format "%3.3u" $d1]
   if [ file exists SETUP.CFG ] {file copy -force SETUP.CFG SETUP.$DD}

   if { "$tcl_platform(platform)" == "unix" } {
      if { "$X_dir" == "" } {
         exec $exec_dir/hycs_std ${DD}
      } else {
         exec $X_dir/xterm -fn fixed -e $exec_dir/hycs_std ${DD}
      }
   } else {
     exec $exec_dir/hycs_std.exe ${DD}
   }
   $log insert end "Finished simulation: $DD \n"
   update

   if [ file exists WARNING.$DD ] {file delete WARNING.$DD}
   if [ file exists VMDIST.$DD ]  {file delete VMDIST.$DD}
   if [ file exists SETUP.$DD ]   {file delete SETUP.$DD}
   if [ file exists CONC.$DD ]    {file delete CONC.$DD}
   incr d1
}
msg_box " Geolocation calculations completed! "
tkwait window .msg_win
}


#======================================================
proc plot_geol {lang} {

global Grid_name X_dir exec_dir tcl_platform Timesum Zero Wght
global PyPreferred
global PyDebug PySource_time_zone PyStreet_map PyOutput_format PyView

set PyPreferred [expr {[$lang select] == "${lang}.py"}]

if { $PyPreferred } {
    set out_file source.${PyOutput_format}
} else {
    set out_file source.html
}

set arg1 -b  ; append arg1 [lindex $Grid_name 0]
set arg2 -t  ; append arg2 $Timesum
set arg3 -v  ; append arg3 $Zero

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
   exec $exec_dir/conprob $arg1 $arg2 $arg3
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/conprob $arg1 $arg2 $arg3
   }
} else {
   exec $exec_dir/conprob.exe $arg1 $arg2 $arg3
}

set f [open "LABELS.CFG" w]
if { $Wght == 0 } {
    puts $f "'TITLE&','Weighted Source Sensitivity Function&'"
} else {
    puts $f "'TITLE&','Inverse of the Averaged Emissions&'"
}
puts $f "'MAPID&',' Receptor &'"
puts $f "'UNITS&',' mass &'" 
puts $f "'VOLUM&',' &'"
close $f

set arg1 -i ; append arg1 cmean 
set arg2 -o ; append arg2 $out_file
set arg3 -z ; append arg3 80
set arg4 -l ; append arg4 32

if { $PyPreferred } {
    set arg_ext " "
    set arg_ext2 "-81"
    if { $PyView } { append arg_ext " --interactive" }
    if { $PyDebug } { append arg_ext " --debug" }
    if { $PySource_time_zone } { append arg_ext " --source-time-zone" }

    switch -nocase $PyStreet_map {
        OPEN_STREET { append arg_ext " --street-map=0" }
        OPEN_TOPO { append arg_ext " --street-map=1" }
        default { }
    }

    run_python_program "$exec_dir/concplot.py" "$arg_ext $arg_ext2 \
                    $arg1 $arg2 $arg3 $arg4"
} else {
    set arg_ext "+g1"
    set arg_ext2 "-81"
    if { "$tcl_platform(platform)" == "unix" } {
        if { "$X_dir" == "" } {
            exec $exec_dir/concplot \
                    $arg_ext $arg_ext2 $arg1 $arg2 $arg3 $arg4
        } else {
            exec $X_dir/xterm -fn fixed -e $exec_dir/concplot \
                    $arg_ext $arg_ext2 $arg1 $arg2 $arg3 $arg4
        }
    } else {
            exec $exec_dir/concplot.exe \
                    $arg_ext $arg_ext2 $arg1 $arg2 $arg3 $arg4
    }
}

if [file exists LABELS.CFG] {file delete -force LABELS.CFG}
if { !$PyPreferred } { svg_box $out_file }

}


#------------------------------------------------------------------------------
# set initial defaults

proc set_defaultg {} {

global Timesum Zero Unit Wght
global PyDebug PySource_time_zone PyStreet_map PyOutput_format PyView

if [ info exists Timesum ] { } else {set Timesum ""}

if { $Timesum == "" } {
   set Timesum 1
   set Zero 0.0
   set Unit 1.0   
   set Wght 0
   set PyDebug false
   set PySource_time_zone false
   set PyStreet_map NOT_USED
   set PyOutput_format pdf
   set PyView false
   }
}
