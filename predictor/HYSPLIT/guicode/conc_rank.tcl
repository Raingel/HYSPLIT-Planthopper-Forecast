proc conc_rank { } {

#-----------------------------------------------------------------------------
# CONC_RANK.TCL: After running the matrix option, this script extracts each
#                of the source locations from the CONTROL file; uses c2datem
#                to create a corresponding DATEM formatted file with the
#                source location in the header. The statmain program is then
#                run to create a statistical summary file for each release
#                location (measurement data is required!). Then stat2grid is
#                invoked to plot a map of the selected statistic.
#-----------------------------------------------------------------------------
# Last Revised: 04 Nov 2011 - initial version from conc_geol.tcl
#               18 Apr 2013 - support new gridplot
#               29 Jan 2015 - initialization for each variable if not defined
#               25 Jan 2016 - minor color change to buttons
#               28 Jul 2017 - used kstat for initialization flag
#               26 Jul 2021 - use svg instead of postscript
#-----------------------------------------------------------------------------

global html_dir name_datem name_stats name_plot kstat Cscale

if [winfo exists .concrank] {destroy .concrank}
set wr .concrank
toplevel $wr
wm title $wr " Statistical Source Location Analysis from Matrix "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.step1
frame $wr.stepa
frame $wr.step2
frame $wr.stepb
frame $wr.step3
frame $wr.stepc
frame $wr.step4
frame $wr.stepd
frame $wr.step5
frame $wr.bot
pack $wr.top $wr.step1 $wr.stepa $wr.step2 $wr.stepb $wr.step3 $wr.stepc \
             $wr.step4 $wr.stepd $wr.step5 $wr.bot -side top -pady 5 -padx 5

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 5i \
 -text "This procedure is intended to find the source location that\
  gives the best fit with measurement data. After running matrix,\
  this script extracts each source location from the CONTROL file,\
  converts the output to DATEM format, calls the statistical\
  anlaysis program and plots the results by source location."
pack $wr.top.lab

#----------------------------------------
# Step1 define the measured data file

label $wr.step1.lab -width 50 -bg Gray65 -anchor w \
      -text "Step 1: Set the measured data file and conversion"
button $wr.step1.but  -text Browse -width 8 -command {
   set temp [tk_getOpenFile -title "File Selection"]
   if {[string length $temp] > 0} {set name_datem $temp}}
pack  $wr.step1.lab $wr.step1.but -side left -padx 5

entry $wr.stepa.ent -textvariable name_datem -relief sunken -width 50
entry $wr.stepa.cnv -textvariable Cscale -relief sunken -width 8
pack  $wr.stepa.ent $wr.stepa.cnv -side left -padx 5

#----------------------------------------
# Step2 define the statistical summary text file

label $wr.step2.lab -width 50 -bg Gray65 -anchor w \
      -text "Step 2: Set the statistical summary text file"
button $wr.step2.but  -text Browse -width 8 -command {
   set temp [tk_getOpenFile -title "File Selection"]
   if {[string length $temp] > 0} {set name_stats $temp}}
pack  $wr.step2.lab $wr.step2.but -side left -padx 5

entry $wr.stepb.ent -textvariable name_stats -relief sunken -width 60 
pack  $wr.stepb.ent


#---------------------------------------
# Step3 define the statistical summary graphics file

label $wr.step3.lab -width 50 -bg Gray65 -anchor w \
      -text "Step 3: Set the statistical graphics output file"
button $wr.step3.but  -text Browse -width 8 -command {
   set temp [tk_getOpenFile -title "File Selection"]
   if {[string length $temp] > 0} {set name_plot $temp}}
pack  $wr.step3.lab $wr.step3.but -side left -padx 5

entry $wr.stepc.ent -textvariable name_plot -relief sunken -width 60 
pack  $wr.stepc.ent


#--------------------------------------
# Step4 process all source locations

label $wr.step4.lab -width 50 -bg Gray65 -anchor w \
      -text "Step 4: Process data files for each source"
button $wr.step4.but  -bg green -text Execute -width 8 -command {run_source}
pack  $wr.step4.lab $wr.step4.but -side left -padx 5

label $wr.stepd.lab -anchor w -text "Statistic:"
radiobutton $wr.stepd.d0 -text "Corr" -variable kstat -value "3" 
radiobutton $wr.stepd.d1 -text "NMSE" -variable kstat -value "4"
radiobutton $wr.stepd.d2 -text   "FB" -variable kstat -value "5"
radiobutton $wr.stepd.d3 -text  "FMS" -variable kstat -value "6"
radiobutton $wr.stepd.d4 -text  "KSP" -variable kstat -value "7"
radiobutton $wr.stepd.d5 -text "Rank" -variable kstat -value "8"
label $wr.stepd.rgt -width 8 -text " "
pack $wr.stepd.lab $wr.stepd.d0 $wr.stepd.d1 $wr.stepd.d2 \
     $wr.stepd.d3  $wr.stepd.d4 $wr.stepd.d5 $wr.stepd.d1 \
     $wr.stepd.rgt -side left

#-------------------------------------
# Step5 Run the analysis

label  $wr.step5.lab -width 50 -bg Gray65 -anchor w \
       -text "Step 5: Select statistic and create source map"
button $wr.step5.but  -bg green -text Execute -width 8 -command {run_plot}
pack   $wr.step5.lab $wr.step5.but -side left -padx 5

#--------------------------------------
# bottom action buttons

button $wr.bot.dismiss  -bg red -text Quit -width 24 -command "destroy $wr"
button $wr.bot.help -text Help -width 24 \
       -command "load_html [file join $html_dir S355.htm ] "
label  $wr.bot.blank -text "   " -width 8
pack   $wr.bot.dismiss $wr.bot.help $wr.bot.blank -side left -padx 5

set_defaultr
}

#======================================================
proc run_source {} {

global name_datem name_stats name_plot kstat Cscale
global X_dir exec_dir tcl_platform Grid_name

if [ file exists hysplit_datem.txt ] {file delete hysplit_datem.txt}
if [ file exists SRM_cdump ] {file delete SRM_cdump}
if [ file exists stat_map.bin ] {file delete stat_map.bin}
if [ file exists $name_stats ] {file delete $name_stats}

set f [open "CONTROL" r]

gets $f Start_time
gets $f Num_Start_location
for { set i 1} { $i <=$Num_Start_location} {incr i} {
    gets $f Start_loc
    set site [string trim $Start_loc]
    set slat [lindex $site 0]
    set slon [lindex $site 1]

#   use matrix to extract for source location
    set arg1 -i ; append arg1 [lindex $Grid_name 0]
    set arg2 -o ; append arg2 SRM_cdump
    set arg3 -y ; append arg3 $slat
    set arg4 -x ; append arg4 $slon
    set arg5 -ms
    if { "$tcl_platform(platform)" == "unix" } {
       if { "$X_dir" == "" } {
       exec $exec_dir/matrix $arg1 $arg2 $arg3 $arg4 $arg5
       } else {
       exec $X_dir/xterm -fn fixed -e $exec_dir/matrix $arg1 $arg2 $arg3 $arg4 $arg5
       }
    } else {
       exec $exec_dir/matrix $arg1 $arg2 $arg3 $arg4 $arg5
    }

#   convert binary output file to DATEM text format
    set arg1 -c ; append arg1 $Cscale
    set arg2 -i ; append arg2 SRM_cdump
    set arg3 -m ; append arg3 $name_datem
    set arg4 -o ; append arg4 hysplit_datem.txt
    set arg5 -s
    if { "$tcl_platform(platform)" == "unix" } {
       if { "$X_dir" == "" } {
       exec $exec_dir/c2datem $arg1 $arg2 $arg3 $arg4 $arg5
       } else {
       exec $X_dir/xterm -fn fixed -e $exec_dir/c2datem $arg1 $arg2 $arg3 $arg4 $arg5
       }
    } else {
       exec $exec_dir/c2datem $arg1 $arg2 $arg3 $arg4 $arg5
    }

#   compute statistics and append to file
    set arg1 -l ; append arg1 10
    set arg2 -t ; append arg2 0
    set arg3 -d ; append arg3 $name_datem
    set arg4 -r ; append arg4 hysplit_datem.txt
    set arg5 -s ; append arg5 $name_stats
    if { "$tcl_platform(platform)" == "unix" } {
       if { "$X_dir" == "" } {
       exec $exec_dir/statmain $arg1 $arg2 $arg3 $arg4 $arg5
       } else {
       exec $X_dir/xterm -fn fixed -e $exec_dir/statmain $arg1 $arg2 $arg3 $arg4 $arg5
       }
    } else {
       exec $exec_dir/statmain $arg1 $arg2 $arg3 $arg4 $arg5
    }

#   cleanup
    if [ file exists SRM_cdump ] {file delete SRM_cdump}
    if [ file exists hysplit_datem.txt ] {file delete hysplit_datem.txt}
}
close $f

msg_box " Source location processing complete! "
tkwait window .msg_win
}


#======================================================
proc run_plot {} {

global name_plot name_stats kstat
global X_dir exec_dir tcl_platform

# convert statistical file to binary for plotting
set arg1 -i  ; append arg1 $name_stats
set arg2 -o  ; append arg2 stat_map.bin
set arg3 -v  ; append arg3 $kstat

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
   exec $exec_dir/stat2grid $arg1 $arg2 $arg3
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/stat2grid $arg1 $arg2 $arg3
   }
} else {
   exec $exec_dir/stat2grid $arg1 $arg2 $arg3
}

# configure plot label
set f [open LABELS.CFG w]
if { "$kstat" == "3" } {puts $f "'TITLE&','Spatial Statistical Results for CC&'"}
if { "$kstat" == "4" } {puts $f "'TITLE&','Spatial Statistical Results for NMSE&'"}
if { "$kstat" == "5" } {puts $f "'TITLE&','Spatial Statistical Results for FB&'"}
if { "$kstat" == "6" } {puts $f "'TITLE&','Spatial Statistical Results for FMS&'"}
if { "$kstat" == "7" } {puts $f "'TITLE&','Spatial Statistical Results for KSP&'"}
if { "$kstat" == "8" } {puts $f "'TITLE&','Spatial Statistical Results for RANK/4&'"}
puts $f "'UNITS&',' &'"
puts $f "'VOLUM&',' &'"
close $f

# plot graphic
set arg1 -i ; append arg1 stat_map.bin
set arg2 -o ; append arg2 $name_plot
set arg3 -l ; append arg3 0.0
set arg4 -d
if { "$kstat" == "3" } {append arg4 0.05} 
if { "$kstat" == "4" } {append arg4 5.00}  
if { "$kstat" == "5" } {append arg4 0.20}  
if { "$kstat" == "6" } {append arg4 5.00}  
if { "$kstat" == "7" } {append arg4 5.00} 
if { "$kstat" == "8" } {
   append arg4 0.10
   set arg6 -c ; append arg6 0.25 
} else {
   set arg6 -c ; append arg6 1.0
}
set arg5 -a ; append arg5 0 

set arg_ext "+g1"

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
      exec $exec_dir/gridplot $arg_ext $arg1 $arg2 $arg3 $arg4 $arg5 $arg6
   } else {
      exec $X_dir/xterm -fn fixed -e $exec_dir/gridplot $arg_ext $arg1 $arg2 $arg3 $arg4 $arg5 $arg6
   }
} else {
   exec $exec_dir/gridplot.exe $arg_ext $arg1 $arg2 $arg3 $arg4 $arg5 $arg6
}

file delete LABELS.CFG
svg_box ${name_plot}.html
}


#------------------------------------------------------------------------------
# set initial defaults

proc set_defaultr {} {

global name_datem name_stats name_plot kstat Cscale

if [ info exists kstat ] { } else {set kstat ""}

if { $kstat == "" } {
   set kstat 8
   if { $Cscale == "" } {set Cscale 1.0E+12}
   if { $name_datem == "" } {set name_datem measured.txt}
   if { $name_stats == "" } {set name_stats sumstat.txt}
   if { $name_plot == "" }  {set name_plot  statmap}
   }
}
