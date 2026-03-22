proc datem {} {
#------------------------------------------------------------------------------
# DATEM.TCL: convert HYSPLIT output to DATEM format and compare to measurements
# Last Revised: 25 Sep 2009 - initial version
#               11 Dec 2009 - added rotation angle
#               11 Jul 2011 - standardize variable name
#               17 Apr 2013 - force input file name
#               07 Jul 2014 - correct force name entry for multiple inputs
#               19 Jul 2017 - option to rename statistical output files
#               27 Jul 2017 - interpolation option
#		21 Mar 2018 - changed init test from file name to Angle
#               15 Aug 2018 - carried file rename through for input to scatter
#------------------------------------------------------------------------------

global Grid_dir Grid_name Num_disp Force_name
global Interp Stat_val Stat_avg Stat_lev Angle Stat_out
global Cscale name_datem Model_file 
global html_dir 

if [winfo exists .datem] {destroy .datem}
set wr .datem
toplevel $wr
wm title $wr " DATEM Model Verification Statistics "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.mid0
frame $wr.mida
frame $wr.mid1
frame $wr.mid2
frame $wr.mid3
frame $wr.div0
frame $wr.mid6
frame $wr.mid7
frame $wr.mid8
frame $wr.mid9
frame $wr.div1
frame $wr.bot

pack $wr.top -padx 6 -pady 6
pack $wr.mid0 $wr.mida $wr.mid1 -pady 4 -padx 4
pack $wr.mid2 $wr.mid3 -pady 4 -padx 4
pack $wr.div0 $wr.mid6 $wr.mid7 $wr.mid8 $wr.mid9 $wr.div1 $wr.bot -pady 5 -padx 10

#-->information

label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i\
 -text "Convert the binary concentration file to DATEM format, compute\
 the model performance statistics, and show a scatter diagram. Measured\
 data must already be available in the DATEM format. Experimental data\
 may be downloaded from http://www.arl.noaa.gov/DATEM.php."
pack $wr.top.lab

#-->select binary hysplit input file and conversion factor

label $wr.mid0.lab1 -text "Input: "
pack $wr.mid0.lab1 -side left -padx 2 
set d 0
foreach item $Grid_name {
   radiobutton $wr.mid0.$d -variable Num_disp -text $item -value $d -background grey
   pack $wr.mid0.$d -side left -padx 4
   incr d
}
entry $wr.mid0.ent -textvariable Force_name -relief sunken -width 12
label $wr.mid0.txt -text "  Force: "
pack  $wr.mid0.txt $wr.mid0.ent -side left 

label $wr.mid0.lab2 -text "  Rotation:"
entry $wr.mid0.ent2 -textvariable Angle -width 5
pack  $wr.mid0.lab2  $wr.mid0.ent2 -side left -padx 4

label $wr.mida.log -text "Interpolation method:"
pack $wr.mida.log -side left
radiobutton $wr.mida.d0 -variable Interp -text "Nearest Neighbor" -value 0 
radiobutton $wr.mida.d1 -variable Interp -text "Bilinear Interpolation" -value 1 
pack $wr.mida.d0 $wr.mida.d1 -side left 

label $wr.mid1.lab1 -text "Measured Data File                      Conversion: "
entry $wr.mid1.ent1 -textvariable Cscale -width 12
pack  $wr.mid1.lab1  $wr.mid1.ent1 -side left -padx 4

#-->select measured data and output file

entry $wr.mid2.ent1  -textvariable name_datem -width 25
pack  $wr.mid2.ent1 -side left
label $wr.mid2.lab2 -text "           Output Data: "
entry $wr.mid2.ent2 -textvariable Model_file -width 15
pack  $wr.mid2.lab2 $wr.mid2.ent2 -side left

button $wr.mid3.win1  -text Browse -width 25 -command {
   set temp [tk_getOpenFile -title "File Selection"]
   if {[string length $temp] > 0} {set name_datem $temp}}
label $wr.mid3.lab -text "          "
button $wr.mid3.run -bg yellow -text "Create DATEM File" -width 28 -command {run_datem}
pack   $wr.mid3.win1 $wr.mid3.lab $wr.mid3.run -side left

#-->verification statistics

label $wr.div0.lab -text "_____________________________________________________________"
pack  $wr.div0.lab -side left

label $wr.mid6.lev -text "Verification Statistics:"
pack  $wr.mid6.lev -side left
radiobutton $wr.mid6.d1 -variable Stat_val -text "All Values" -value 0 
radiobutton $wr.mid6.d2 -variable Stat_val -text "Plume Only" -value 1 
radiobutton $wr.mid6.d3 -variable Stat_val -text "Exclude 0-0" -value 2 
pack $wr.mid6.d1 $wr.mid6.d2 $wr.mid6.d3 -side left

label $wr.mid7.lev -text "Averaging Method:"
pack  $wr.mid7.lev -side left
radiobutton $wr.mid7.d1 -variable Stat_avg -text "None" -value 0 
radiobutton $wr.mid7.d2 -variable Stat_avg -text "Temporal" -value 1 
radiobutton $wr.mid7.d3 -variable Stat_avg -text "Spatial" -value 2 
pack $wr.mid7.d1 $wr.mid7.d2 $wr.mid7.d3 -side left

label $wr.mid8.lab1 -text "Contingency Level: "
entry $wr.mid8.ent1 -textvariable Stat_lev -width 12
pack  $wr.mid8.lab1  $wr.mid8.ent1 -side left -padx 4

button $wr.mid9.run -bg yellow -text "Compute Statistics" -width 30 -command {run_stat}
entry  $wr.mid9.ent -textvariable Stat_out -relief sunken -width 12
label  $wr.mid9.txt -text "  Rename output: "
pack   $wr.mid9.run $wr.mid9.txt $wr.mid9.ent -side left  -padx 4

#-->termination

label $wr.div1.lab \
-text "_____________________________________________________________"
pack $wr.div1.lab -side left

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help  -width 8 \
       -command "load_html [file join $html_dir S345.htm ] "
button $wr.bot.save  -bg green -text "Scatter Plot" -width 20 -command {run_plot}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 
datem_init
}


#---------------------------------------------------------------------
proc run_datem {} {

global Cscale name_datem Model_file
global X_dir tcl_dir exec_dir tcl_platform
global Grid_dir Grid_name Num_disp Force_name
global Angle Start_locn Interp

if { "$Force_name" == "" } {
   set Grid_dir_n  [lindex $Grid_dir $Num_disp]
   set Grid_name_n [lindex $Grid_name $Num_disp]
   set arg1 -i  ; append arg1 $Grid_dir_n$Grid_name_n
} else {
   set arg1 -i  ; append arg1 $Force_name
}

set Map_lat [lindex $Start_locn(1) 0]
set Map_lon [lindex $Start_locn(1) 1]

set arg2 -o  ; append arg2 $Model_file
set arg3 -m  ; append arg3 $name_datem
set arg4 -c  ; append arg4 $Cscale

if { "$Interp" == "1" } {
  set arg5 -xi
} else {
  set arg5 -xn
}

if {$Angle == 0} {
   set arg6 -:
} else {  
   set arg6 "-r${Angle}:${Map_lat}:${Map_lon}"
}

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
   exec $exec_dir/c2datem $arg1 $arg2 $arg3 $arg4 $arg5 $arg6
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/c2datem $arg1 $arg2 $arg3 $arg4 $arg5 $arg6
   }
} else {
   exec $exec_dir/c2datem.exe $arg1 $arg2 $arg3 $arg4 $arg5 $arg6
}
}


#---------------------------------------------------------------------
proc run_stat {} {

global name_datem Model_file Stat_val Stat_avg Stat_lev Stat_out
global X_dir tcl_dir exec_dir tcl_platform

set arg1 -d  ; append arg1 $name_datem
set arg2 -r  ; append arg2 $Model_file
set arg3 -o1 
set arg4 -t0
set arg7 -l  ; append arg7 $Stat_lev

if {$Stat_val == 0} {set arg5 -:}  
if {$Stat_val == 1} {set arg5 -p}  
if {$Stat_val == 2} {set arg5 -x}  

if {$Stat_avg == 0} {set arg6 -:  ; set Statout A}  
if {$Stat_avg == 1} {set arg6 -at ; set Statout T} 
if {$Stat_avg == 2} {set arg6 -as ; set Statout S} 

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
   exec $exec_dir/statmain $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/statmain $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7
   }
} else {
   exec $exec_dir/statmain.exe $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7
}

if [file exists stat${Statout}.txt] {
   set log [ScrollText .f]
   $log insert end " Contents of statA.txt ...\n"
   update
   set fileid [open "stat${Statout}.txt" r]
   while {[eof $fileid] != 1} {
      gets $fileid cline
      $log insert end $cline\n
   }
   close $fileid
   
   if { "$Stat_out" != "" } {
      if [file exists stat${Statout}_$Stat_out.txt] {file delete stat${Statout}_$Stat_out.txt}
      if [file exists stat${Statout}.txt] {file rename stat${Statout}.txt stat${Statout}_$Stat_out.txt}

      if [file exists data${Statout}_$Stat_out.txt] {file delete data${Statout}_$Stat_out.txt}
      if [file exists data${Statout}.txt] {file rename data${Statout}.txt data${Statout}_$Stat_out.txt}
   } 

} else {
   msg_box "File not found: stat${Statout}.txt"
}
}

#-----------------------------------------------------------

proc run_plot {} {

global Stat_avg Stat_lev Stat_out
global X_dir tcl_dir exec_dir tcl_platform

if { "$Stat_out" != "" } {
   if {$Stat_avg == 0} {set Statout dataA_${Stat_out}.txt}  
   if {$Stat_avg == 1} {set Statout dataT_${Stat_out}.txt}
   if {$Stat_avg == 2} {set Statout dataS_${Stat_out}.txt}
} else {
   if {$Stat_avg == 0} {set Statout dataA.txt}  
   if {$Stat_avg == 1} {set Statout dataT.txt}
   if {$Stat_avg == 2} {set Statout dataS.txt}
}

if [file exists $Statout] {
   set arg1 -i  ; append arg1 $Statout
   set arg2 -p  ; append arg2 $Stat_lev

   set arg_ext "+g1"

   if { "$tcl_platform(platform)" == "unix" } {
      if { "$X_dir" == "" } {
      exec $exec_dir/scatter $arg_ext $arg1 $arg2
      } else {
      exec $X_dir/xterm -fn fixed -e $exec_dir/scatter $arg_ext $arg1 $arg2
      }
   } else {
      exec $exec_dir/scatter.exe $arg_ext $arg1 $arg2
   }
   svg_box {scatter.html}

} else {
   msg_box "File not found: $Statout"
}
}


#-----------------------------------------------------------

proc datem_init {} {

global Cscale name_datem Model_file Stat_val Stat_avg Stat_lev Angle Stat_out
global Force_name Interp

if [ info exists Angle ] { } else {
   set Angle ""
}

if { $Angle == "" } {
   set Interp 1
   set Angle 0.0
   set Cscale 1.0E+12
   set name_datem measured.txt
   set Model_file hysplit.txt
   set Stat_val 0
   set Stat_avg 0
   set Stat_lev 10.0
   set Stat_out ""
   if [ info exists Force_name ] { } else {set Force_name ""}
}
}
