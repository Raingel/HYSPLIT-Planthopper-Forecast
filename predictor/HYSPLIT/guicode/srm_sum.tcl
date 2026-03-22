proc srm2sum {} {
#------------------------------------------------------------------------------
# SRM_SUM.TCL: apply source term to time-varying TCM single output file
# Last Revised: 03 Sep 2014
#               28 Jul 2017 - revised initialization
#------------------------------------------------------------------------------

global Out_file polid src_name num_time
global html_dir Grid_name Num_disp Force_name

if [winfo exists .timeplot] {destroy .tcm2sum}
set wr .tcm2sum
toplevel $wr
wm title $wr " Convert Time-Varying TCM to Concentration "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.mid0
frame $wr.mid1
frame $wr.mid2
frame $wr.mid3
frame $wr.mid4
frame $wr.mid6
frame $wr.bot
pack $wr.top $wr.mid0 $wr.mid1 $wr.mid2 $wr.mid3 $wr.mid4 \
     $wr.mid6 $wr.bot -pady 5 -padx 10

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "Add together the pollutant concentrations from one HYSPLIT TCM\
concentration file and create a single pollutant (.bin) output file.\
In the TCM file the pollutants represent different starting times as\
configured from a HYSPLIT Transfer Coefficient Matrix (TCM) simulation\
with ICHEM=10. In creating the output file, each starting time is multiplied\
by a source term defined in the source data input file."
pack $wr.top.lab

#-->select grid
label $wr.mid0.lab -text "TCM Input File: "
pack $wr.mid0.lab -side left -padx 5
set d 0
foreach item $Grid_name {
   radiobutton $wr.mid0.$d -variable Num_disp -text $item -value $d -background grey
   pack $wr.mid0.$d -side left -padx 2
   incr d
}
entry $wr.mid0.ent -textvariable Force_name -relief sunken -width 15
label $wr.mid0.txt -text "  Force Name: "
pack  $wr.mid0.txt $wr.mid0.ent -side left 

#-->output file
label $wr.mid1.lab -text "Output File Base Name: "
entry $wr.mid1.ent -textvariable Out_file -relief sunken -width 15
pack $wr.mid1.lab $wr.mid1.ent -side left

#-->sum pollutant character ID
label $wr.mid2.lab -text "Output Pollutant Name: "
entry $wr.mid2.ent -textvariable polid -relief sunken -width 4
pack $wr.mid2.lab $wr.mid2.ent -side left

#-->source term file name
label  $wr.mid3.l1  -text "Use existing Source File:" 
entry  $wr.mid3.e1  -textvariable src_name -width 15
label  $wr.mid3.l3  -text "  " 
button $wr.mid3.win  -text Browse -width 8 -command {
   set temp [tk_getOpenFile -title "File Selection"]
   if {[string length $temp] > 0} {set src_name $temp}}
pack   $wr.mid3.l1 $wr.mid3.e1 $wr.mid3.l3 $wr.mid3.win -side left -padx 2

#--create new source term file
label  $wr.mid4.l2  -text " Or new file for time periods:" 
entry  $wr.mid4.e2  -textvariable num_time  -width 2
label  $wr.mid4.l3  -text "          "
button $wr.mid4.but  -text "Create" -width 8  -command "make_src"
pack   $wr.mid4.l2 $wr.mid4.e2 $wr.mid4.l3 $wr.mid4.but -side left -padx 2

#-->termination
label $wr.mid6.lab \
-text "_____________________________________________________________"
pack $wr.mid6.lab -side left

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help  -width 8 \
       -command "load_html [file join $html_dir S340.htm ] "
button $wr.bot.save  -bg green -text "Create Output" -width 20 -command {run_sums}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 
src_defaults
}

#---------------------------------------------------------------------
proc run_sums {} {

global Out_file polid src_name num_time Force_name
global html_dir tcl_platform exec_dir Grid_dir Grid_name Num_disp

if { "$Force_name" == "" } {
   set Grid_dir_n [lindex $Grid_dir $Num_disp]
   set Grid_name_n  [lindex $Grid_name $Num_disp]
   set arg1 -i  ; append arg1 $Grid_dir_n$Grid_name_n
} else {
   set arg1 -i  ; append arg1 $Force_name
}
 
set arg2 -o  ; append arg2 ${Out_file}
set arg3 -p  ; append arg3 $polid
set arg4 -s  ; append arg4 $src_name
set arg5 -i  ; append arg5 ${Out_file}

# turn on for diagnostic testing
# msg_box "tcmsum $arg1 $arg2 $arg3 $arg4 $arg5"  
# tkwait window .msg_win

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
     exec $exec_dir/tcmsum $arg1 $arg2 $arg3 $arg4 $arg5
   } else {
     exec $X_dir/xterm -fn fixed -e $exec_dir/tcmsum $arg1 $arg2 $arg3 $arg4 $arg5
   }
} else {
  exec $exec_dir/tcmsum.exe $arg1 $arg2 $arg3 $arg4 $arg5
}
}

#-----------------------------------------------------------------
#-->set initial defaults
proc src_defaults {} {
global Out_file polid src_name num_time Force_name

if [ info exists src_name ] { } else {set src_name ""}
if { $src_name == "" } {
   set Out_file tcmsum
   set polid SUM
   set src_name cfactors.txt
   set num_time 1
   if [ info exists Force_name ] { } else {set Force_name ""}
   }
}

#-----------------------------------------------------------------
proc make_src {} {

global src_data src_name num_time

if [winfo exists .srctime] {destroy .srctime}
set wr .srctime
toplevel $wr
wm title $wr " Source Term Time Variation "
wm  geometry $wr +100+100

frame $wr.title
frame $wr.loc
frame $wr.end
pack $wr.title $wr.loc $wr.end -padx 4 -side top

label $wr.title.lab1 -text "Enter Values for $num_time Emission Times"
label $wr.title.lab2 -text "YYYY MM DD HH Value"
pack  $wr.title.lab1 $wr.title.lab2

for { set d 1} { $d <=$num_time} {incr d} {
   frame $wr.loc.dat$d
   pack $wr.loc.dat$d -side top
   label $wr.loc.dat$d.lab -text "$d:"
   entry $wr.loc.dat$d.ent -textvariable src_data($d) -width 23
   pack $wr.loc.dat$d.lab $wr.loc.dat$d.ent -padx 4 -side left
}
button $wr.end.dismiss -text Quit -width 8 -command "destroy $wr"
button $wr.end.help -text OK  -width 8 -command "save_time $wr"
pack $wr.end.dismiss $wr.end.help -side left -pady 6 -padx 10
init_times
}

#-----------------------------------------------------------------
proc init_times {} {

global src_data src_name num_time

set date [clock format [clock scan now] -format "%Y %m %d %H"]
set YY [lindex [split $date] 0]
set MM [lindex [split $date] 1]
set DD [lindex [split $date] 2]
set HH [lindex [split $date] 3]
if { $YY <  40 } { set YY 20$YY}
if { $YY < 100 } { set YY 19$YY}

for { set d 1} { $d <=$num_time} {incr d} {
    set src_data($d) "$YY $MM $DD $HH 1000.0"
}
}

#-----------------------------------------------------------------
proc save_time {wr} {

global src_data src_name num_time

if {$src_name == ""} {set src_name "cfactors.txt"}
set f [open $src_name w]

puts $f "YYYY MM DD HH Value"
for { set d 1} { $d <=$num_time} {incr d} {
    puts $f $src_data($d)
}
close $f
destroy $wr
}
