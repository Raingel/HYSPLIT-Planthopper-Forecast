proc ens_rank { } {

#-----------------------------------------------------------------------------
#  ENS_RANK.TCL: This script uses c2datem to create a corresponding DATEM 
#                formatted file. The statmain program is then run to create
#                a statistical summary file for each member. A measurement
#                data file is required to run this procedure. 
#-----------------------------------------------------------------------------
# Last Revised: 22 Feb 2012 - initial version from conc_rank.tcl
#               17 Apr 2013 - added display window of statistica summary file
#               25 Jul 2013 - corrected argument list for c2datem call
#               02 Jul 2014 - added commented out diagnostic
#               19 Jul 2017 - added binary input file selection options
#               28 Jul 2017 - defined rank_init for initialization
#-----------------------------------------------------------------------------

global html_dir name_datem name_stats kstat Cscale Grid_name Num_disp Force_name

if [winfo exists .ensrank] {destroy .ensrank}
set wr .ensrank
toplevel $wr
wm title $wr " Statistical Analysis of Ensemble "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.step0
frame $wr.step1
frame $wr.stepa
frame $wr.step2
frame $wr.stepb
frame $wr.step4
frame $wr.bot
pack $wr.top $wr.step0 $wr.step1 $wr.stepa $wr.step2 $wr.stepb \
             $wr.step4 $wr.bot -side top -pady 5 -padx 5

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "This procedure is intended to compare multiple simulations\
  with measured data. After running one of the ensembles, this script\
  converts the output to DATEM format, and then calls the statistical\
  anlaysis program tabulating the results for each ensemble member."
pack $wr.top.lab

#----------------------------------------
# Step0 define the base name of the binary input file

label $wr.step0.lab -text "Ensemble base name:"
pack  $wr.step0.lab -side left -padx 5
set d 0
foreach item $Grid_name {
   radiobutton $wr.step0.$d -variable Num_disp -text $item -value $d -background grey -command ens_rank
   pack $wr.step0.$d -side left -padx 2
   incr d
}
set Force_name [lindex $Grid_name $Num_disp]
entry $wr.step0.ent -textvariable Force_name -relief sunken -width 15
label $wr.step0.txt -text "  Force: "
pack  $wr.step0.txt $wr.step0.ent -side left 

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


#--------------------------------------
# Step3 process all source locations

label $wr.step4.lab -width 50 -bg Gray65 -anchor w \
      -text "Step 3: Process data files for each member"
button $wr.step4.but -bg yellow -text Execute -width 8 -command {run_members}
pack  $wr.step4.lab $wr.step4.but -side left -padx 5


#--------------------------------------
# bottom action buttons

button $wr.bot.dismiss  -bg red -text Quit -width 24 -command "destroy $wr"
button $wr.bot.help -text Help -width 24 \
       -command "load_html [file join $html_dir S354.htm ] "
label  $wr.bot.blank -text "   " -width 8
pack   $wr.bot.dismiss $wr.bot.help $wr.bot.blank -side left -padx 5

set_defaultr
}

#======================================================
proc run_members {} {

global name_datem name_stats kstat Cscale
global X_dir exec_dir tcl_platform Force_name

if [ file exists hysplit_datem.txt ] {file delete hysplit_datem.txt}
if [ file exists $name_stats ] {file delete $name_stats}

set Fnum 0
while {$Fnum < 999} {
    incr Fnum       

#   add member number appendix to the ensemble binary input file
    set myfile $Force_name
    append myfile .[format "%3.3u" $Fnum]
    set arg2 -i ; append arg2 $myfile

    if [ file exists $myfile ] {


#   convert binary output file to DATEM text format
    set arg1 -c ; append arg1 $Cscale
    set arg3 -m ; append arg3 $name_datem
    set arg4 -o ; append arg4 hysplit_datem.txt
    set arg5 -xi
    set arg6 -s

#   optional diagnostic statement for testing script
#   msg_box "c2datem $arg1 $arg2 $arg3 $arg4 $arg5 $arg6"
#   tkwait window .msg_win

    if { "$tcl_platform(platform)" == "unix" } {
       if { "$X_dir" == "" } {
       exec $exec_dir/c2datem $arg1 $arg2 $arg3 $arg4 $arg5 $arg6
       } else {
       exec $X_dir/xterm -fn fixed -e $exec_dir/c2datem $arg1 $arg2 $arg3 $arg4 $arg5 $arg6
       }
    } else {
       exec $exec_dir/c2datem $arg1 $arg2 $arg3 $arg4 $arg5 $arg6
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

#   save complete statistics
    set myfile stat_[format "%3.3u" $Fnum].txt
    if [ file exists statA.txt ] {file rename -force statA.txt $myfile}

#   display summary statistics
    if [ file exists $name_stats ] {
       set log [ScrollText .f]
       $log insert end "                            ENSEMBLE RUN STATISTICAL SUMMARY\n"
       set fileid [open "$name_stats" r]
       while {[eof $fileid] != 1} {
          gets $fileid cline
          $log insert end $cline\n
       }
       close $fileid
    } else {
       msg_box "Statistics file not created: $name_stats"
       tkwait window .msg_win
    }

#   cleanup
    if [ file exists hysplit_datem.txt ] {file rename -force hysplit_datem.txt hysplit_datem.[format "%3.3u" $Fnum] }

}
}

msg_box " Statistical processing complete! "
tkwait window .msg_win
destroy .ensrank
}


#------------------------------------------------------------------------------
# set initial defaults

proc set_defaultr {} {

global name_datem name_stats kstat Cscale rank_init

if [ info exists rank_init ] { } else {set rank_init ""}

if {$rank_init == ""} {
   set rank_init "yes"
   if {$Cscale == ""} {set Cscale 1.0E+12}
   if {$name_datem == ""} {set name_datem measured.txt}
   set name_stats sumstat.txt
   }
set kstat 8
}
