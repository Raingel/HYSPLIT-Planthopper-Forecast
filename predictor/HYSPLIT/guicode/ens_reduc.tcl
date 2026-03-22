proc ens_reduc { } {

#-----------------------------------------------------------------------------
# ENS_REDUC.TCl: 
#                the release amount using a DATEM formatted measured data file
#-----------------------------------------------------------------------------
# Last Revised: 20 Nov 2014 - initial version from srm_solve.tcl
#               23 Jan 2014 - create STDOUT when error messages exist
#               28 Jul 2017 - renamed out_file to rout_file for initialization 
#-----------------------------------------------------------------------------

global html_dir Grid_name Units Level 
global name_datem name_model rout_file
if [winfo exists .ensred] {destroy .ensred}
set wr .ensred
toplevel $wr
wm title $wr " Ensemble reduction"
wm  geometry $wr +50+15

frame $wr.top
frame $wr.step1
frame $wr.stepa
frame $wr.step2
frame $wr.stepb
frame $wr.step3
frame $wr.stepc
frame $wr.bot
pack $wr.top $wr.step1 $wr.stepa $wr.step2 $wr.stepb \
    $wr.step3 $wr.stepc \
     -side top -pady 5 -padx 10
pack $wr.bot -side top -pady 15 -padx 10


#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "Applies a reduction technique to an ensemble. \
In this technique all the possible model combinations \
are tested and the chosen subensemble is the one that \
shows the minimum square error. The measurements and \
model results should be defined by DATEM format files \
with an identical number of records."
pack $wr.top.lab

#-----------------------------------------------------------
# Step1 select input model files in datem format
# file names selected using input file name as wildcard

label $wr.step1.lab -width 50 -bg Gray65 -anchor w \
      -text "Step 1: Define the model input file base name"
button $wr.step1.but  -text Browse -width 8 -command {
   set temp [tk_getOpenFile -title "File Selection"]
   if {[string length $temp] > 0} {set name_model $temp}}
pack  $wr.step1.lab $wr.step1.but -side left -padx 5

entry $wr.stepa.ent -textvariable name_model -relief sunken -width 60 
pack  $wr.stepa.ent


#----------------------------------------------------------
# Step2 define the measured data file

label $wr.step2.lab -width 50 -bg Gray65 -anchor w \
      -text "Step 2: Define the measured data input file"
button $wr.step2.but  -text Browse -width 8 -command {
   set temp [tk_getOpenFile -title "File Selection"]
   if {[string length $temp] > 0} {set name_datem $temp}}
pack  $wr.step2.lab $wr.step2.but -side left -padx 5

entry $wr.stepb.ent -textvariable name_datem -relief sunken -width 60 
pack  $wr.stepb.ent


#-------------------------------------
# Step3 apply reduction technique 

label  $wr.step3.lab -width 50 -bg Gray65 -anchor w \
       -text "Step 3: Apply reduction technique"
button $wr.step3.but  -text "Apply" -width 8 -command accu_div
pack   $wr.step3.lab $wr.step3.but -side left -padx 5

entry  $wr.stepc.ent -textvariable rout_file -relief sunken -width 60
pack  $wr.stepc.ent

#----------------------
# bottom action buttons

button $wr.bot.dismiss  -bg red -text Quit -width 28 -command "destroy $wr"
button $wr.bot.help -text Help -width 28 \
       -command "load_html [file join $html_dir S356.htm ] "
pack   $wr.bot.dismiss $wr.bot.help -side left -padx 8


set_defaultm
}

#------------------------------------------------------------------------------
# apply reduction technique minimizing square error

proc accu_div {} {

global name_model rout_file name_datem
global X_dir tcl_platform exec_dir
global Units Species Level Half

set arg1 -b  ; append arg1 $name_model
set arg2 -m  ; append arg2 $name_datem
set arg3 -o  ; append arg3 $rout_file

if [ file exists STDOUT ] {file delete -force STDOUT} 
if [ file exists $rout_file ] {file delete -force $rout_file} 

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
   exec $exec_dir/accudiv $arg1 $arg2 $arg3 >STDOUT
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/accudiv $arg1 $arg2 $arg3                                     
   }
} else {
   exec $exec_dir/accudiv.exe $arg1 $arg2 $arg3 >STDOUT
}

   if [file exists STDOUT] {
      set log [ScrollText .f]
      set fid [open "STDOUT" r]
      while {[eof $fid] != 1} {
      gets $fid cline
      $log insert end $cline\n
   }
      close $fid
      file delete -force STDOUT
   }

#   display summary statistics
    if [ file exists $rout_file ] {
       set log [ScrollText .f]
       $log insert end "                            ENSEMBLE REDUCTION SUMMARY\n"
       set fileid [open "$rout_file" r]
       while {[eof $fileid] != 1} {
          gets $fileid cline
          $log insert end $cline\n
       }
       close $fileid
    } else {
       msg_box "Ensemble reduction file not created: $rout_file"
       tkwait window .msg_win
    }
}


#------------------------------------------------------------------------------
# set initial defaults

proc set_defaultm {} {

global name_datem rout_file name_model

if [ info exists rout_file ] { } else {set rout_file ""}

if { $rout_file == "" } {
   if { $name_datem == "" } {set name_datem "measured.txt"}
   if { $name_model == "" } {set name_model "hysplit_datem"}
   set rout_file  "reduc.txt"
   }
}


