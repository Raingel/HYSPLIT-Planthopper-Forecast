proc cfg_test {} {

#-----------------------------------------------------------------------------
# TEST_CFG.TCL: Test the CONTROL and SETUP.CFG files to optimize settings
#-----------------------------------------------------------------------------
# Last Revised: 06 Jul 2016 - initial version from srm_solve.tcl
#               30 Jul 2016 - open both original and modified files
#-----------------------------------------------------------------------------

global html_dir

if [winfo exists .cfgtest] {destroy .cfgtest}
set wr .cfgtest
toplevel $wr
wm title $wr " Test and Optimize CONTROL and SETUP.CFG "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.step1
frame $wr.step2
frame $wr.step3
frame $wr.step5
frame $wr.bot
pack $wr.top $wr.step1 $wr.step2 $wr.step3 $wr.step5 \
             -side top -pady 5 -padx 10
pack $wr.bot -side top -pady 15 -padx 10

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 5i \
 -text "Invokes a program to analyze various input files such as CONTROL\
 and SETUP.CFG to determine if the user options are correctly configured.\
 Modified input files are written to CONTROL_mod and SETUP_mod.CFG. Copy\
 mods prior to running the model, however, GUI variables are not changed.\
 Update the GUI variables by reading the mod files from the retrieve menu."

pack $wr.top.lab

#-----------------------------------------------------------
# Step1 analyze the input files

label  $wr.step1.lab -width 50 -bg Gray65 -anchor w \
       -text "Step 1: Analyze CONTROL and SETUP.CFG"
button $wr.step1.but -bg cyan -text "Analyze" -width 8 -command Test_run
pack   $wr.step1.lab $wr.step1.but -side left -padx 5

#----------------------------------------------------------
# Step2 view files

label $wr.step2.lab -width 60 -bg Gray65 -anchor w \
      -text "Step 2: View Output Files ..."
pack  $wr.step2.lab -side left -padx 5

button $wr.step3.but1 -bg yellow -text WARNING -width 8 -command {Test_view WARNING_mod   .f}
button $wr.step3.but2 -bg yellow -text MESSAGE -width 8 -command {Test_view MESSAGE_mod   .f}
button $wr.step3.but3 -bg yellow -text CONTROL -width 8 -command {Test_view CONTROL_mod   .a; \
                                                                  Test_view CONTROL       .b  }
button $wr.step3.but4 -bg yellow -text SETUP   -width 8 -command {Test_view SETUP_mod.CFG .c; \
                                                                  Test_view SETUP.CFG     .d  }
pack   $wr.step3.but1 $wr.step3.but2 $wr.step3.but3 $wr.step3.but4 -side left -padx 5

#-------------------------------------
# Step5 copy changes from _mod to run files

label  $wr.step5.lab -width 50 -bg Gray65 -anchor w \
       -text "Step 3: Copy mods to CONTROL and SETUP.CFG"
button $wr.step5.but -bg cyan -text "Copy" -width 8 -command {Test_update}
pack   $wr.step5.lab $wr.step5.but -side left -padx 5

#----------------------
# bottom action buttons

button $wr.bot.dismiss  -bg red -text Quit -width 28 -command "destroy $wr"
button $wr.bot.help -text Help -width 28 \
       -command "load_html [file join $html_dir S356.htm ] "
pack   $wr.bot.dismiss $wr.bot.help -side left -padx 8
}

#=================================================================

proc Test_run {} {
global exec_dir result log tcl_platform

if [file exists MESSAGE_mod] {file delete MESSAGE_mod}
if [file exists WARNING_mod] {file delete WARNING_mod}
if [file exists CONTROL_mod] {file delete CONTROL_mod}
if [file exists SETUP_mod.CFG] {file delete SETUP_mod.CFG}

if { "$tcl_platform(platform)" == "unix" } {
   set code "${exec_dir}/hysptest"
   set xops "|$code |& cat"
   } else {
   set code ${exec_dir}/hysptest.exe
   set xops "|$code"
   }

if [file exists $code] {
   set log [ScrollText .f]
   $log configure -cursor watch
   $log insert end "Test started ...\n"
   update

   if [catch {open $xops} result] {
      $log insert end $result
      } else {
      fileevent $result readable Log
      $log insert end "Test complete\n"
      }

   } else {
   msg_box "Test executable HYSPTEST not found!"
   }
}


#------------------------------------------------------------------------------
# show results 

proc Test_view {File win} {

global log 

set log [ScrollText $win]
$log insert end "$File file contents:\n\n"

if [file exists $File] {
   set fileid [open "$File" r]
   while {[eof $fileid] != 1} {
      gets $fileid cline
      $log insert end $cline\n
   }
   close $fileid
   }
}


#------------------------------------------------------------------------------
# copy the modified files to the operational files

proc Test_update {} {
global Mtype

if {"$Mtype" == "T"} {
   if [file exists CONTROL_mod] {file copy -force CONTROL_mod default_traj}
   } else {
     if [file exists CONTROL_mod] {file copy -force CONTROL_mod default_conc}
   }

if [file exists SETUP_mod.CFG] {file copy -force SETUP_mod.CFG SETUP.CFG}
}

