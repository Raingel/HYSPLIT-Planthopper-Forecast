proc srm_solve { } {

#-----------------------------------------------------------------------------
# SRM_SOLVE.TCL: Solve the source-receptor matrix for the time variation of
#                the release amount using a DATEM formatted measured data file
#-----------------------------------------------------------------------------
# Last Revised: 30 Jul 2007 - initial version from auto_geol.tcl
#               25 Jun 2013 - revised to solve for time variation of source
#               01 Sep 2016 - added lsort of glob output
#               28 Jul 2017 - changed init test from srm_flist to solve_init
#               02 Sep 2020 - default undefined Cscale = 1
#-----------------------------------------------------------------------------

global html_dir Grid_name Cscale Level Species Half Zero Pcnt
global name_datem srm_flist srm_matrix srm_vector
if [winfo exists .srmsolve] {destroy .srmsolve}
set wr .srmsolve
toplevel $wr
wm title $wr " Solution of the Source-Receptor Matrix "
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
frame $wr.stepe
frame $wr.bot
pack $wr.top $wr.step1 $wr.stepa $wr.step2 $wr.stepb $wr.step3 \
     $wr.stepc $wr.step4 $wr.stepd $wr.step5 $wr.stepe \
     -side top -pady 5 -padx 10
pack $wr.bot -side top -pady 15 -padx 10


#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "Solves the transfer coefficient matrix for the source\
 term vector where each column in the matrix represents the\
 dilution factors for that release period to each receptor row.\
 The measurements should be defined in the DATEM format."
pack $wr.top.lab

#-----------------------------------------------------------
# Step1 select input binary data file by creating INFILE
# file of file names using input file name as wildcard

label  $wr.step1.lab -width 50 -bg Gray65 -anchor w \
       -text "Step 1: Create filename list using a wildcard"
button $wr.step1.but  -text "Create" -width 8 -command srm_infile
pack   $wr.step1.lab $wr.step1.but -side left -padx 5

entry  $wr.stepa.ent -textvariable srm_flist -relief sunken -width 60
pack   $wr.stepa.ent


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

#-------------------------------------------------
# Step3 Define the conversion details

label  $wr.step3.lab -width 60 -bg Gray65 -anchor w \
       -text "Step 3: Define the file conversion details"
pack   $wr.step3.lab -side left -padx 5

entry $wr.stepc.ent1 -textvariable Level -width 2
label $wr.stepc.lab1 -text "= Height   "
entry $wr.stepc.ent2 -textvariable Species -width 2
label $wr.stepc.lab2 -text "= Species  "
entry $wr.stepc.ent3 -textvariable Cscale -relief sunken -width 8
label $wr.stepc.lab3 -text "= Units    "
entry $wr.stepc.ent4 -textvariable Half -relief sunken -width 4
label $wr.stepc.lab4 -text "= Half"
pack  $wr.stepc.ent1 $wr.stepc.lab1 $wr.stepc.ent2 $wr.stepc.lab2 \
      $wr.stepc.ent3 $wr.stepc.lab3 $wr.stepc.ent4 $wr.stepc.lab4 \
      -side left -padx 1


#-------------------------------------
# Step4 Create the source-receptor matrix CSV file

label  $wr.step4.lab -width 50 -bg Gray65 -anchor w \
       -text "Step 4: Create the transfer coefficient matrix"
button $wr.step4.but  -text "Create" -width 8 -command srm_matrix
pack   $wr.step4.lab $wr.step4.but -side left -padx 5

entry  $wr.stepd.ent -textvariable srm_matrix -relief sunken -width 60
pack   $wr.stepd.ent

#-------------------------------------
# Step5 Display the results

label  $wr.step5.lab -width 50 -bg Gray65 -anchor w \
      -text "Step 5: Solve the equations and display results"
button $wr.step5.but  -text "Solve" -width 8 -command srm_tcsolve
pack   $wr.step5.lab $wr.step5.but -side left -padx 5

entry $wr.stepe.ent1 -textvariable Zero -width 5
label $wr.stepe.lab1 -text "= Zero    "
entry $wr.stepe.ent2 -textvariable Pcnt -width 3
label $wr.stepe.lab2 -text "= % delete   "
entry $wr.stepe.ent3 -textvariable srm_vector -relief sunken -width 12
label $wr.stepe.lab3 -text "= Output File"
pack  $wr.stepe.ent1 $wr.stepe.lab1 $wr.stepe.ent2 $wr.stepe.lab2 $wr.stepe.ent3\
      $wr.stepe.lab3 -side left -padx 1

#----------------------
# bottom action buttons

button $wr.bot.dismiss  -bg red -text Quit -width 28 -command "destroy $wr"
button $wr.bot.help -text Help -width 28 \
       -command "load_html [file join $html_dir S337.htm ] "
pack   $wr.bot.dismiss $wr.bot.help -side left -padx 8

set_defaultm
}

#------------------------------------------------------------------------------
# create the trasfer coefficient matrix with the last column as measurements

proc srm_matrix {} {

global srm_flist srm_matrix name_datem
global X_dir tcl_platform exec_dir
global Cscale Species Level Half

set arg1 -i  ; append arg1 $srm_flist
set arg2 -m  ; append arg2 $name_datem
set arg3 -o  ; append arg3 $srm_matrix
set arg4 -p  ; append arg4 $Species
set arg5 -z  ; append arg5 $Level
set arg6 -c  ; append arg6 $Cscale
set arg7 -h  ; append arg7 $Half

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
   exec $exec_dir/c2array $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/c2array $arg1 $arg2 $arg3 $arg4 \
                                                    $arg5 $arg6 $arg7
   }
} else {
   exec $exec_dir/c2array.exe $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7
}
}

#------------------------------------------------------------------------------
# solve and show results 

proc srm_tcsolve {} {

global log 
global srm_matrix srm_vector Zero Pcnt
global X_dir tcl_platform exec_dir

set arg1 -i  ; append arg1 $srm_matrix
set arg2 -o  ; append arg2 $srm_vector
set arg3 -p  ; append arg3 $Pcnt
set arg4 -z  ; append arg4 $Zero

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
   exec $exec_dir/tcsolve $arg1 $arg2 $arg3 $arg4
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/tcsolve $arg1 $arg2 $arg3 $arg4
   }
} else {
   exec $exec_dir/tcsolve.exe $arg1 $arg2 $arg3 $arg4
}

set log [ScrollText .f]
$log insert end "$srm_vector file contents:\n"

if [file exists $srm_vector] {
   set fileid [open "$srm_vector" r]
   while {[eof $fileid] != 1} {
      gets $fileid cline
      $log insert end $cline\n
   }
   close $fileid
   }
}

#------------------------------------------------------------------------------
# set initial defaults

proc set_defaultm {} {

global name_datem srm_flist srm_matrix srm_vector 
global Cscale Level Species Half Zero Pcnt solve_init

if [ info exists solve_init ] { } else {
   set solve_init ""
}

if { $solve_init == "" } {
   set solve_init "yes"
   if { $name_datem == "" } {set name_datem "measured.txt"}
   if { $srm_matrix == "" } {set srm_matrix "c2array.csv"}
   if { $srm_vector == "" } {set srm_vector "source.txt"}
   if { $Cscale == "" }     {set Cscale 1.0}
   set srm_flist "cdump"
   set Level 1
   set Species 1
   set Half 0.0
   set Zero 0.0
   set Pcnt 0
   }
}

#------------------------------------------------------------------------------
# create file listing (INFILE) using default file name as the wildcard

proc srm_infile  {} {

  global srm_flist

  if {"$srm_flist" == ""} {
     msg_box " ERROR: Need wildcard in Input Name field! "
  } else {
     if [file exists INFILE] {file delete -force INFILE}

     set g [open INFILE w]
     set flist [lsort [glob -nocomplain *${srm_flist}*]]
     foreach f $flist {puts $g $f}
     close $g

     if {[file size INFILE] == 0} {
        msg_box " Contents of ${srm_flist} empty! "
     } else {
        set srm_flist INFILE
        msg_box " File of file names created (${srm_flist}) "
     }
  }
}

