proc srm_lbfgsb { } {

#-----------------------------------------------------------------------------
# SRM_LBFGSB.TCL: Solve the source-receptor matrix for the time variation of
#                 the release amount and a DATEM formatted measured data file
#                 using Large-Scale Bound Constrained Optimization (LBFGSB) -
#                 cost function minimization
#-----------------------------------------------------------------------------
# Last Revised: 10 Aug 2016 - initial version from srm_solve.tcl
#               26 Aug 2016 - used catch command for exec of c2array
#               01 Sep 2016 - added lsort of glob output
#               16 Nov 2016 - file name changes for UNIX consistency
#               28 Jul 2017 - lbfgsb_init test for initialization
#               02 Sep 2020 - default undefined Cscale = 1
#-----------------------------------------------------------------------------

global html_dir Grid_name Cscale Level Species Half
global name_datem srm_flist srm_matrix srm_vector
global bckg_const LN_X LN_Y bounds Xscale uof uoa ubf uba

if [winfo exists .srmlbfgsb] {destroy .srmlbfgsb}
set wr .srmlbfgsb
toplevel $wr
wm title $wr " Cost Function Solution of the Source-Receptor Matrix "
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
frame $wr.stepf
frame $wr.stepg
frame $wr.steph
frame $wr.stepi
frame $wr.step6
frame $wr.stepj
frame $wr.bot
pack $wr.top $wr.step1 $wr.stepa $wr.step2 $wr.stepb $wr.step3 \
     $wr.stepc $wr.step4 $wr.stepd $wr.step5 $wr.stepe $wr.stepf \
     $wr.stepg $wr.steph $wr.stepi $wr.step6 $wr.stepj -side top -pady 5 -padx 10
pack $wr.bot -side top -pady 15 -padx 10

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 5i \
 -text "Solves the transfer coefficient matrix (TCM) for the source term\
 vector using the cost function minimization method. Each column of the\
 TCM represents the dilution factors for that release period to each\
 receptor row. The measurements for each row are in the last column\
 of the CSV file."
pack $wr.top.lab

#-----------------------------------------------------------
# Step1 select input binary data file by creating INFILE
# file of file names using input file name as wildcard

label  $wr.step1.lab -width 50 -bg Gray65 -anchor w \
       -text "Step 1: Create filename list using a wildcard"
button $wr.step1.but -bg yellow -text "Create" -width 8 -command srm_infile2
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
       -text "Step 3: Define the binary to CSV file conversion details"
pack   $wr.step3.lab -side left -padx 5

entry $wr.stepc.ent1 -textvariable Level   -width 2
label $wr.stepc.lab1 -text "= Height   "
entry $wr.stepc.ent2 -textvariable Species -width 2
label $wr.stepc.lab2 -text "= Species  "
entry $wr.stepc.ent3 -textvariable Cscale   -width 8
label $wr.stepc.lab3 -text "= Units   "
entry $wr.stepc.ent4 -textvariable Half    -width 4
label $wr.stepc.lab4 -text "= Half"
pack  $wr.stepc.ent1 $wr.stepc.lab1 $wr.stepc.ent2 $wr.stepc.lab2 \
      $wr.stepc.ent3 $wr.stepc.lab3 $wr.stepc.ent4 $wr.stepc.lab4 \
      -side left -padx 1

#-------------------------------------
# Step4 Create the source-receptor matrix CSV file

label  $wr.step4.lab -width 50 -bg Gray65 -anchor w \
       -text "Step 4: Create the transfer coefficient matrix"
button $wr.step4.but -bg yellow -text "Create" -width 8 -command srm_matrix2
pack   $wr.step4.lab $wr.step4.but -side left -padx 5

entry  $wr.stepd.ent -textvariable srm_matrix -relief sunken -width 60
pack   $wr.stepd.ent

#-------------------------------------
# Step5 Display the results

label  $wr.step5.lab -width 50 -bg Gray65 -anchor w \
      -text "Step 5: Create the solution parameters input file"
button $wr.step5.but -bg yellow -text "Create" -width 8 -command save_parms
pack   $wr.step5.lab $wr.step5.but -side left -padx 5

global bckg_const LN_X LN_Y bounds Xscale uof uoa ubf uba


label $wr.stepe.lab1 -text "First-guess:"
entry $wr.stepe.ent1 -textvariable bckg_const -width 10
label $wr.stepe.lab2 -text "   Scaling factor:"
entry $wr.stepe.ent2 -textvariable Xscale     -width 5
pack  $wr.stepe.lab1 $wr.stepe.ent1 $wr.stepe.lab2 $wr.stepe.ent2 \
      -side left -padx 1

label $wr.stepf.lab1 -text "Guess uncertainty (frac):"
entry $wr.stepf.ent1 -textvariable ubf  -width 5
label $wr.stepf.lab2 -text "+constant:"
entry $wr.stepf.ent2 -textvariable uba  -width 5
pack  $wr.stepf.lab1 $wr.stepf.ent1 $wr.stepf.lab2 $wr.stepf.ent2 \
      -side left -padx 1

label $wr.stepg.lab1 -text "Data uncertainty  (frac):"
entry $wr.stepg.ent1 -textvariable uof   -width 5
label $wr.stepg.lab2 -text "+constant:"
entry $wr.stepg.ent2 -textvariable uoa   -width 5
pack  $wr.stepg.lab1 $wr.stepg.ent1 $wr.stepg.lab2 $wr.stepg.ent2 \
      -side left -padx 1

label $wr.steph.lab -text "Solution bounds:"
radiobutton $wr.steph.d0 -text "none " -variable bounds -value "0"
radiobutton $wr.steph.d1 -text "lower" -variable bounds -value "1"
radiobutton $wr.steph.d2 -text "both " -variable bounds -value "2"
radiobutton $wr.steph.d3 -text "upper" -variable bounds -value "3"
pack $wr.steph.lab -side left 
pack $wr.steph.d0 $wr.steph.d1 $wr.steph.d2 $wr.steph.d3 -side left

label $wr.stepi.lab -text " Log Scaling:"
checkbutton $wr.stepi.out -variable LN_X -text "Solution" -background grey
checkbutton $wr.stepi.inp -variable LN_Y -text "TCM results" -background grey
pack $wr.stepi.lab $wr.stepi.out $wr.stepi.inp -padx 2 -side left 

#-------------------------------------
# Step6 Solve the coefficient matrix and display results

label  $wr.step6.lab -width 50 -bg Gray65 -anchor w \
      -text "Step 6: Solve the equations and display results"
button $wr.step6.but  -text "Solve" -width 8 -command srm_lgsolve
pack   $wr.step6.lab $wr.step6.but -side left -padx 5

label $wr.stepj.lab1 -text "Output File:"
entry $wr.stepj.ent1 -textvariable srm_vector -relief sunken -width 46
pack  $wr.stepj.lab1 $wr.stepj.ent1 -side left -padx 5

#----------------------
# bottom action buttons

button $wr.bot.dismiss  -bg red -text Quit -width 28 -command "destroy $wr"
button $wr.bot.help -text Help -width 28 \
       -command "load_html [file join $html_dir S437.htm ] "
pack   $wr.bot.dismiss $wr.bot.help -side left -padx 8

set_defaultlg
}

#------------------------------------------------------------------------------
# create the transfer coefficient matrix with the last column as measurements

proc srm_matrix2 {} {

global row col
global srm_flist srm_matrix name_datem
global X_dir tcl_platform exec_dir
global Cscale Species Level Half

if {"$name_datem" == ""} {
     msg_box " Step 2 measured data file not defined! "
} else {
    if [file exists $name_datem] {
       if {[file size $name_datem] == 0} {
          msg_box " Contents of Step 2 file ${name_datem} empty! "
       } else {

# valid measured data; create C2ARRAY.CSV

set arg1 -i  ; append arg1 $srm_flist
set arg2 -m  ; append arg2 $name_datem
set arg3 -o  ; append arg3 $srm_matrix
set arg4 -p  ; append arg4 $Species
set arg5 -z  ; append arg5 $Level
set arg6 -c  ; append arg6 $Cscale
set arg7 -h  ; append arg7 $Half

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
      set code "exec $exec_dir/c2array $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7" 
   } else {
      set code "exec $exec_dir/c2array $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7" 
#     set code "exec $X_dir/xterm -fn fixed -e $exec_dir/c2array $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7"
   }
} else {
   set code "exec $exec_dir/c2array.exe $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7"
}

# run c2array for the CSV and TXT files

   if [catch $code result] {
      msg_box "CSV file error: $result"
   } else {
      set f [open c2array.txt w]
      puts $f $result
      close $f
      msg_box "CSV $result"
   }

# determine the number of rows and columns from c2array stdout (c2array.txt)

  set f [open c2array.txt r]
  gets $f cline
  close $f

  set mark [string first : $cline]
  set cline [string range $cline $mark+1 end]
  set cline [string trim $cline]
  set clist [split $cline]

  set row [lindex $clist 0]
  set col [lindex $clist [llength $clist]-1]

# end of C2ARRAY.CSV creation section

       }
    } else {
        msg_box " Step 2 file ${name_datem} does not exist! "
    }
}
}

#------------------------------------------------------------------------------
# solve and show results 

proc srm_lgsolve {} {

global log 
global srm_vector
global X_dir tcl_platform exec_dir

if [file exists SOURCE_OUT_000] {
    file delete -force SOURCE_OUT_000}
if [file exists RPT_OUT_000] {
    file delete -force RPT_OUT_000}
if [file exists CONC_OUT_000] {
    file delete -force CONC_OUT_000}
if [file exists Iterate_000] {
    file delete -force Iterate_000}

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
   exec $exec_dir/lbfgsb 000
   } else {
   exec $exec_dir/lbfgsb 000
#  exec $X_dir/xterm -fn fixed -e $exec_dir/lbfgsb 000
   }
} else {
   exec $exec_dir/lbfgsb.exe 000
}

file copy -force SOURCE_OUT_000 $srm_vector

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

proc set_defaultlg {} {

global row col
global Cscale Level Species Half lbfgsb_init
global name_datem srm_flist srm_matrix srm_vector 
global bckg_const LN_X LN_Y bounds Xscale uof uoa ubf uba

if [ info exists lbfgsb_init ] { } else {
   set lbfgsb_init ""
}

if { $lbfgsb_init == "" } {
   set lbfgsb_init "yes"
   set srm_flist "cdump"
   if { $name_datem == "" } {set name_datem "measured.txt"}
   if { $srm_matrix == "" } {set srm_matrix "c2array.csv"}
   if { $srm_vector == "" } {set srm_vector "source.txt"}
   if { $Cscale == "" } {set Cscale 1.0}
   set Level 1
   set Species 1
   set Half 0.0
   set bckg_const 1.0
   set LN_X 0
   set LN_Y 1
   set bounds 0
   set Xscale 1.0
   set uof 1.0
   set uoa 0.1
   set ubf 1.0
   set uba 0.1
   set row 0
   set col 0
   }
}

#------------------------------------------------------------------------------
# create file listing (INFILE) using default file name as the wildcard

proc srm_infile2  {} {

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


#------------------------------------------------------------------------------
proc save_parms {} {

global row col srm_matrix bckg_const LN_X LN_Y bounds Xscale uof uoa ubf uba

set LNX .false.
set LNY .false.

if {$LN_X == 1} {set LNX .true.}
if {$LN_Y == 1} {set LNY .true.}

if {$row == 0 || $col == 0} {
   msg_box " Number of rows or columns not set, run C2ARRAY to set field!"

} else {

set f [open "PARAMETER_IN_000" w]

puts $f " &DIMENSIONS"
puts $f "  N_ctrl = $col,"
puts $f "  Nx_ctrl = $col,"
puts $f "  Ny_ctrl = 1,"
puts $f " /"

puts $f " &TCM_INPUTS"
puts $f "  CSV_IN = '$srm_matrix',"
puts $f "  N_obs = $row,"
puts $f " /"

puts $f " &RUN_CONTRL"
puts $f "  bckg_const = $bckg_const,"
puts $f "  APRIORI = 'in.dat',"
puts $f "  LN_X = $LNX,"
puts $f "  LN_Y = $LNY,"
puts $f " /"

puts $f " &SMOOTH_PNT"
puts $f "  Smoother=.true.,"
puts $f "  c_smooth=1D-6,"
puts $f " /"

puts $f " &SMOOTH_P2D"
puts $f "  Smooth2D=.false.,"
puts $f "  x_smooth=1.0,"
puts $f "  y_smooth=1.0,"
puts $f " /"

puts $f " &MODEL_UNC"
puts $f "  UNC_Model=.false.,"
puts $f "  T_model_unc=1.0,"
puts $f "  Floor_x=1.0,"
puts $f "  Ceiling_x=1.0,"
puts $f "  Floor_y=1.0,"
puts $f " /"

puts $f " &LBFGS_CTRL"
puts $f "  lbfgs_m = 7,"
puts $f "  lbfgs_iprint=1,"
puts $f "  lbfgs_factor=1.,"
puts $f "  lbfgs_pgtol=1.0d-36,"
puts $f "  Max_iterations=500,"
puts $f "  f_factor_stop=1e-16,"
puts $f " /"

puts $f " &BOUNDS_ARR"
puts $f "  lbfgs_nbd_c=$bounds,"
puts $f " /"

puts $f " &UNCERTANTY"
puts $f "   X_Scaling=$Xscale,"
puts $f "   Unc_o_f=$uof,"
puts $f "   Unc_o_a=$uoa,"
puts $f "   Unc_b_f=$ubf,"
puts $f "   Unc_b_a=$uba,"
puts $f " /"

close $f
}
}


