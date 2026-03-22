proc conc_2asc {} {

#------------------------------------------------------------------------------
# CONC_ASC.TCL: convert binary concentration file to ascii 
# Last Revised: 09 Jul 2002
#               13 Aug 2002
#               10 Jan 2005 - simplify xterm shell
#               04 May 2007 - more command line options
#               07 Nov 2008 - version 4.9 update
#               17 Apr 2013 - force input file name
#               07 Jul 2014 - correct force name entry for multiple inputs
#               28 Jul 2017 - defined unique initialization variable
#               02 Sep 2020 - test for already defined Cscale and Dscale
#------------------------------------------------------------------------------

global asc_all asc_del asc_txt asc_zro asc_dig asc_min Cscale Dscale
global html_dir Grid_name Num_disp Force_name
if [winfo exists .concasc] {destroy .concasc}
set wr .concasc
toplevel $wr
wm title $wr " Convert Binary Concentration to ASCII  "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.mid1
frame $wr.mid3
frame $wr.mid4
frame $wr.mid5
frame $wr.mid6
frame $wr.bot
pack $wr.top $wr.mid1 $wr.mid3 $wr.mid4 $wr.mid5 $wr.mid6 \
     $wr.bot -side top -pady 5 -padx 5

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "Converts all non-zero grid points from the selected binary\
concentration file to an ASCII file for each sampling time period.\
Output files are named according the input file and day_hour of the\
sampling start time: \\working\\infile_day_hour."
pack $wr.top.lab

#-->select grid
label $wr.mid1.lab -text "Grid To convert:"
pack $wr.mid1.lab -side left -padx 5
set d 0
foreach item $Grid_name {
   radiobutton $wr.mid1.$d -variable Num_disp -text $item -value $d -background grey
   pack $wr.mid1.$d -side left -padx 2
   incr d
}
entry $wr.mid1.ent -textvariable Force_name -relief sunken -width 15
label $wr.mid1.txt -text "  Force: "
pack  $wr.mid1.txt $wr.mid1.ent -side left 

label $wr.mid3.lab -fg blue -text "File Options"
pack $wr.mid3.lab 
checkbutton $wr.mid3.d0 -text "Single File"  -variable asc_all -background grey
checkbutton $wr.mid3.d1 -text "Minimum Text" -variable asc_txt -background grey
checkbutton $wr.mid3.d2 -text "Include Zero" -variable asc_zro -background grey
pack $wr.mid3.d0 $wr.mid3.d1 $wr.mid3.d2 -side left -padx 2

label $wr.mid4.lab -fg blue -text "Precision Options"
pack $wr.mid4.lab 
checkbutton $wr.mid4.d0 -text "Extended Digits" -variable asc_dig -background grey
checkbutton $wr.mid4.d1 -text "Include Minutes" -variable asc_min -background grey
checkbutton $wr.mid4.d2 -text "Comma Delimited" -variable asc_del -background grey
pack $wr.mid4.d0 $wr.mid4.d1 $wr.mid4.d2  -side left -padx 2

label $wr.mid5.lab -fg blue -text "Conversion Options"
pack $wr.mid5.lab 
label $wr.mid5.t1 -text "Concentration:"
entry $wr.mid5.e1 -textvariable Cscale -width 10
label $wr.mid5.t2 -text "       Deposition:"
entry $wr.mid5.e2 -textvariable Dscale -width 10
pack $wr.mid5.t1 $wr.mid5.e1 $wr.mid5.t2 $wr.mid5.e2 -side left

#-->termination
label $wr.mid6.lab \
-text "_____________________________________________________________"
pack $wr.mid6.lab -side left

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help -width 8 \
       -command "load_html [file join $html_dir S341.htm ] "

button $wr.bot.save  -bg green -text "Execute Conversion" -width 20 -command {run_conc_cnvrt}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 

c2a_init
}

#--------------------------------------------------------------------------
proc run_conc_cnvrt {} {
global asc_all asc_del asc_txt asc_zro asc_dig asc_min Cscale Dscale
global X_dir Grid_dir Grid_name tcl_dir exec_dir
global Num_disp Bot Top Lev tcl_platform Force_name
global HiRes Color Fixed

if { "$Force_name" == "" } {
   set Grid_dir_n [lindex $Grid_dir $Num_disp]
   set Grid_name_n  [lindex $Grid_name $Num_disp]
   set arg0 -i  ; append arg0 $Grid_dir_n$Grid_name_n
} else {
   set arg0 -i  ; append arg0 $Force_name
}

set arg7 -u  ; append arg7 $Cscale
set arg8 -U  ; append arg8 $Dscale  

set arg1 "-:"
set arg2 "-:"
set arg3 "-:" 
set arg4 "-:"
set arg5 "-:"
set arg6 "-:" 

if { $asc_all == 1 } {set arg1 -s}
if { $asc_del == 1 } {set arg2 -d} 
if { $asc_txt == 1 } {set arg3 -m}
if { $asc_zro == 1 } {set arg4 -z}
if { $asc_dig == 1 } {set arg5 -x} 
if { $asc_min == 1 } {set arg6 -t}

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
   exec $exec_dir/con2asc $arg0 $arg1 $arg2 $arg3 $arg4 \
        $arg5 $arg6 $arg7 $arg8
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/con2asc $arg0 $arg1 $arg2 $arg3 $arg4 \
        $arg5 $arg6 $arg7 $arg8
   }
} else {
   exec $exec_dir/con2asc.exe $arg0 $arg1 $arg2 $arg3 $arg4 \
        $arg5 $arg6 $arg7 $arg8
}
destroy .concasc
}

#--------------------------------------------------------------------------
proc c2a_init {} {
global asc_all asc_del asc_txt asc_zro asc_dig asc_min 
global Cscale Dscale Force_name asc_init

if [ info exists asc_init ] { } else {
   set asc_init ""
}

if { $asc_init == "" } {
   set asc_init "yes"
   set asc_all 0
   set asc_del 0
   set asc_txt 0 
   set asc_zro 0 
   set asc_dig 0
   set asc_min 0
   if {"$Cscale" == ""} {set Cscale 1.0}
   if {"$Dscale" == ""} {set Dscale 1.0}
   if [ info exists Force_name ] { } else {set Force_name ""}
}
}
