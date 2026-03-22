proc con2rem {} {
#------------------------------------------------------------------------------
# CON2REM.TCL: convert HYSPLIT output to radiological dose
# Last Revised: 01 Apr 2011 - initial version
#               17 Apr 2013 - force input file
#               02 Jul 2013 - revised interface, more options
#               07 Jul 2014 - correct force name entry for multiple inputs
#               28 Jul 2017 - test for Dose_file for initialization
#               13 Dec 2019 - replace concplot and concplot.exe with concplot.py
#               16 Jan 2020 - add option to use FORTRAN or Python trajplot.
#               26 Jul 2021 - use svg instead of postscript
#               04 Apr 2022 - add -81 option to concplot to create plot(s)
#                             even when all values are zero.
#               07 Nov 2023 - change street maps to OPEN_STREET and OPEN_TOPO.
#------------------------------------------------------------------------------

global Grid_dir Grid_name Num_disp Force_name
global Activity Dose_file Fuel Process Yield Watts 
global Atype Dtype Drate Decay Dunit Species Extend
global html_dir 
global PyDebug PySource_time_zone PyStreet_map PyOutput_format
global PyPreferred

if [winfo exists .con2rem] {destroy .con2rem}
set wr .con2rem
toplevel $wr
wm title $wr " Convert Unit Concentrations to Dose "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.mid0
frame $wr.midD
frame $wr.mid1
frame $wr.div0
frame $wr.midC
frame $wr.div1
frame $wr.mid2
frame $wr.mid3
frame $wr.midB
frame $wr.mid4
frame $wr.mid5
frame $wr.midA
frame $wr.mid9
frame $wr.div2
frame $wr.bot

ttk::notebook $wr.lang
frame $wr.lang.py
frame $wr.lang.f
$wr.lang add $wr.lang.f -text "Fortran"
$wr.lang add $wr.lang.py -text "Python"

pack $wr.top -padx 2 -pady 4
pack $wr.mid0 $wr.midD $wr.mid1 $wr.div0 $wr.midC $wr.div1 $wr.mid2 -pady 4
pack $wr.mid3 $wr.midB $wr.mid4 $wr.mid5
pack $wr.midA $wr.mid9 $wr.div2 $wr.lang $wr.bot -pady 4
pack configure $wr.lang -expand true -fill x -padx 10

#-->information

label $wr.top.lab -fg blue -relief raised -justify left -wraplength 5i\
 -text "Convert the binary concentration file to an equivalent binary\
 file with the radiological dose equivalent. The original calculation\
 assumes a unit source, and the actual activity and dose conversions\
 are applied in this step using the species defined in activity.txt."
pack $wr.top.lab

#-->select binary hysplit input file and conversion factor

label $wr.mid0.lab1 -text "  Input Data: "
pack $wr.mid0.lab1 -side left -padx 2 
set d 0
foreach item $Grid_name {
   radiobutton $wr.mid0.$d -variable Num_disp -text $item -value $d -background grey
   pack $wr.mid0.$d -side left -padx 4
   incr d
}
entry $wr.mid0.ent -textvariable Force_name -relief sunken -width 16
label $wr.mid0.txt -text "  Force: "
pack  $wr.mid0.txt $wr.mid0.ent -side left 

label $wr.midD.lab1 -text "         Existing Activity file:"
entry $wr.midD.ent1 -textvariable Activity -width 16
pack  $wr.midD.lab1  $wr.midD.ent1 -side left -padx 6

button $wr.mid1.but -bg yellow -text "Create New" -width 18 -command {dose_makef}
radiobutton $wr.mid1.d1 -variable Atype -text "Detonation" -value 0 
radiobutton $wr.mid1.d2 -variable Atype -text "Reactor" -value 1
pack  $wr.mid1.but $wr.mid1.d1 $wr.mid1.d2 -side left -padx 6 

label $wr.div0.lab \
-text "_____________________________________________________________"
pack $wr.div0.lab -side left

label $wr.midC.lab1 -text "Customize: "
button $wr.midC.but1  -text "Detonation" -width 15 -command {dose_setdet}
button $wr.midC.but2  -text "Reactor"    -width 15 -command {dose_setnuc}
pack  $wr.midC.lab1  $wr.midC.but1 $wr.midC.but2 -side left -padx 6

label $wr.div1.lab \
-text "_____________________________________________________________"
pack $wr.div1.lab -side left

label $wr.mid2.lab1 -text "Output Data File Name: "
entry $wr.mid2.ent1 -textvariable Dose_file -width 12
pack  $wr.mid2.lab1 $wr.mid2.ent1 -side left

label $wr.mid3.lab1 -text "Output Type: "
radiobutton $wr.mid3.d1 -variable Dtype -text "Dose" -value 0 
radiobutton $wr.mid3.d2 -variable Dtype -text "Concentration" -value 1
pack $wr.mid3.lab1 $wr.mid3.d1 $wr.mid3.d2 -side left

radiobutton $wr.midB.d1 -variable Species -text "Sum species" -value 0 
radiobutton $wr.midB.d2 -variable Species -text "Match Input" -value 1
radiobutton $wr.midB.d3 -variable Species -text "Output species" -value 2
pack $wr.midB.d1 $wr.midB.d2 $wr.midB.d3 -side left

label $wr.mid4.lab1 -text "Dose Type: "
radiobutton $wr.mid4.d1 -variable Drate -text "Rate" -value 0 
radiobutton $wr.mid4.d2 -variable Drate -text "Total" -value 1
pack $wr.mid4.lab1 $wr.mid4.d1 $wr.mid4.d2 -side left

label $wr.mid5.lab1 -text "Dose Units: "
radiobutton $wr.mid5.d1 -variable Dunit -text "Rem" -value 0 
radiobutton $wr.mid5.d2 -variable Dunit -text "Sv" -value 1
pack $wr.mid5.lab1 $wr.mid5.d1 $wr.mid5.d2 -side left

label $wr.midA.lab1 -text "Apply Decay:"
radiobutton $wr.midA.d1 -variable Decay -text "No"     -value 0 
radiobutton $wr.midA.d2 -variable Decay -text "Yes   " -value 1
entry $wr.midA.ent1 -textvariable Extend -width 4
label $wr.midA.lab2 -text " hrs extra decay"
pack  $wr.midA.lab1 $wr.midA.d1 $wr.midA.d2 $wr.midA.ent1 $wr.midA.lab2 -side left

button $wr.mid9.run -bg yellow -text "Create Binary Dose File" -width 50 -command {run_dose}
pack   $wr.mid9.run -side left

#-->termination

label $wr.div2.lab \
-text "_____________________________________________________________"
pack $wr.div2.lab -side left

#-->Python options
label $wr.lang.f.lab -text "Output will be in SVG"
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
checkbutton $wr.lang.py.misc.source_tz -variable PySource_time_zone -text "Source time zone" -background grey
checkbutton $wr.lang.py.misc.debug -variable PyDebug -text "Debug messages" -background grey
pack $wr.lang.py.misc.source_tz $wr.lang.py.misc.debug -side left -padx 5

pack $wr.lang.py.row1 $wr.lang.py.misc -side top -pady 5 -padx 10

if { $PyPreferred } {
    $wr.lang select $wr.lang.py
} else {
    $wr.lang select $wr.lang.f
}

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 12 -command "destroy $wr"
button $wr.bot.help -text Help  -width 12 \
       -command "load_html [file join $html_dir S347.htm ] "
button $wr.bot.save  -bg green -text "Plot Dose" -width 20 -command {dose_plot ".con2rem.lang"}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 6
dose_init
}


#---------------------------------------------------------------------
proc run_dose {} {

global Activity Dose_file Fuel Process Yield Watts 
global Dtype Drate Decay Dunit Species Extend
global Grid_dir Grid_name Num_disp Force_name
global X_dir tcl_dir exec_dir tcl_platform

set log [ScrollText .msg]
$log configure -cursor watch

if { "$Force_name" == "" } {
   set Grid_dir_n  [lindex $Grid_dir $Num_disp]
   set Grid_name_n [lindex $Grid_name $Num_disp]
   set arg2 -i  ; append arg2 $Grid_dir_n$Grid_name_n

   if [file exists $Grid_dir_n$Grid_name_n] { } else {
      msg_box "ERROR: conversion requires file: $Grid_dir_n$Grid_name_n!"
      return
   }
} else {
   set arg2 -i  ; append arg2 $Force_name
   if [file exists $Force_name] { } else {
      msg_box "ERROR: conversion requires file: $Force_name!"
      return
   }
}

set arg1 -a  ; append arg1 $Activity
set arg3 -o  ; append arg3 $Dose_file
set arg4 -f  ; append arg4 $Fuel
set arg5 -p  ; append arg5 $Process
set arg6 -y  ; append arg6 $Yield
set arg7 -w  ; append arg7 $Watts
set arg8 -c  ; append arg8 $Dtype
set arg9 -d  ; append arg9 $Drate
set arg0 -t  ; append arg0 $Decay
set argA -s  ; append argA $Species
set argB -x  ; append argB $Extend
set argC -q  ; append argC $Dunit


if [file exists $Activity] { } else {
   msg_box "ERROR: conversion requires file: activity.txt!"
   return
}

# msg_box "$arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8 $arg9 $arg0"
# tkwait window .msg_win

if [ file exists STDOUT ] {file delete STDOUT} 
if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
   exec $exec_dir/con2rem $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8 \
                          $arg9 $arg0 $argA $argB $argC >STDOUT
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/con2rem $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8 \
                                                    $arg9 $arg0 $argA $argB $argC >STDOUT
   }
} else {
   exec $exec_dir/con2rem.exe $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8 \
                              $arg9 $arg0 $argA $argB $argC >STDOUT
}

if [file exists STDOUT] {
   set fid [open "STDOUT" r]
   while {[eof $fid] != 1} {
      gets $fid cline
      $log insert end $cline\n
   }
   if [file exists $Dose_file] {
      set cline " Created binary output file: $Dose_file"
   } else {
      set cline " Binary output file not created"
   }
   $log insert end $cline\n
   close $fid
   file delete -force STDOUT
}

}

#-----------------------------------------------------------

proc dose_plot {lang} {

global Dose_file Dtype Drate Dunit
global X_dir tcl_dir exec_dir tcl_platform
global PyPreferred
global PyDebug PySource_time_zone PyStreet_map PyOutput_format

set PyPreferred [expr {[$lang select] == "${lang}.py"}]

if { $PyPreferred } {
    set out_file doseplot.${PyOutput_format}
} else {
    set out_file doseplot.html
}

set f [open "LABELS.CFG" w]

if { $Dtype == 0} {
   puts $f "'TITLE&','Radiological Dose&'"
   set arg6 -x  ; append arg6 1000.0
   set arg7 -y  ; append arg7 1000.0

   if { $Dunit == 0} {
      if { $Drate == 0} {
         puts $f "'MAPID&','Dose Rate &'"
         puts $f "'UNITS&','mR &'"
         puts $f "'VOLUM&','/hr &'"
         set arg8 -v  ; append arg8 100+50+20+10+5+2+1+0.5+0.2+0.1

      } else {
         puts $f "'MAPID&','Accumulated Dose &'"
         puts $f "'UNITS&','mR &'"
         puts $f "'VOLUM&',' &'"
         set arg8 -v  ; append arg8 1000+500+200+100+50+20+10+5+2+1
      }

   } else {
      if { $Drate == 0} {
         puts $f "'MAPID&','Dose Rate &'"
         puts $f "'UNITS&','mSv &'"
         puts $f "'VOLUM&','/hr &'"
         set arg8 -v  ; append arg8 10+5+2+1+0.5+0.2+0.1+0.05+0.02+0.01

      } else {
         puts $f "'MAPID&','Accumulated Dose &'"
         puts $f "'UNITS&','mSv &'"
         puts $f "'VOLUM&',' &'"
         set arg8 -v  ; append arg8 100+50+20+5+2+1+0.5+0.2+0.1
      }
   }

} else {
   puts $f "'TITLE&','Air Concentration/Deposition&'"
   puts $f "'UNITS&','Bq&'"
   set arg6 -x  ; append arg6 1.0
   set arg7 -y  ; append arg7 1.0
   set arg8 -:
}
close $f

set arg1 -i  ; append arg1 $Dose_file
set arg2 -o  ; append arg2 $out_file
set arg3 -g  ; append arg3 0:250
set arg4 -k  ; append arg4 2
set arg5 -z  ; append arg5 95
set arg9 -s  ; append arg9 0

if [file exists $Dose_file] {
    if { $PyPreferred } {
        set arg_ext " "
        set arg_ext2 "-81"
        #if { $View == 1 } { append arg_ext " --interactive" }
        append arg_ext " --interactive"
        if { $PyDebug } { append arg_ext " --debug" }
        if { $PySource_time_zone } { append arg_ext " --source-time-zone" }

        switch -nocase $PyStreet_map {
            OPEN_STREET { append arg_ext " --street-map=0" }
            OPEN_TOPO { append arg_ext " --street-map=1" }
            default { }
        }

        run_python_program "$exec_dir/concplot.py" "$arg_ext $arg_ext2 \
                         $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8 $arg9"
    } else {
        set arg_ext "+g1"
        set arg_ext2 "-81"
        if { "$tcl_platform(platform)" == "unix" } {
            if { "$X_dir" == "" } {
                exec $exec_dir/concplot $arg_ext $arg_ext2 $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8 $arg9
            } else {
                exec $X_dir/xterm -fn fixed -e $exec_dir/concplot $arg_ext $arg_ext2 $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8 $arg9
            }
        } else {
            if [catch {
                exec $exec_dir/concplot.exe $arg_ext $arg_ext2 $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8 $arg9} result] {
                msg_box "ERROR: concentration display file not created!"
            }
        }
    }
} else {
    msg_box "ERROR: dose file not created!"
}

if [file exists LABELS.CFG]  {file delete -force LABELS.CFG}
if { !$PyPreferred && [file exists $out_file] } { svg_box $out_file } 
}


#-----------------------------------------------------------

proc dose_init {} {

global Activity Dose_file Fuel Process Yield Watts Force_name
global Atype Dtype Drate Decay Dunit Species Extend

if [ info exists Dose_file ] { } else {
   set Dose_file ""
}

if { $Dose_file == "" } {
   set Angle 0.0
   set Activity activity.txt
   set Dose_file rdump
   set Atype -1
   set Fuel 1
   set Process 1
   set Yield 1.0
   set Watts 0.0
   set Dtype 0
   set Drate 0
   set Decay 1
   set Dunit 0
   set Species 0
   set Extend 0
   if [ info exists Force_name ] { } else {set Force_name ""}
}
}

#------------------------------------------------------------

proc dose_setdet {} {

global Fuel Process Yield Watts 

if [winfo exists .dosedet_win] {destroy .dosedet_win}
set wr .dosedet_win
toplevel $wr
wm title $wr "Configure Detonation Options"
wm  geometry $wr +100+100

frame $wr.mid6
frame $wr.mid7
frame $wr.mid8
frame $wr.mid9
pack  $wr.mid6 $wr.mid7 $wr.mid8 $wr.mid9 -pady 4 -padx 4

label $wr.mid6.lev -text "Fuel Type: "
pack  $wr.mid6.lev -side left
radiobutton $wr.mid6.d1 -variable Fuel -text "U235" -value 1 
radiobutton $wr.mid6.d2 -variable Fuel -text "P239" -value 2 
pack $wr.mid6.d1 $wr.mid6.d2 -side left

label $wr.mid7.lev -text "   Fission Process:"
pack  $wr.mid7.lev -side left
radiobutton $wr.mid7.d1 -variable Process -text "High-Energy" -value 1 
radiobutton $wr.mid7.d2 -variable Process -text "Thermal" -value 2
pack $wr.mid7.d1 $wr.mid7.d2 -side left

label $wr.mid8.lab1 -text "Yield (kT):"
entry $wr.mid8.ent1 -textvariable Yield -width 6
label $wr.mid8.lab2 -text " or Thermal (MWh):"
entry $wr.mid8.ent2 -textvariable Watts -width 6
pack  $wr.mid8.lab1  $wr.mid8.ent1 $wr.mid8.lab2  $wr.mid8.ent2 -side left

pack $wr.mid6 $wr.mid7 $wr.mid8 -pady 4 -padx 4

button $wr.mid9.dismiss -text Return -width 40 -command "destroy $wr"
pack   $wr.mid9.dismiss -side left -padx 8 
}


#------------------------------------------------------------

proc dose_setnuc {} {

global Yield

if [winfo exists .dosedet_win] {destroy .dosedet_win}
set wr .dosedet_win
toplevel $wr
wm title $wr "Configure Reactor Accident Options"
wm  geometry $wr +100+100

frame $wr.mid8
frame $wr.mid9
pack  $wr.mid8 $wr.mid9 -pady 4 -padx 4

label $wr.mid8.lab1 -text "Fraction of FDNPP Maximum"
entry $wr.mid8.ent1 -textvariable Yield -width 20
pack  $wr.mid8.lab1 $wr.mid8.ent1 -pady 4 -padx 4

button $wr.mid9.dismiss -text Return -width 20 -command "destroy $wr"
pack   $wr.mid9.dismiss -side left -padx 8 
}

#------------------------------------------------------------

proc dose_makef {} {

global X_dir tcl_dir exec_dir tcl_platform
global Atype

if { $Atype == "0" } {
   set arg1 -a  ; append arg1 "create"
} elseif { $Atype == "1" } {
   set arg1 -a  ; append arg1 "fdnpp"
} else {
   msg_box "Activity Selection Required!"
   return
}


if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
   exec $exec_dir/con2rem $arg1
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/con2rem $arg1
   }
} else {
   exec $exec_dir/con2rem.exe $arg1
}


}

