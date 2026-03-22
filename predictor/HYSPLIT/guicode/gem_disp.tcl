proc gem_disp {} {

#------------------------------------------------------------------------------
# GEM_DISP.TCL: color fill plot of concentration grid which is especially
# good to display results on global spanning grids
# Last Revised: 27 Nov 2007 - initial version 
#               06 Nov 2008 - version 4.9 compatibility
#               26 Jul 2021 - use svg instead of postscript
#------------------------------------------------------------------------------

global Gembase Gemdelt Psout_file Frame Num_polid Log_scale Map_delta
global html_dir gemconc Addconc
if [winfo exists .gemdisp] {destroy .gemdisp}
set wr .gemdisp
toplevel $wr
wm title $wr " Color Fill of the Concentration Grid "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.mid1
frame $wr.mid2
frame $wr.mid3
frame $wr.mid4
frame $wr.mid5
frame $wr.mid6
frame $wr.bot
pack $wr.top $wr.mid1 $wr.mid2 $wr.mid3 $wr.mid4 $wr.mid5 $wr.mid6 \
     $wr.bot -side top -pady 5 -padx 10

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "Concentration graphic that color fills each cell of the global\
grid with one of 12 colors. The current version is restricted to one\
level and one pollutant species."
pack $wr.top.lab

#-->select grid

label $wr.mid1.lab -text "Grid To Display: "
entry $wr.mid1.ent -textvariable gemconc -relief sunken -width 15
pack $wr.mid1.lab $wr.mid1.ent -side left -padx 2

#-->output file

label $wr.mid2.lab -text "Output SVG File: "
entry $wr.mid2.ent -textvariable Psout_file -relief sunken -width 15
pack  $wr.mid2.lab $wr.mid2.ent -side left -padx 5

#-->contouring

label $wr.mid3.lab1 -text "Contour base:"
entry $wr.mid3.ent1 -textvariable Gembase -relief sunken -width 10
label $wr.mid3.lab2 -text "   Contour delta:"
entry $wr.mid3.ent2 -textvariable Gemdelt -relief sunken -width 10
pack $wr.mid3.lab1 $wr.mid3.ent1 $wr.mid3.lab2 $wr.mid3.ent2 -side left -padx 1

#-->display options

label $wr.mid4.lab -text "Display Options:"
checkbutton $wr.mid4.box -variable Frame     -text "Frames   " -background grey
checkbutton $wr.mid4.kol -variable Log_scale -text "Log Scale" -background grey
checkbutton $wr.mid4.con -variable Addconc   -text "Add Plume" -background grey
pack $wr.mid4.lab $wr.mid4.box $wr.mid4.kol $wr.mid4.con -side left -padx 5

label $wr.mid5.lab1 -text "Pollutant:"
entry $wr.mid5.ent1 -textvariable Num_polid -relief sunken -width 2
label $wr.mid5.lab2 -text "   Offset longitude:"
entry $wr.mid5.ent2 -textvariable Map_delta  -relief sunken -width 8
pack $wr.mid5.lab1 $wr.mid5.ent1 $wr.mid5.lab2 $wr.mid5.ent2 -side left -padx 5

#-->termination
label $wr.mid6.lab \
-text "________________________________________________________________"
pack $wr.mid6.lab -side left

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help -width 8 \
       -command "load_html [file join $html_dir S338.htm ] "
button $wr.bot.save  -bg green -text "Execute Display" -width 20 -command {run_cgrid}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10
gemdisp_set
}

proc gemdisp_set {} {
global Gembase Gemdelt Psout_file Frame Num_polid Log_scale Map_delta Addconc gemconc

set Psout_file gemplot

if { "$Gembase" == "" } {
   set Gembase 1.0E-36
   set Gemdelt 1000.0
   set Frame 0
   set Num_polid 1
   set Log_scale 1
   set Map_delta 0.0
   set Addconc 0
   set gemconc gemconc.bin
}
}


#------------------------------------------------------------
# run plotting program

proc run_cgrid {} {

global Grid_dir Grid_name
global Gembase Gemdelt Psout_file Frame Num_polid Log_scale Map_delta
global X_dir tcl_platform exec_dir gemconc Addconc

# preprocessor step to merge concentration files
if { $Addconc == 1 } {
   set arg1 -i
   append arg1 [lindex $Grid_dir 0]
   append arg1 [lindex $Grid_name 0]
   set arg2 -b ; append arg2 $gemconc

   if { "$tcl_platform(platform)" == "unix" } {
      if { "$X_dir" == "" } {
         exec $exec_dir/concadd $arg1 $arg2
      } else {
         exec $X_dir/xterm -fn fixed -e $exec_dir/concadd $arg1 $arg2
      }
   } else {
      exec $exec_dir/concadd.exe $arg1 $arg2
   }
   set arg1 -i  ; append arg1 concadd.bin
} else {
   set arg1 -i  ; append arg1 $gemconc
}

set arg2 -l  ; append arg2 $Gembase
set arg3 -d  ; append arg3 $Gemdelt
set arg4 -o  ; append arg4 $Psout_file
set arg5 -m  ; append arg5 $Frame
set arg6 -s  ; append arg6 $Num_polid
set arg7 -a  ; append arg7 $Log_scale
set arg8 -x  ; append arg8 $Map_delta

set arg_ext "+g1"

if { "$tcl_platform(platform)" == "unix" } {

   if { "$X_dir" == "" } {
   exec $exec_dir/gridplot $arg_ext $arg1 $arg2 $arg3 \
        $arg4 $arg5 $arg6 $arg7 $arg8
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/gridplot $arg_ext $arg1 $arg2 $arg3 \
        $arg4 $arg5 $arg6 $arg7 $arg8
   }
} else {
   exec $exec_dir/gridplot.exe $arg_ext $arg1 $arg2 $arg3 \
        $arg4 $arg5 $arg6 $arg7 $arg8
}

if { $Frame == 1 } {
   if [file exists ${Psout_file}_001.html] {
      foreach f [glob ${Psout_file}_???.html] {
         svg_box $f
      }
   } else {
      msg_box "${Psout_file}_001.html not found!" 
   }
} else {
   if [file exists ${Psout_file}.html] {
      svg_box ${Psout_file}.html
   } else {
      msg_box "${Psout_file}.html not found!" 
   }
}
}
