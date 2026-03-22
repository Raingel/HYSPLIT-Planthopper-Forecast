proc par_shift {} {
#-----------------------------------------------------------------------------
# PAR_SHIFT.TCL: particle shifting
# Last Revised: 28 May 2010
#               06 Aug 2018 - added reset button
#               26 Jul 2021 - use svg instead of postscript
#-----------------------------------------------------------------------------
if [winfo exists .parshift] {destroy .parshift}

global html_dir ainpf aoutf suffix 
global adj_time adj_mo adj_da adj_hr
global Rotate Rangl Rdist Rlat Rlon
global Blend Dlat Dlon Lat1 Lon1 Lat2 Lon2

set wr .parshift
toplevel $wr
wm title $wr " Particle Position Shift Utility "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.data
frame $wr.mid0
frame $wr.mid1
frame $wr.mid2
frame $wr.div1
frame $wr.mid3
frame $wr.mid4
frame $wr.mid5
frame $wr.div2
frame $wr.mid6
frame $wr.mid7
frame $wr.div3
frame $wr.bot

pack $wr.top $wr.data $wr.mid0 $wr.mid1 $wr.mid2 $wr.div1 -side top -pady 2
pack $wr.mid3 $wr.mid4 $wr.mid5 $wr.div2 -side top -pady 2
pack $wr.mid6 $wr.mid7 $wr.div3 -side top -pady 2
pack $wr.bot -side top -pady 10

#-->information

label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
-text "Spatial adjustment of the particle positions in the HYSPLIT particle position\
file. The position adjustment is only applied for one time period. The shift is\
specified in either delta latitude-longitude units or as a rotation and distance\
from a point. Adjustments outside of the zone may be linearly blended to zero."

pack $wr.top.lab

arl_set

#-->particle input-output files

label $wr.data.lab1 -text "Input:"
entry $wr.data.ent1 -textvariable ainpf -relief sunken -width 10
button $wr.data.win1  -text Browse -width 6 -command {
   set temp1 [tk_getOpenFile -title "Input File Selection"]
   if {[string length $temp1] > 0} {set ainpf $temp1}}
pack $wr.data.lab1 $wr.data.ent1 $wr.data.win1 -side left -padx 2

label $wr.data.lab2 -text " Output:"
entry $wr.data.ent2 -textvariable aoutf -relief sunken -width 10
button $wr.data.win2  -text Browse -width 6 -command {
   set temp [tk_getOpenFile -title "Output File Selection"]
   if {[string length $temp2] > 0} {set aoutf $temp2}}
pack $wr.data.lab2 $wr.data.ent2 $wr.data.win2 -side left -padx 2

label $wr.data.lab3 -text " Suffix:"
checkbutton $wr.data.ent3 -text ".###" -variable suffix -background grey 
pack $wr.data.lab3 $wr.data.ent3 -side left -padx 2

#-->set process time

label $wr.mid1.lab0 -text "Process Time:"
radiobutton $wr.mid1.d0 -text "First " -variable adj_time -value "0" 
radiobutton $wr.mid1.d1 -text "Set value =" -variable adj_time -value "1" 
pack  $wr.mid1.lab0 $wr.mid1.d0 $wr.mid1.d1 -side left

label $wr.mid1.lab1 -text " Month:"
entry $wr.mid1.ent1 -textvariable adj_mo -relief sunken -width 3
pack  $wr.mid1.lab1 $wr.mid1.ent1 -side left

label $wr.mid1.lab2 -text " Day:"
entry $wr.mid1.ent2 -textvariable adj_da -relief sunken -width 3
pack  $wr.mid1.lab2 $wr.mid1.ent2 -side left

label $wr.mid1.lab3 -text " Hour:"
entry $wr.mid1.ent3 -textvariable adj_hr -relief sunken -width 3
pack  $wr.mid1.lab3 $wr.mid1.ent3 -side left

#-->choose which adjustment to perform

label $wr.mid2.lab -text "Adjustment Type:"
radiobutton $wr.mid2.d0 -text "Window"   -variable Rotate -value "0" 
radiobutton $wr.mid2.d1 -text "Rotation   " -variable Rotate -value "1" 
checkbutton $wr.mid2.d3 -text "Blend" -variable Blend -background grey 
pack $wr.mid2.lab $wr.mid2.d0 $wr.mid2.d1 $wr.mid2.d3 -side left

#-->select adjustment window limits

label $wr.div1.lab -fg blue -text "------------------------ Window Parameters ------------------------" 
pack  $wr.div1.lab -side left

label $wr.mid3.lab1 -text " Shift Latitude:"
entry $wr.mid3.ent1 -textvariable Dlat -relief sunken -width 6
pack  $wr.mid3.lab1 $wr.mid3.ent1 -side left

label $wr.mid3.lab2 -text " Shift Longitude:"
entry $wr.mid3.ent2 -textvariable Dlon -relief sunken -width 6
pack  $wr.mid3.lab2 $wr.mid3.ent2 -side left

label $wr.mid4.lab1 -text "Lower Left Latitude:"
entry $wr.mid4.ent1 -textvariable Lat1 -relief sunken -width 6
pack  $wr.mid4.lab1 $wr.mid4.ent1 -side left

label $wr.mid4.lab2 -text "      Upper Right Latitude:"
entry $wr.mid4.ent2 -textvariable Lat2 -relief sunken -width 6
pack  $wr.mid4.lab2 $wr.mid4.ent2 -side left

label $wr.mid5.lab1 -text "          Longitude:"
entry $wr.mid5.ent1 -textvariable Lon1 -relief sunken -width 6
pack  $wr.mid5.lab1 $wr.mid5.ent1 -side left

label $wr.mid5.lab2 -text "                 Longitude:"
entry $wr.mid5.ent2 -textvariable Lon2 -relief sunken -width 6
pack  $wr.mid5.lab2 $wr.mid5.ent2 -side left

#-->select rotation parameters

label $wr.div2.lab -fg blue -text "----------------------- Rotation Parameters -----------------------" 
pack  $wr.div2.lab -side left

label $wr.mid6.lab1 -text "Rotation Angle (deg):"
entry $wr.mid6.ent1 -textvariable Rangl -relief sunken -width 8
pack  $wr.mid6.lab1 $wr.mid6.ent1 -side left

label $wr.mid6.lab2 -text " Distance (km):"
entry $wr.mid6.ent2 -textvariable Rdist -relief sunken -width 9
pack  $wr.mid6.lab2 $wr.mid6.ent2 -side left

label $wr.mid7.lab1 -text "From latitude:"
entry $wr.mid7.ent1 -textvariable Rlat -relief sunken -width 8
pack  $wr.mid7.lab1 $wr.mid7.ent1 -side left

label $wr.mid7.lab2 -text "  Longitude:"
entry $wr.mid7.ent2 -textvariable Rlon -relief sunken -width 9
pack  $wr.mid7.lab2 $wr.mid7.ent2 -side left

#-->bottom action buttons

label $wr.div3.lab -fg blue -text "___________________________________________________________________" 
pack  $wr.div3.lab -side left

button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help  -width 8 \
       -command "load_html [file join $html_dir S346.htm ] "
button $wr.bot.reset -bg green -text "Reset" -width 8 -command {unset suffix; arl_set}
button $wr.bot.save  -bg green -text "Process Points" -width 20 -command {run_parshift}

pack  $wr.bot.dismiss $wr.bot.help $wr.bot.reset $wr.bot.save -side left -padx 10 
}


##################################################
#-->set directory defaults

proc arl_set {} {

global Start_locn
global pinpf poutf ainpf aoutf suffix 
global adj_time adj_mo adj_da adj_hr
global Rotate Rangl Rdist Rlat Rlon
global Blend Dlat Dlon Lat1 Lon1 Lat2 Lon2

set date [clock format [clock scan now] -format "%y %m %d %H"]

if [ info exists suffix ] {
} else {

  if [ info exists pinpf ] {
     if {"$pinpf" == ""} {
        set ainpf "PARDUMP"
     } else {
        set ainpf $poutf
     }
  } else {
     set ainpf "PARDUMP"
  }

  if [ info exists poutf ] {
     if {"$poutf" == ""} {
        set aoutf "PARINIT"
     } else {
        set aoutf $pinpf
     }
  } else {
     set aoutf "PARINIT"
  }

  set suffix 0
  set adj_time 0
  set adj_mo [lindex [split $date] 1]
  set adj_da [lindex [split $date] 2]
  set adj_hr [lindex [split $date] 3]

  set Blend 0
  set Dlat "0.0"
  set Dlon "0.0"
  set Lat1 "-90.0"
  set Lon1 "-180.0"
  set Lat2 "90.0"
  set Lon2 "180.0"

  set Rotate 0
  set Rangl "0.0"
  set Rdist "0.0"

  if [ info exists Start_locn ] {
     set Rlat [lindex $Start_locn(1) 0]
     set Rlon [lindex $Start_locn(1) 1]
  } else {
     set Rlat 0.0
     set Rlon 0.0
  }
}
}


##################################################
#-->run parshift program

proc run_parshift {} {
global exec_dir tcl_platform X_dir
global ainpf aoutf suffix 
global adj_time adj_mo adj_da adj_hr
global Rotate Rangl Rdist Rlat Rlon
global Blend Dlat Dlon Lat1 Lon1 Lat2 Lon2

if [file exists $ainpf] {
   if {[file size $ainpf] == 0} {
      msg_box "Input file empty: $ainpf\n"
      destroy .parshift
      return
   }
} else {
   msg_box "Input file not found: $ainpf\n"
   destroy .parshift
   return
}

if [file exists $aoutf] {file delete $aoutf}

# USAGE: parshift -[options (default)]
#  -b[blend shift outside of the window]
#  -i[input base file name (PARDUMP)]
#  -o[output base file name (PARINIT)]
#  -r[rotation degrees:kilometers:latitude:longitude]
#  -s[search for multiple files with .000 suffix]
#  -t[time MMDDHH (missing process first time only)]
#  -w[window corner lat1:lon1:lat2:lon2 (-90.0:-180.0:90.0:180.0)]
#  -x[delta longitude (0.0)]
#  -y[delta latitude  (0.0)]

#-->decoder command line arguments

  set hyph ":"
  if {$Blend == 1} {set argb -b} else {set argb "-:"}
  set argi -i  ; append argi $ainpf
  set argo -o  ; append argo $aoutf
  set argr -r  ; append argr $Rangl$hyph$Rdist$hyph$Rlat$hyph$Rlon
  if {$suffix == 1} {set args -s} else {set args "-:"}
  if {$adj_time == 1} {set argt -t$adj_mo$adj_da$adj_hr} else {set argt "-:"}
  set argw -w  ; append argw $Lat1$hyph$Lon1$hyph$Lat2$hyph$Lon2
  set argx -x  ; append argx $Dlon
  set argy -y  ; append argy $Dlat

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
      if {$Rotate == 1} {
         exec $exec_dir/parshift $argi $argo $argr $argb $args $argt
      } else {
         exec $exec_dir/parshift $argi $argo $argw $argx $argy $argb $args $argt  
      }

   } else {
      if {$Rotate == 1} {
         exec $X_dir/xterm -fn fixed -e $exec_dir/parshift $argi $argo $argr $argb $args $argt
      } else {
         exec $X_dir/xterm -fn fixed -e  $exec_dir/parshift $argi $argo $argw $argx $argy $argb $args $argt
      }
   }

} else {
   if {$Rotate == 1} {
      exec $exec_dir/parshift.exe $argi $argo $argr $argb $args $argt
   } else {
      exec $exec_dir/parshift.exe $argi $argo $argw $argx $argy $argb $args $argt
   }

}


if [file exists $aoutf] {
  set arg_ext "+g1"
  if { "$tcl_platform(platform)" == "unix" } {
     if { "$X_dir" == "" } {
        exec $exec_dir/parhplot $arg_ext -i$aoutf -oadjplot.html -m0
     } else {
        exec $X_dir/xterm -fn fixed -e $exec_dir/parhplot $arg_ext -i$aoutf -oadjplot.html -m0
     }
  } else {
     if [catch {exec $exec_dir/parhplot.exe $arg_ext -i$aoutf -oadjplot.html -m0} result] {
        msg_box "ERROR: particle display file not created!"
        destroy .parshift
        return
     }
  }
  svg_box adjplot.html

} else {
  msg_box "Output file not found: $aoutf"
  destroy .parshift
  return

}

}
