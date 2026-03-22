proc cfg_emit { } {

#-----------------------------------------------------------------------------
# EMIT_CFG.TCL: Emission file configuration script
# Last Revised: 23 May 2006 - initial version
#-----------------------------------------------------------------------------

global html_dir num_locs  

if [winfo exists .cfgemit] {destroy .cfgemit}
set wr .cfgemit
toplevel $wr
wm title $wr "Temporal Emission File Configuration"
wm  geometry $wr +25+50

frame $wr.top
frame $wr.num 
frame $wr.loc
frame $wr.end
pack $wr.top $wr.num $wr.loc $wr.end -side top -fill x -expand 1 -pady 5

#-->description
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "Creates an optional temporal emission file that\
would replace the starting locations, times, and emission\
amounts defined in the standard CONTROL file. The use of\
this file is enabled by defining the file name in the\
SETUP.CFG namelist EFILE variable through the Advanced-\
Concentration-Configuration menu tab."
pack $wr.top.lab

#-->pre-define the number of samplers

label $wr.num.lab -text "Set the number of release locations:"
entry $wr.num.ent -textvariable num_locs -width 2
pack $wr.num.lab $wr.num.ent -side top

#-->configure each sampler

button $wr.loc.but  -text "Configure Locations" -width 25 -command "loc_setup"
label $wr.loc.lab -text " _________________________________________________"
pack $wr.loc.but $wr.loc.lab  -side top 

#-->final save and exits

button $wr.end.quit  -bg red -text Quit -width 10 -command "destroy $wr" 
button $wr.end.help -text Help  -width 14 \
       -command "load_html [file join $html_dir S417.htm ] "
button $wr.end.none  -text "Delete" -width 10 -command "file delete EMITIMES; destroy $wr"
button $wr.end.save  -bg green -text "Save to File" -width 14 -command "write_locs; destroy $wr"
pack  $wr.end.quit $wr.end.help $wr.end.none $wr.end.save -side left -padx 8 

#--initialize values

if [file exists EMITIMES] {
   load_locs 
   } else {
   init_locs
   }

}

#--------------------------------------------------------------------------
proc init_locs {} {

global Start_time Num_Start_location 
global Start_locn
global num_locs rel_loc dur_loc xyz_loc val_loc avg_loc out_loc 

if [info exists Num_Start_location] {

   set YY [lindex [split $Start_time] 0]
   set MM [lindex [split $Start_time] 1]
   set DD [lindex [split $Start_time] 2]
   set HH [lindex [split $Start_time] 3]

   if { $MM==0 } {
      set date [clock format [clock scan now] -format "%Y %m %d %H"]
      set YY [lindex [split $date] 0]
      set MM [lindex [split $date] 1]
      set DD [lindex [split $date] 2]
      set HH [lindex [split $date] 3]
   }

   if { $YY <  40 } { set YY 20$YY}
   if { $YY < 100 } { set YY 19$YY}

} else {
   msg_box "Run Concentration Setup before this menu!"
   destroy .cfgemit
   return
}

set num_locs $Num_Start_location 
for { set i 1} { $i <=$num_locs} {incr i} {
    set rel_loc [lappend rel_loc "$YY $MM $DD ${HH} 00"]
    set dur_loc [lappend dur_loc 2400]
    set xyz_loc [lappend xyz_loc $Start_locn($i)]
    set val_loc [lappend val_loc 1.0]
    set avg_loc [lappend avg_loc 0.0]
    set out_loc [lappend out_loc 0.0]
}
}

#------------------------------------------------------------------------------
proc load_locs {} {

global num_locs rel_loc dur_loc xyz_loc val_loc avg_loc out_loc 

set f [open "EMITIMES" r]
gets $f tmp_string
gets $f tmp_string

gets $f tmp_string
set num_locs [lindex $tmp_string 5]

for { set i 1} { $i <=$num_locs} {incr i} {
    gets $f tmp_string

    set rel_loc [lappend rel_loc [lrange $tmp_string 0 4]]
    set dur_loc [lappend dur_loc [lindex $tmp_string 5]]
    set xyz_loc [lappend xyz_loc [lrange $tmp_string 6 8]]
    set val_loc [lappend val_loc [lindex $tmp_string 9]]
    set avg_loc [lappend avg_loc [lindex $tmp_string 10]]
    set out_loc [lappend out_loc [lindex $tmp_string 11]]
}
close $f
}


#------------------------------------------------------------------------------
proc loc_setup {} {

global num_locs  

if [winfo exists .locsetup] {destroy .locsetup}
set wr .locsetup
toplevel $wr
wm title $wr "Select Release Point for Configuratiion"
wm  geometry $wr +50+75

frame $wr.top
frame $wr.bot 
pack $wr.top $wr.bot -side top

label $wr.top.lab -text " Select Location Number " -relief ridge -bg red
pack $wr.top.lab -side top -pady 5 

for { set d 1} { $d <=$num_locs} {incr d} {
   radiobutton $wr.top.$d -variable dummy  -text "Location $d" -value $d \
                          -background grey -command "loc_enter $d"
   pack $wr.top.$d -side top
}

label $wr.bot.lab -text "__________________________________________"
pack $wr.bot.lab 
button $wr.bot.quit -text "Return to Previous Menu" -width 30 -command "destroy $wr"
pack $wr.bot.quit 
}

#----------------------------------------------------------------------------------
proc loc_enter {No} {

global html_dir
global num_locs rel_k dur_k xyz_k val_k avg_k out_k    

if [winfo exists .locenter] {destroy .locenter}
set wr .locenter
toplevel $wr
wm title $wr "Configuration Menu for Release Location Number $No"
wm  geometry $wr +75+100

frame $wr.rel
frame $wr.dur
frame $wr.xyz
frame $wr.val
frame $wr.avg
frame $wr.out
frame $wr.end

pack  $wr.rel $wr.dur $wr.xyz $wr.val $wr.val $wr.avg $wr.out $wr.end \
     -side top -fill x -expand 1 -pady 10

label $wr.rel.lab -text "Release start time (yyyy mm dd hh mm) :"
entry $wr.rel.ent -textvariable rel_k -width 20
pack  $wr.rel.lab $wr.rel.ent -side left -padx 5

label $wr.dur.lab -text "              Release duration (hhmm) :"
entry $wr.dur.ent -textvariable dur_k -width 20
pack  $wr.dur.lab $wr.dur.ent -side left -padx 5

label $wr.xyz.lab -text "   Release location (Lat Lon Hgt-agl) :"
entry $wr.xyz.ent -textvariable xyz_k  -width 20
pack  $wr.xyz.lab $wr.xyz.ent -side left -padx 5

label $wr.val.lab -text "            Emission rate (mass/hour) :"
entry $wr.val.ent -textvariable val_k -width 20
pack  $wr.val.lab $wr.val.ent -side left -padx 5

label $wr.avg.lab -text "            Emission area (sq meters) :" 
entry $wr.avg.ent -textvariable avg_k -width 20
pack  $wr.avg.lab $wr.avg.ent -side left -padx 5

label $wr.out.lab -text "  Heat release for plume rise (watts) :" 
entry $wr.out.ent -textvariable out_k -width 20
pack  $wr.out.lab $wr.out.ent -side left -padx 5

button $wr.end.quit -bg red -text Quit -width 12 -command "destroy $wr"
button $wr.end.help -text Help -width 12 \
       -command "load_html [file join $html_dir S417.htm ] "
button $wr.end.done -bg green -text Save -width 20 -command "set_locs $No"
pack $wr.end.quit $wr.end.help $wr.end.done -side left -padx 8 
ini_locs $No
}

#--------------------------------------------------------------------------
proc ini_locs {No} {

global num_locs rel_loc dur_loc xyz_loc val_loc avg_loc out_loc 
global          rel_k   dur_k   xyz_k   val_k   avg_k   out_k    

set rel_k [lindex $rel_loc [expr $No-1]]
if { $rel_k == ""} { set rel_k [lindex $rel_loc 0]}

set dur_k [lindex $dur_loc [expr $No-1]]
if { $dur_k == ""} { set dur_k [lindex $dur_loc 0]}

set xyz_k [lindex $xyz_loc [expr $No-1]]
if { $xyz_k == ""} { set xyz_k [lindex $xyz_loc 0]}

set val_k [lindex $val_loc [expr $No-1]]
if { $val_k == ""} { set val_k [lindex $val_loc 0]}

set avg_k [lindex $avg_loc [expr $No-1]]
if { $avg_k == ""} { set avg_k [lindex $avg_loc 0]}

set out_k [lindex $out_loc [expr $No-1]]
if { $out_k == ""} { set out_k [lindex $out_loc 0]}
}

#--------------------------------------------------------------------------
proc set_locs {No} {

global num_locs rel_loc dur_loc xyz_loc val_loc avg_loc out_loc 
global          rel_k   dur_k   xyz_k   val_k   avg_k   out_k    

set t1 ""
set t2 ""
set t3 ""
set t4 ""
set t5 ""
set t6 ""

for {set i 1} {$i<=$num_locs} {incr i} {
if {$i == $No} {
     lappend t1 $rel_k
     lappend t2 $dur_k
     lappend t3 $xyz_k
     lappend t4 $val_k
     lappend t5 $avg_k
     lappend t6 $out_k
}  else {
     lappend t1 [lindex $rel_loc  [expr $i-1]]
     lappend t2 [lindex $dur_loc  [expr $i-1]]
     lappend t3 [lindex $xyz_loc  [expr $i-1]]
     lappend t4 [lindex $val_loc  [expr $i-1]]
     lappend t5 [lindex $avg_loc  [expr $i-1]]
     lappend t6 [lindex $out_loc  [expr $i-1]]
   }
}
set rel_loc  $t1
set dur_loc  $t2
set xyz_loc  $t3
set val_loc  $t4
set avg_loc  $t5
set out_loc  $t6

destroy .locenter
}

#--------------------------------------------------------------------------
proc write_locs {} {

global num_locs rel_loc dur_loc xyz_loc val_loc avg_loc out_loc Num_pollut

set f [open "EMITIMES" w]

puts $f "YYYY MM DD HH    DURATION(hhhh) #RECORDS"
puts $f "YYYY MM DD HH MM DURATION(hhmm) LAT LON HGT(m) RATE(/h) AREA(m2) HEAT(w)"

set temp [lrange [lindex $rel_loc 0] 0 3] 
append temp " 9999 "
set num_recs [expr $num_locs * $Num_pollut]
append temp $num_recs
puts $f $temp

if {$num_locs < $num_recs} {
   for { set i 1} { $i <= $num_locs }   {incr i} {
   for { set j 1} { $j <= $Num_pollut } {incr j} {
         set temp     [lindex $rel_loc [expr $i-1]] 
      append temp " " [lindex $dur_loc [expr $i-1]] 
      append temp " " [lindex $xyz_loc [expr $i-1]] 
      append temp " " [lindex $val_loc [expr $i-1]] 
      append temp " " [lindex $avg_loc [expr $i-1]] 
      append temp " " [lindex $out_loc [expr $i-1]] 
      puts $f $temp
   }
   }

} else {
   for { set i 1} { $i <= $num_locs }   {incr i} {
         set temp     [lindex $rel_loc [expr $i-1]] 
      append temp " " [lindex $dur_loc [expr $i-1]] 
      append temp " " [lindex $xyz_loc [expr $i-1]] 
      append temp " " [lindex $val_loc [expr $i-1]] 
      append temp " " [lindex $avg_loc [expr $i-1]] 
      append temp " " [lindex $out_loc [expr $i-1]] 
      puts $f $temp
   }
}
close $f
}
