proc cfg_lags { } {

#-----------------------------------------------------------------------------
# LAGS_CFG.TCL: LAGrangian Sampler configuration script
# Last Revised: 19 Oct 2005 - initial version
#-----------------------------------------------------------------------------

global html_dir

if [winfo exists .cfglags] {destroy .cfglags}
set wr .cfglags
toplevel $wr
wm title $wr "Dynamic Lagrangian Sampler Configuration"
wm  geometry $wr +25+50

frame $wr.top
frame $wr.num 
frame $wr.lag
frame $wr.end
pack $wr.top $wr.num $wr.lag $wr.end -side top -fill x -expand 1 -pady 5

#-->description
label $wr.top.lab -fg blue -justify left -wraplength 6i \
 -text "Creates the optional LAGSET.CFG file to configure a\
dynamic sampler that can pass through the model simulation\
domain, either passively (Lagrangian) with the wind or using\
a pre-defined velocity vector. The dynamic sampler transport\
is vertically isobaric and it samples model produced values\
that are internally generated in a snapshot concentration grid."
pack $wr.top.lab

#-->pre-define the number of samplers

label $wr.num.lab -text "Set the number of samplers:"
entry $wr.num.ent -textvariable num_lags -width 2
pack $wr.num.lab $wr.num.ent -side top

#-->configure each sampler

button $wr.lag.but  -text "Configure Samplers" -width 25 -command "lag_setup"
label $wr.lag.lab -text " _________________________________________________"
pack $wr.lag.but $wr.lag.lab  -side top 

#-->final save and exits

button $wr.end.quit  -bg red -text Quit -width 10 -command "destroy $wr" 
button $wr.end.help -text Help -width 14 \
       -command "load_html [file join $html_dir S415.htm ] "
button $wr.end.none  -text "Delete" -width 10 -command "file delete LAGSET.CFG; destroy $wr"
button $wr.end.save  -bg green -text "Save to File" -width 14 -command "write_lags; destroy $wr"
pack  $wr.end.quit $wr.end.help $wr.end.none $wr.end.save -side left -padx 8 

#--initialize values

if [file exists LAGSET.CFG] {
   load_lags 
   } else {
   init_lags
   }

}

#--------------------------------------------------------------------------
proc init_lags {} {

global num_lags loc_lags vec_lags rel_lags sam_lags avg_lags out_lags nam_lags 

set date [clock format [clock scan now] -format "%y %m %d %H"]
set YY [lindex [split $date] 0]
set MM [lindex [split $date] 1]
set DD [lindex [split $date] 2]
set HH [lindex [split $date] 3]

set num_lags 1
lappend loc_lags "40.0 -90.0 500.0"
lappend vec_lags "000.0 00.0"
lappend rel_lags "$YY $MM $DD $HH 00"
lappend sam_lags "$YY $MM $DD $HH 00"
lappend avg_lags "00"
lappend out_lags "60"
lappend nam_lags "'LAGOUT.TXT'"
}

#------------------------------------------------------------------------------
proc load_lags {} {

global num_lags loc_lags vec_lags rel_lags sam_lags avg_lags out_lags nam_lags 

set f [open "LAGSET.CFG" r]
gets $f num_lags
for { set i 1} { $i <=$num_lags} {incr i} {
    gets $f tmp_string 
    set loc_lags [lappend loc_lags [string trim $tmp_string]]
    gets $f tmp_string 
    set vec_lags [lappend vec_lags [string trim $tmp_string]]
    gets $f tmp_string 
    set rel_lags [lappend rel_lags [string trim $tmp_string]]
    gets $f tmp_string 
    set sam_lags [lappend sam_lags [string trim $tmp_string]]
    gets $f tmp_string 
    set avg_lags [lappend avg_lags [string trim $tmp_string]]
    gets $f tmp_string 
    set out_lags [lappend out_lags [string trim $tmp_string]]
    gets $f tmp_string 
    set nam_lags [lappend nam_lags [string trim $tmp_string]]
}
close $f
}


#------------------------------------------------------------------------------
proc lag_setup {} {

global num_lags  

if [winfo exists .lagsetup] {destroy .lagsetup}
set wr .lagsetup
toplevel $wr
wm title $wr "Lagrangian Sampler Selection for Configuratiion"
wm  geometry $wr +50+75

frame $wr.top
frame $wr.bot 
pack $wr.top $wr.bot -side top

label $wr.top.lab -text " Select Sampler " -relief ridge -bg red
pack $wr.top.lab -side top -pady 5 

for { set d 1} { $d <=$num_lags} {incr d} {
   radiobutton $wr.top.$d -variable dummy  -text "Sampler $d" -value $d \
                          -background grey -command "lag_enter $d"
   pack $wr.top.$d -side top
}

label $wr.bot.lab -text "__________________________________________"
pack $wr.bot.lab 
button $wr.bot.quit -text "Return to Previous Menu" -width 30 -command "destroy $wr"
pack $wr.bot.quit 
}

#----------------------------------------------------------------------------------
proc lag_enter {No} {

global html_dir
global num_lags loc_lags vec_lags rel_lags sam_lags avg_lags out_lags nam_lags 
global          loc_x    vec_x    rel_x    sam_x    avg_x    out_x    nam_x    

if [winfo exists .lagenter] {destroy .lagenter}
set wr .lagenter
toplevel $wr
wm title $wr "Configuration Menu for Sampler Number $No"
wm  geometry $wr +75+100

frame $wr.loc
frame $wr.vec
frame $wr.rel
frame $wr.sam
frame $wr.avg
frame $wr.out
frame $wr.nam
frame $wr.end
pack $wr.loc $wr.vec $wr.rel $wr.sam $wr.avg $wr.out $wr.nam $wr.end \
     -side top -fill x -expand 1 -pady 10

label $wr.loc.lab -text "Release location (Lat Lon Hgt-agl)     :"
entry $wr.loc.ent -textvariable loc_x  -width 20
pack  $wr.loc.lab $wr.loc.ent -side left -padx 5

label $wr.vec.lab -text "Velocity vector (Direction Speed m/s)  :"
entry $wr.vec.ent -textvariable vec_x -width 20
pack  $wr.vec.lab $wr.vec.ent -side left -padx 5

label $wr.rel.lab -text "Sampler release time (yy mm dd hh min) :"
entry $wr.rel.ent -textvariable rel_x -width 20
pack  $wr.rel.lab $wr.rel.ent -side left -padx 5

label $wr.sam.lab -text "Sampling start time (yy mm dd hh min)  :"
entry $wr.sam.ent -textvariable sam_x -width 20
pack  $wr.sam.lab $wr.sam.ent -side left -padx 5

label $wr.avg.lab -text "Sampler averaging time (min)           :" 
entry $wr.avg.ent -textvariable avg_x -width 20
pack  $wr.avg.lab $wr.avg.ent -side left -padx 5

label $wr.out.lab -text "Disk write output interval (min)       :" 
entry $wr.out.ent -textvariable out_x -width 20
pack  $wr.out.lab $wr.out.ent -side left -padx 5

label $wr.nam.lab -text "Output file (/dir/name in quotes)      :" 
entry $wr.nam.ent -textvariable nam_x -width 20
pack  $wr.nam.lab $wr.nam.ent -side left -padx 5

button $wr.end.quit -bg red -text Quit -width 12 -command "destroy $wr"
button $wr.end.help -text Help -width 12 \
       -command "load_html [file join $html_dir S415.htm ] "
button $wr.end.done -bg green -text Save -width 20 -command "set_lags $No"
pack $wr.end.quit $wr.end.help $wr.end.done -side left -padx 8 
ini_lags $No
}

#--------------------------------------------------------------------------
proc ini_lags {No} {

global num_lags loc_lags vec_lags rel_lags sam_lags avg_lags out_lags nam_lags 
global          loc_x    vec_x    rel_x    sam_x    avg_x    out_x    nam_x    

set loc_x [lindex $loc_lags [expr $No-1]]
if { $loc_x == ""} { set loc_x [lindex $loc_lags 0]}

set vec_x [lindex $vec_lags [expr $No-1]]
if { $vec_x == ""} { set vec_x [lindex $vec_lags 0]}

set rel_x [lindex $rel_lags [expr $No-1]]
if { $rel_x == ""} { set rel_x [lindex $rel_lags 0]}

set sam_x [lindex $sam_lags [expr $No-1]]
if { $sam_x == ""} { set sam_x [lindex $sam_lags 0]}

set avg_x [lindex $avg_lags [expr $No-1]]
if { $avg_x == ""} { set avg_x [lindex $avg_lags 0]}

set out_x [lindex $out_lags [expr $No-1]]
if { $out_x == ""} { set out_x [lindex $out_lags 0]}

set nam_x [lindex $nam_lags [expr $No-1]]
if { $nam_x == ""} { set nam_x [lindex $nam_lags 0]}
}

#--------------------------------------------------------------------------
proc set_lags {No} {

global num_lags loc_lags vec_lags rel_lags sam_lags avg_lags out_lags nam_lags 
global          loc_x    vec_x    rel_x    sam_x    avg_x    out_x    nam_x    

set t1 ""
set t2 ""
set t3 ""
set t4 ""
set t5 ""
set t6 ""
set t7 ""

for {set i 1} {$i<=$num_lags} {incr i} {
if {$i == $No} {
     lappend t1 $loc_x
     lappend t2 $vec_x
     lappend t3 $rel_x
     lappend t4 $sam_x
     lappend t5 $avg_x
     lappend t6 $out_x
     lappend t7 $nam_x
}  else {
     lappend t1 [lindex $loc_lags [expr $i-1]]
     lappend t2 [lindex $vec_lags [expr $i-1]]
     lappend t3 [lindex $rel_lags [expr $i-1]]
     lappend t4 [lindex $sam_lags [expr $i-1]]
     lappend t5 [lindex $avg_lags [expr $i-1]]
     lappend t6 [lindex $out_lags [expr $i-1]]
     lappend t7 [lindex $nam_lags [expr $i-1]]
   }
}
set loc_lags  $t1
set vec_lags  $t2
set rel_lags  $t3
set sam_lags  $t4
set avg_lags  $t5
set out_lags  $t6
set nam_lags  $t7

destroy .lagenter
}

#--------------------------------------------------------------------------
proc write_lags {} {

global num_lags loc_lags vec_lags rel_lags sam_lags avg_lags out_lags nam_lags 

set f [open "LAGSET.CFG" w]

puts $f $num_lags

for { set i 1} { $i <=$num_lags} {incr i} {

     puts $f [lindex $loc_lags [expr $i-1]]
     puts $f [lindex $vec_lags [expr $i-1]]
     puts $f [lindex $rel_lags [expr $i-1]]
     puts $f [lindex $sam_lags [expr $i-1]]
     puts $f [lindex $avg_lags [expr $i-1]]
     puts $f [lindex $out_lags [expr $i-1]]
     puts $f [lindex $nam_lags [expr $i-1]]
}
close $f
}
