proc conc_init { } {

#-----------------------------------------------------------------------------
# CONC_SETUP.TCL: concentration simulation setup script
# Last Revised:   06 Feb 2002
#                 13 Aug 2002
#                 14 Apr 2004
#                 02 Jul 2004 - test for exceeding GUI limit
#                 21 Sep 2004 - defaults and reset to deposition menu
#                 27 May 2005 - browse widgets 
#                 16 May 2006 - variable start location listing
#                 19 Mar 2007 - carry deposition variable to emission ID
#                 21 May 2007 - unlimited number of starting locations
#                 18 Aug 2009 - handle spaces in directory name
#                 23 Sep 2009 - removed delimiter from end of dir string
#                 23 Jan 2011 - radibutton particle default dropped 0.006
#                 06 Jul 2011 - revised wet in-cloud particle default
#                 17 May 2012 - more vertical motion choices
#                 25 Jun 2012 - damped vertical motion
#                 01 Nov 2012 - further revised particle wet removal default
#                 08 Nov 2013 - dynamically change AGL/MSL label
#                 13 Jun 2014 - strip comments after hash mark on retrieve
#                 16 Jun 2014 - syntax correction on hash test
#                 25 Sep 2014 - changes to default wet removal parameters
#                 27 Jan 2015 - default Henry's constant to 3.0
#                 09 Apr 2015 - added FMDV as deposition option (foot & mouth)
#                 11 Jan 2016 - changed default particle wet removal coefficients
#                 14 Sep 2016 - added Save_path
#                 15 Aug 2018 - fwrd/back button changes with sign of run
#                 01 Sep 2020 - preconfigure buttons on deposition initialized
#                             - deposition reset restores Identis if not saved
#-----------------------------------------------------------------------------

global html_dir Grid_dir Grid_name Grids_path Grid_number
global Totle_hours Direct
global Use_vertical Use_listing Use_text

if [winfo exists .concinit] {destroy .concinit}
set wr .concinit
toplevel $wr
wm title $wr "Concentration Setup"
wm  geometry $wr +50+50

frame $wr.sttime
frame $wr.stloc
frame $wr.tottime
frame $wr.vertical
frame $wr.data
frame $wr.part234
frame $wr.sept
frame $wr.exit
pack $wr.sttime $wr.stloc $wr.tottime $wr.vertical $wr.data \
$wr.part234 $wr.sept $wr.exit -side top -fill x -expand 1 -pady 10

#-->starting information

label $wr.sttime.lab -text "       Starting time (YY MM DD HH): " 
entry $wr.sttime.ent -textvariable Start_time -width 26
pack $wr.sttime.lab $wr.sttime.ent -side left 

label $wr.stloc.lab -text "  Number of starting locations:"
entry $wr.stloc.ent -textvariable Num_Start_location -width 2
label $wr.stloc.lab1 -text "====>"
button $wr.stloc.bun  -text "Setup starting locations" -width 26 -command {start_loc}
pack $wr.stloc.lab $wr.stloc.ent $wr.stloc.lab1 $wr.stloc.bun -side left

#-->duration and direction flag

frame $wr.tottime.time
frame $wr.tottime.dir
frame $wr.tottime.top
pack  $wr.tottime.time $wr.tottime.dir $wr.tottime.top -side left -padx 10

label $wr.tottime.time.lab -text "Total run time (hrs)"
entry $wr.tottime.time.ent -textvariable Totle_hours -width 4
pack $wr.tottime.time.lab $wr.tottime.time.ent -side top
bind $wr.tottime.time.ent <KeyRelease> {
     if {$Totle_hours < 0} {
        set Direct 1
     } else {
        set Direct 0}
}

label $wr.tottime.dir.lab -text "Direction"
radiobutton $wr.tottime.dir.fwd -text "Fwrd" -variable Direct \
-value "0"  -command {set Totle_hours [expr abs($Totle_hours)]}
radiobutton $wr.tottime.dir.bck -text "Back" -variable Direct \
-value "1" -command {set Totle_hours -[expr abs($Totle_hours)]}
pack $wr.tottime.dir.lab -side top 
pack $wr.tottime.dir.fwd $wr.tottime.dir.bck -side left

label $wr.tottime.top.lab -text "Top of model (m agl)"
entry $wr.tottime.top.ent -textvariable Top_model -relief sunken -width 8
pack $wr.tottime.top.lab $wr.tottime.top.ent -side top


#-->vertical motion

label  $wr.vertical.lab -text "Vertical Motion Method:"
entry  $wr.vertical.ent -text Use_text -relief sunken -width 22
button $wr.vertical.val  -text "Select" -width 14 -command conc_vert
pack $wr.vertical.lab $wr.vertical.ent $wr.vertical.val -side left -padx 8

frame $wr.data.lab
frame $wr.data.pick
pack $wr.data.lab $wr.data.pick -side top

button $wr.data.lab.set  -text "Add Meteorology Files" -width 25 -command {
   do_file_list .concinit.data.pick.src.list .concinit.data.pick.dir.list
}

button $wr.data.lab.clr  -text "Clear" -width 5 -command { 
   .concinit.data.pick.src.list delete 0 end
   .concinit.data.pick.dir.list delete 0 end
   set Grid_number 0
}

label $wr.data.lab.lab -text "   Selected Files:"
entry $wr.data.lab.ent  -textvariable Grid_number -relief sunken -width 2
pack  $wr.data.lab.set $wr.data.lab.clr $wr.data.lab.lab $wr.data.lab.ent -side left

frame $wr.data.pick.dir
frame $wr.data.pick.src
pack $wr.data.pick.dir $wr.data.pick.src -side left -padx 5

listbox $wr.data.pick.dir.list -relief sunken -width 34 -height 5 -setgrid yes -bg LightCyan3
pack $wr.data.pick.dir.list -fill both -expand yes 

listbox $wr.data.pick.src.list -relief sunken -width 20 -height 5 -setgrid yes -bg LightCyan3
pack $wr.data.pick.src.list -fill both -expand yes

button $wr.part234.part2  -text "Pollutant, Deposition and Grids setup" \
-width 60 -command "group_data_setup"
pack $wr.part234.part2 -padx 5

label $wr.sept.lab \
-text " _________________________________________________________________"
pack $wr.sept.lab -side left
button $wr.exit.dismiss  -bg red -text Quit -width 10 -command "destroy $wr" 
button $wr.exit.help -text Help  -width 10 \
       -command "load_html [file join $html_dir S310.htm ] "
button $wr.exit.saveas  -text "Save as" -width 10 -command "save_as_name"
button $wr.exit.retr  -text Retrieve  -width 10 -command "retrieve_prm_file"
button $wr.exit.save  -bg green -text "Save" -width 12 -command "save_to_control"
pack  $wr.exit.dismiss $wr.exit.help $wr.exit.saveas $wr.exit.retr $wr.exit.save\
-side left -padx 8 
init_concentration {0}
}

#------------------------------------------------------------------------------
proc init_concentration {skip} {
global Num_disp 
global Use_vertical Use_listing Use_text
set Num_disp 0
set f [open "default_conc" r]
read_prm_file $f $skip

#vertical motion method
set Use_listing ""
lappend Use_listing "0 = input model data" 
lappend Use_listing "1 = isobaric        "  
lappend Use_listing "2 = isentropic      "  
lappend Use_listing "3 = constant density"  
lappend Use_listing "4 = isosigma        "  
lappend Use_listing "5 = from divergence " 
lappend Use_listing "6 = remap MSL to AGL"  
lappend Use_listing "7 = average data    " 
lappend Use_listing "8 = damped magnitude" 
set Use_text [lindex $Use_listing $Use_vertical]
}

#-------------------------------------------------------------------------------
proc read_prm_file {f skip} {
global Grid_number Grids_path 
global Start_time Num_Start_location Totle_hours Use_vertical Top_model 
global Start_locn Direct

gets $f Start_time
set hash [string first # $Start_time]
if { $hash >= 0 } {set Start_time [string range $Start_time 0 $hash-1]}
set Start_time [string trim $Start_time]
 
gets $f Num_Start_location
set hash [string first # $Num_Start_location]
if { $hash >= 0 } {set Num_Start_location [string range $Num_Start_location 0 $hash-1]}
for { set i 1} { $i <=$Num_Start_location} {incr i} {
    gets $f Start_loc
    set hash [string first # $Start_loc]
    if { $hash >= 0 } {set Start_loc [string range $Start_loc 0 $hash-1]}
    set Start_locn($i) [string trim $Start_loc]
}

gets $f Totle_hours 
set hash [string first # $Totle_hours]
if { $hash >= 0 } {set Totle_hours [string range $Totle_hours 0 $hash-1]}
set Totle_hours [string trim $Totle_hours] 
set Direct 0
if {$Totle_hours < 0} {set Direct 1}

gets $f Use_vertical
set hash [string first # $Use_vertical]
if { $hash >= 0 } {set Use_vertical [string range $Use_vertical 0 $hash-1]}
set Use_vertical [string trim $Use_vertical]  

gets $f Top_model
set hash [string first # $Top_model]
if { $hash >= 0 } {set Top_model [string range $Top_model 0 $hash-1]}
set Top_model [string trim $Top_model] 

gets $f Grid_number
set Grid_number [string trim $Grid_number]
if { $hash >= 0 } {set Grid_number [string range $Grid_number 0 $hash-1]}
set Grid_number [string trim  $Grid_number]
if {$skip == 0} {
   .concinit.data.pick.dir.list delete 0 end
   .concinit.data.pick.src.list delete 0 end
   }
for { set i 1} {$i<=$Grid_number} {incr i} {   
    gets $f tmp
    set hash [string first # $tmp]
    if { $hash >= 0 } {set tmp [string range $tmp 0 $hash-1]}
    set tmp [string trim $tmp]
    set Grids_path [string range $tmp 0 end-1]
    if {$skip == 0} {.concinit.data.pick.dir.list insert end $Grids_path}

    gets $f Grids_name 
    set hash [string first # $Grids_name]
    if { $hash >= 0 } {set Grids_name [string range $Grids_name 0 $hash-1]}
    set Grids_name [string trim $Grids_name]
    if {$skip == 0} {.concinit.data.pick.src.list insert end $Grids_name}
}

#---------pollutants parmeters read-----
global Num_pollut Identis Emi_rates Emi_hours Rel_times

gets $f Num_pollut
set hash [string first # $Num_pollut]
if { $hash >= 0 } {set Num_pollut [string range $Num_pollut 0 $hash-1]}
set Num_pollut [string trim $Num_pollut]

set Identis ""
set Emi_rates ""
set Emi_hours ""
set Rel_times ""
for {set i 0} {$i<$Num_pollut} {incr i} {

gets $f tmp_string
set hash [string first # $tmp_string]
if { $hash >= 0 } {set tmp_string [string range $tmp_string 0 $hash-1]}
set Identis  [lappend Identis [string trim $tmp_string]]

gets $f tmp_string
set hash [string first # $tmp_string]
if { $hash >= 0 } {set tmp_string [string range $tmp_string 0 $hash-1]}
set Emi_rates [lappend Emi_rates [string trim $tmp_string]]

gets $f tmp_string
set hash [string first # $tmp_string]
if { $hash >= 0 } {set tmp_string [string range $tmp_string 0 $hash-1]}
set Emi_hours [lappend Emi_hours [string trim $tmp_string]]

gets $f tmp_string
set hash [string first # $tmp_string]
if { $hash >= 0 } {set tmp_string [string range $tmp_string 0 $hash-1]}
set Rel_times [lappend Rel_times [string trim $tmp_string]]

}
#----------simultaneous primtive input----
global Num_simult Cen_lalo Space_lalo Span_lalo Grid_dir Grid_name Num_ver
global Hei_agl Sam_start Sam_stop Sam_int

gets $f Num_simult
set hash [string first # $Num_simult]
if { $hash >= 0 } {set Num_simult [string range $Num_simult 0 $hash-1]}
set Num_simult [string trim $Num_simult ]

set Cen_lalo ""
set Space_lalo "" 
set Span_lalo ""
set Grid_dir ""
set Grid_name ""
set Num_ver ""
set Hei_agl ""
set Sam_start ""
set Sam_stop ""
set Sam_int ""

for {set i 0} {$i <$Num_simult} {incr i} {
gets $f tmp_string
set hash [string first # $tmp_string]
if { $hash >= 0 } {set tmp_string [string range $tmp_string 0 $hash-1]}
set Cen_lalo [lappend Cen_lalo [string trim $tmp_string]]

gets $f tmp_string
set hash [string first # $tmp_string]
if { $hash >= 0 } {set tmp_string [string range $tmp_string 0 $hash-1]}
set Space_lalo [lappend Space_lalo [string trim $tmp_string]]

gets $f tmp_string
set hash [string first # $tmp_string]
if { $hash >= 0 } {set tmp_string [string range $tmp_string 0 $hash-1]}
set Span_lalo [lappend Span_lalo [string trim $tmp_string]]

gets $f tmp_string
set hash [string first # $tmp_string]
if { $hash >= 0 } {set tmp_string [string range $tmp_string 0 $hash-1]}
set Grid_dir [lappend Grid_dir [string trim $tmp_string]]

gets $f tmp_string
set hash [string first # $tmp_string]
if { $hash >= 0 } {set tmp_string [string range $tmp_string 0 $hash-1]}
set Grid_name [lappend Grid_name [string trim $tmp_string]]

gets $f tmp_string
set hash [string first # $tmp_string]
if { $hash >= 0 } {set tmp_string [string range $tmp_string 0 $hash-1]}
set Num_ver [lappend Num_ver [string trim $tmp_string]]

gets $f tmp_string
set hash [string first # $tmp_string]
if { $hash >= 0 } {set tmp_string [string range $tmp_string 0 $hash-1]}
set Hei_agl [lappend Hei_agl [string trim $tmp_string]]

gets $f tmp_string
set hash [string first # $tmp_string]
if { $hash >= 0 } {set tmp_string [string range $tmp_string 0 $hash-1]}
set Sam_start [lappend Sam_start [string trim $tmp_string]]

gets $f tmp_string
set hash [string first # $tmp_string]
if { $hash >= 0 } {set tmp_string [string range $tmp_string 0 $hash-1]}
set Sam_stop [lappend Sam_stop [string trim $tmp_string]]

gets $f tmp_string
set hash [string first # $tmp_string]
if { $hash >= 0 } {set tmp_string [string range $tmp_string 0 $hash-1]}
set Sam_int [lappend Sam_int [string trim $tmp_string]]
}

global Num_deposi Particles Molecular Wet_remove  Decay_day  Resuspension 
gets $f Num_deposi
set hash [string first # $Num_deposi]
if { $hash >= 0 } {set Num_deposi [string range $Num_deposi 0 $hash-1]}
set Num_deposi [string trim $Num_deposi]

set Particles "" 
set Molecular ""
set Wet_remove ""
set Decay_day ""
set Resuspension ""
for {set i 0} {$i <$Num_deposi} {incr i} {
gets $f tmp_string 
set hash [string first # $tmp_string]
if { $hash >= 0 } {set tmp_string [string range $tmp_string 0 $hash-1]}
set Particles [lappend Particles [string trim $tmp_string]]

gets $f tmp_string
set hash [string first # $tmp_string]
if { $hash >= 0 } {set tmp_string [string range $tmp_string 0 $hash-1]}
set Molecular [lappend Molecular [string trim $tmp_string]]

gets $f tmp_string
set hash [string first # $tmp_string]
if { $hash >= 0 } {set tmp_string [string range $tmp_string 0 $hash-1]}
set Wet_remove [lappend Wet_remove [string trim $tmp_string]]

gets $f  tmp_string
set hash [string first # $tmp_string]
if { $hash >= 0 } {set tmp_string [string range $tmp_string 0 $hash-1]}
set Decay_day [lappend Decay_da [string trim $tmp_string]]

gets $f tmp_string
set hash [string first # $tmp_string]
if { $hash >= 0 } {set tmp_string [string range $tmp_string 0 $hash-1]}
set Resuspension [lappend Resuspension [string trim $tmp_string]]
}
close $f
}


#------------------------------------------------------------------------------
proc group_data_setup {} {
global html_dir Num_pollut Num_simult Num_deposi
if [winfo exists .simultsetup] {destroy .simultsetup}
set wr .simultsetup
toplevel $wr
wm title $wr "Pollutant, Concentration Grid, and Deposition setup"
wm  geometry $wr +50+75
frame $wr.top
frame $wr.sept
frame $wr.exit
pack $wr.top $wr.sept $wr.exit -side top
frame $wr.top.left 
frame $wr.top.midd
frame $wr.top.right
pack $wr.top.left $wr.top.midd $wr.top.right -side left -padx 5

frame $wr.top.left.num
frame $wr.top.left.group
pack $wr.top.left.num $wr.top.left.group -side top -fill x -expand 1 -pady 2

label $wr.top.left.num.lab -text " Pollutant: " -relief ridge -bg red
frame $wr.top.left.num.sub1
pack $wr.top.left.num.lab $wr.top.left.num.sub1 -side top
label $wr.top.left.num.sub1.lab -text "Num="
entry $wr.top.left.num.sub1.ent -textvariable Num_pollut -width 2 
pack $wr.top.left.num.sub1.lab $wr.top.left.num.sub1.ent -side left

radiobutton $wr.top.left.group.rad1 -text "Specie 1 " -variable P_group -value 1 -command "pollut_data 1" 
radiobutton $wr.top.left.group.rad2 -text "Specie 2 " -variable P_group -value 2 -command "pollut_data 2" 
radiobutton $wr.top.left.group.rad3 -text "Specie 3 " -variable P_group -value 3 -command "pollut_data 3" 
radiobutton $wr.top.left.group.rad4 -text "Specie 4 " -variable P_group -value 4 -command "pollut_data 4" 
radiobutton $wr.top.left.group.rad5 -text "Specie 5 " -variable P_group -value 5 -command "pollut_data 5" 
radiobutton $wr.top.left.group.rad6 -text "Specie 6 " -variable P_group -value 6 -command "pollut_data 6" 
radiobutton $wr.top.left.group.rad7 -text "Specie 7 " -variable P_group -value 7 -command "pollut_data 7" 
pack $wr.top.left.group.rad1 $wr.top.left.group.rad2 $wr.top.left.group.rad3 $wr.top.left.group.rad4 \
$wr.top.left.group.rad5 $wr.top.left.group.rad6 $wr.top.left.group.rad7  -side top

frame $wr.top.midd.num
frame $wr.top.midd.group
pack $wr.top.midd.num $wr.top.midd.group -side top -fill x -expand 1 -pady 2
label $wr.top.midd.num.lab -text " Grids: " -relief ridge -bg green
frame $wr.top.midd.num.sub1
pack $wr.top.midd.num.lab $wr.top.midd.num.sub1 -side top
label $wr.top.midd.num.sub1.lab -text "Num=" 
entry $wr.top.midd.num.sub1.ent -textvariable Num_simult -width 2 
pack $wr.top.midd.num.sub1.lab $wr.top.midd.num.sub1.ent -side left

radiobutton $wr.top.midd.group.rad1 -text "Grid 1 " -variable S_group -value 1 -command "simult_data 1" 
radiobutton $wr.top.midd.group.rad2 -text "Grid 2 " -variable S_group -value 2 -command "simult_data 2" 
radiobutton $wr.top.midd.group.rad3 -text "Grid 3 " -variable S_group -value 3 -command "simult_data 3" 
radiobutton $wr.top.midd.group.rad4 -text "Grid 4 " -variable S_group -value 4 -command "simult_data 4" 
radiobutton $wr.top.midd.group.rad5 -text "Grid 5 " -variable S_group -value 5 -command "simult_data 5" 
radiobutton $wr.top.midd.group.rad6 -text "Grid 6 " -variable S_group -value 6 -command "simult_data 6" 
radiobutton $wr.top.midd.group.rad7 -text "Grid 7 " -variable S_group -value 7 -command "simult_data 7" 
pack $wr.top.midd.group.rad1 $wr.top.midd.group.rad2 $wr.top.midd.group.rad3 $wr.top.midd.group.rad4 \
$wr.top.midd.group.rad5 $wr.top.midd.group.rad6 $wr.top.midd.group.rad7  -side top

frame $wr.top.right.num
frame $wr.top.right.group
pack $wr.top.right.num $wr.top.right.group -side top -fill x -expand 1 -pady 2

label $wr.top.right.num.lab -text " Deposition: "  -relief ridge -bg white
frame $wr.top.right.num.sub1
pack $wr.top.right.num.lab $wr.top.right.num.sub1 -side top
label $wr.top.right.num.sub1.lab -text "Num="
entry $wr.top.right.num.sub1.ent -textvariable Num_deposi -width 2 
pack $wr.top.right.num.sub1.lab $wr.top.right.num.sub1.ent -side left

radiobutton $wr.top.right.group.rad1 -text "Specie 1 " -variable D_group -value 1 -command "deposi_data 1" 
radiobutton $wr.top.right.group.rad2 -text "Specie 2 " -variable D_group -value 2 -command "deposi_data 2" 
radiobutton $wr.top.right.group.rad3 -text "Specie 3 " -variable D_group -value 3 -command "deposi_data 3" 
radiobutton $wr.top.right.group.rad4 -text "Specie 4 " -variable D_group -value 4 -command "deposi_data 4" 
radiobutton $wr.top.right.group.rad5 -text "Specie 5 " -variable D_group -value 5 -command "deposi_data 5" 
radiobutton $wr.top.right.group.rad6 -text "Specie 6 " -variable D_group -value 6 -command "deposi_data 6" 
radiobutton $wr.top.right.group.rad7 -text "Specie 7 " -variable D_group -value 7 -command "deposi_data 7" 
pack $wr.top.right.group.rad1 $wr.top.right.group.rad2 $wr.top.right.group.rad3 $wr.top.right.group.rad4 \
$wr.top.right.group.rad5 $wr.top.right.group.rad6 $wr.top.right.group.rad7  -side top

label $wr.sept.lab -text "________________________________________________________"
pack $wr.sept.lab -side left

button $wr.exit.quit -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.exit.help -text Help -width 8 \
       -command "load_html [file join $html_dir S311.htm ] "
button $wr.exit.done -bg green -text Save -width 8 -command "destroy $wr"
pack $wr.exit.quit $wr.exit.help $wr.exit.done -side left -padx 30
init_group_data
}

#----------------------------------------------------------------------------------
proc init_group_data {} {
global D_group S_group P_group
set D_group 1
set S_group 1
set P_group 1
}

#----------------------------------------------------------------------------------
proc pollut_data {No} {
global html_dir Num_pollut Identis Emi_rates Emi_hours Rel_times Num_deposi
if {$No > $Num_pollut} {
          msg_box "ERROR - only $No groups selected, modify the NUMS parameter."
          return
       }

# number of deposition definitions should always equal number of pollutants
set Num_deposi $Num_pollut

if [winfo exists .polluinit] {destroy .polluinit}
set wr .polluinit
toplevel $wr
wm title $wr "Definition of Pollutant Group $No"
wm  geometry $wr +75+100

frame $wr.ide
frame $wr.rate
frame $wr.hour
frame $wr.time
frame $wr.exit
pack $wr.ide $wr.rate $wr.hour $wr.time $wr.exit \
-side top -fill x -expand 1 -pady 10

label $wr.ide.lab -text "Identification (<=4 char)     :"
entry $wr.ide.ent1 -textvariable Identi_n -width 20
pack $wr.ide.lab $wr.ide.ent1 -side left

label $wr.rate.lab -text "Emission rate(1/hr)           :"
entry $wr.rate.ent1 -textvariable Emi_rate_n -width 20
pack $wr.rate.lab $wr.rate.ent1 -side left

label $wr.hour.lab -text "Hours of emission             :" 
entry $wr.hour.ent1 -textvariable Emi_hour_n -width 20
pack $wr.hour.lab $wr.hour.ent1 -side left

label $wr.time.lab -text "Release start(yy mm dd hh min):"
entry $wr.time.ent1 -textvariable Rel_time_n -width 20
pack $wr.time.lab $wr.time.ent1 -side left

button $wr.exit.quit -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.exit.help -text Help -width 8 \
       -command "load_html [file join $html_dir S312.htm ] "
button $wr.exit.done -bg green -text Save -width 8 -command "end_pollut $No"
pack $wr.exit.quit $wr.exit.help $wr.exit.done -side left -padx 5
init_pollut $No
}

#--------------------------------------------------------------------------
proc init_pollut {No} {
global Identis Emi_rates Emi_hours Rel_times Num_pollut
global Identi_n Emi_rate_n Emi_hour_n Rel_time_n 

if {$No > [llength $Identis]} {set No [expr $No-1]}

set Identi_n [lindex $Identis [expr $No-1]]
if { $Identi_n == ""} { set Identi_n [lindex $Identis 0]}
set Emi_rate_n [lindex $Emi_rates [expr $No-1]]
if { $Emi_rate_n == ""} { set Emi_rate_n [lindex $Emi_rates 0]}
set Emi_hour_n [lindex $Emi_hours [expr $No-1]]
if { $Emi_hour_n == ""} { set Emi_hour_n  [lindex $Emi_hours 0]}
set Rel_time_n [lindex $Rel_times [expr $No-1]]
if { $Rel_time_n == ""} { set Rel_time_n [lindex $Rel_times 0]}
}

#--------------------------------------------------------------------------
proc end_pollut {No} {
global Identis Emi_rates Emi_hours Rel_times Num_pollut
global Identi_n Emi_rate_n Emi_hour_n Rel_time_n
set t1 ""
set t2 ""
set t3 ""
set t4 ""

for {set i 1} {$i<=$Num_pollut} {incr i} {
if {$i == $No} {
     lappend t1 $Identi_n
     lappend t2 $Emi_rate_n
     lappend t3 $Emi_hour_n
     lappend t4 $Rel_time_n
         }  else {
       lappend t1 [lindex $Identis [expr $i-1]]
       lappend t2 [lindex $Emi_rates [expr $i-1]]
       lappend t3 [lindex $Emi_hours [expr $i-1]]
       lappend t4 [lindex $Rel_times [expr $i-1]]
                }
   }
set Identis   $t1
set Emi_rates $t2
set Emi_hours $t3
set Rel_times $t4
destroy .polluinit
}

#---------------------------------------------------------------------------------
proc simult_data {No} {
global html_dir Num_simult 
if {$No > $Num_simult} {
          msg_box "ERROR - only $No groups selected, modify the NUMS parameter."
          return
       }

if [winfo exists .simultdata] {destroy .simultdata}
set wr .simultdata
toplevel $wr
wm title $wr "Definition of Concentration Grid $No"
wm  geometry $wr +100+50

frame $wr.cen
frame $wr.spa
frame $wr.lalo
frame $wr.dir
frame $wr.name
frame $wr.ver
frame $wr.hei
frame $wr.sam
frame $wr.sto
frame $wr.int
frame $wr.exit
pack $wr.cen $wr.spa $wr.lalo $wr.dir $wr.name $wr.ver $wr.hei \
$wr.sam $wr.sto $wr.int $wr.exit -side top -fill x -expand 1 -pady 10

label $wr.cen.lab -text "Center of Lat and Lon          :"
entry $wr.cen.ent -textvariable Cen_lalo_n -width 20
pack $wr.cen.lab $wr.cen.ent -side left

label $wr.spa.lab -text "Spacing(deg) Lat, Lon          :"
entry $wr.spa.ent -textvariable Space_lalo_n -width 20
pack $wr.spa.lab $wr.spa.ent -side left

label $wr.lalo.lab -text "Span (deg) Lat, Lon            :" 
entry $wr.lalo.ent -textvariable Span_lalo_n -width 20
pack $wr.lalo.lab $wr.lalo.ent -side left 
label $wr.dir.lab -text "Output grid directory          :" 
entry $wr.dir.ent -textvariable Grid_dir_n -width 20
pack $wr.dir.lab $wr.dir.ent -side left 

label $wr.name.lab -text "Output grid file name          :" 
entry $wr.name.ent -textvariable Grid_name_n -width 20
pack $wr.name.lab $wr.name.ent -side left 

label $wr.ver.lab -text "Num of vertical levels         :"
entry $wr.ver.ent -textvariable Num_ver_n -width 20
pack $wr.ver.lab $wr.ver.ent -side left

label $wr.hei.lab -text "Height of levels(M Agl)        :"
entry $wr.hei.ent -textvariable Hei_agl_n -width 20
pack $wr.hei.lab $wr.hei.ent -side left

label $wr.sam.lab -text "Sampling start(yy mm dd hh min):"
entry $wr.sam.ent -textvariable Sam_start_n -width 20
pack $wr.sam.lab $wr.sam.ent -side left

label $wr.sto.lab -text "Sampling stop(yy mm dd hh min) :" 
entry $wr.sto.ent -textvariable Sam_stop_n -width 20
pack $wr.sto.lab $wr.sto.ent -side left 

label $wr.int.lab -text "(Avg:0 Now:1 Max:2) (hrs) (min):"
entry $wr.int.ent -textvariable Sam_int_n -width 20
pack $wr.int.lab $wr.int.ent -side left

button $wr.exit.exit -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.exit.help -text Help -width 8 \
       -command "load_html [file join $html_dir S313.htm ] "
button $wr.exit.done -bg green -text Save -width 8 -command "end_simult $No"
pack $wr.exit.exit $wr.exit.help $wr.exit.done -side left -padx 20
init_simult_data $No
}


#----------------------------------------------------------------------------------------
proc init_simult_data {No} {
global Num_simult Cen_lalo Space_lalo Span_lalo Grid_dir Grid_name Num_ver
global Hei_agl Sam_start Sam_stop Sam_int
global Cen_lalo_n Space_lalo_n Span_lalo_n Grid_dir_n Grid_name_n Num_ver_n
global Hei_agl_n Sam_start_n Sam_stop_n Sam_int_n
if {$No > [llength $Cen_lalo]} {set No [expr $No-1]}
set Cen_lalo_n   [lindex $Cen_lalo [expr $No-1]]
if { $Cen_lalo_n == ""} { set Cen_lalo_n [lindex $Cen_lalo 0]}
set Space_lalo_n [lindex $Space_lalo [expr $No-1]]
if { $Space_lalo_n == ""} { set Space_lalo_n [lindex $Space_lalo 0]}
set Span_lalo_n  [lindex $Span_lalo [expr $No-1]]
if { $Span_lalo_n == ""} { set Span_lalo_n [lindex $Span_lalo 0]}
set Grid_dir_n   [lindex $Grid_dir [expr $No-1]]
if { $Grid_dir_n == ""} { set Grid_dir_n [lindex $Grid_dir 0]}
set Grid_name_n  [lindex $Grid_name [expr $No-1]]
if { $Grid_name_n == ""} { set Grid_name_n [lindex $Grid_name 0]}
set Num_ver_n    [lindex $Num_ver [expr $No-1]]
if { $Num_ver_n == ""} { set Num_ver_n [lindex $Num_ver 0]}
set Hei_agl_n    [lindex $Hei_agl [expr $No-1]]
if { $Hei_agl_n == ""} { set Hei_agl_n [lindex $Hei_agl 0]}
set Sam_start_n  [lindex $Sam_start [expr $No-1]]
if { $Sam_start_n == ""} { set Sam_start_n [lindex $Sam_start 0]}
set Sam_stop_n   [lindex $Sam_stop [expr $No-1]]
if { $Sam_stop_n == ""} { set Sam_stop_n [lindex $Sam_stop 0]}
set Sam_int_n    [lindex $Sam_int [expr $No-1]]
if { $Sam_int_n == ""} { set Sam_int_n [lindex $Sam_int 0]}
}

#------------------------------------------------------------------------------
proc end_simult {No} {
global Num_simult Cen_lalo Space_lalo Span_lalo Grid_dir Grid_name Num_ver
global Hei_agl Sam_start Sam_stop Sam_int
global Cen_lalo_n Space_lalo_n Span_lalo_n Grid_dir_n Grid_name_n Num_ver_n
global Hei_agl_n Sam_start_n Sam_stop_n Sam_int_n

set t1 ""
set t2 ""
set t3 ""
set t4 ""
set t5 ""
set t6 ""
set t7 ""
set t8 ""
set t9 ""
set t10 ""

for {set i 1} {$i<=$Num_simult} {incr i} {
if {$i == $No} {
     lappend t1 $Cen_lalo_n
     lappend t2 $Space_lalo_n
     lappend t3 $Span_lalo_n
     lappend t4 $Grid_dir_n
     lappend t5 $Grid_name_n
     lappend t6 $Num_ver_n
     lappend t7 $Hei_agl_n
     lappend t8 $Sam_start_n 
     lappend t9 $Sam_stop_n
     lappend t10 $Sam_int_n
         }  else {
       lappend t1 [lindex $Cen_lalo [expr $i-1]]
       lappend t2 [lindex $Space_lalo [expr $i-1]]
       lappend t3 [lindex $Span_lalo [expr $i-1]]
       lappend t4 [lindex $Grid_dir [expr $i-1]]
       lappend t5 [lindex $Grid_name [expr $i-1]]
       lappend t6 [lindex $Num_ver [expr $i-1]]
       lappend t7 [lindex $Hei_agl [expr $i-1]]
       lappend t8 [lindex $Sam_start [expr $i-1]]
       lappend t9 [lindex $Sam_stop [expr $i-1]]
       lappend t10 [lindex $Sam_int [expr $i-1]]
   }
}
set  Cen_lalo    $t1
set  Space_lalo  $t2
set  Span_lalo   $t3
set  Grid_dir    $t4
set  Grid_name   $t5
set  Num_ver     $t6
set  Hei_agl     $t7
set  Sam_start   $t8
set  Sam_stop    $t9
set  Sam_int     $t10

destroy .simultdata
}

#-------------------------------------------------------------------------------------
proc deposi_data {No} {
global Identis Polid Ident0 
global html_dir Num_deposi
global vpar vdry vwet vpol

if {$No > $Num_deposi} {
          msg_box "ERROR - only $No groups selected, modify the NUMS parameter."
          return
       }

set Polid [expr $No-1]
set Ident0 [lindex $Identis $Polid]

if [winfo exists .depoinit] {destroy .depoinit}
set wr .depoinit
toplevel $wr
wm title $wr "Deposition Definition for Pollutant $No"
wm  geometry $wr +125+150

frame $wr.setd
frame $wr.type
frame $wr.gravit
frame $wr.flux
frame $wr.wet
frame $wr.decay
frame $wr.susp
frame $wr.exit
pack  $wr.setd $wr.type $wr.gravit $wr.flux $wr.wet $wr.decay $wr.susp \
$wr.exit -side top -fill x -expand 1 -pady 10

frame $wr.setd.lab
frame $wr.setd.par
frame $wr.setd.dry
frame $wr.setd.wet
pack $wr.setd.lab $wr.setd.par $wr.setd.dry $wr.setd.wet -side left -padx 10

# Simple default values

label $wr.setd.lab.one -text "Set Simple" -fg blue
label $wr.setd.lab.two -text "Defaults->" -fg blue
pack $wr.setd.lab.one $wr.setd.lab.two
 
label $wr.setd.par.lab -text "Particle or Gas" 
radiobutton $wr.setd.par.on  -text "Particle" -variable vpar -value "1" \
  -command {set Particles_n "5.0 6.0 1.0"
            set Molecular_n "0.0 0.0 0.0 0.0 0.0"
            set vdry 1}
radiobutton $wr.setd.par.off -text "Gas"      -variable vpar -value "0" \
  -command {set Particles_n "0.0 0.0 0.0"
            set Molecular_n "0.0 0.0 0.0 0.0 0.0"
            set vdry 0}
pack $wr.setd.par.lab
pack $wr.setd.par.on $wr.setd.par.off -side left -padx 2

label $wr.setd.dry.lab -text "Dry Deposition" 
radiobutton $wr.setd.dry.on  -text "Yes" -variable vdry -value "1" \
  -command {set Molecular_n "0.006 0.0 0.0 0.0 0.0"}
radiobutton $wr.setd.dry.off -text "No"  -variable vdry -value "0" \
  -command {set Molecular_n "0.0 0.0 0.0 0.0 0.0"}
pack $wr.setd.dry.lab
pack $wr.setd.dry.on $wr.setd.dry.off -side left -padx 2

label $wr.setd.wet.lab -text "Wet Deposition" 
radiobutton $wr.setd.wet.on  -text "Yes" -variable vwet -value "1" \
  -command { if { $vpar == "1" } {set Wet_remove_n "0.0 8.0E-05 8.0E-05"
                } else {set Wet_remove_n "3.0 0.0 0.0"} }
radiobutton $wr.setd.wet.off -text "No"  -variable vwet -value "0" \
  -command {set Wet_remove_n "0.0 0.0 0.0"}
pack $wr.setd.wet.lab
pack $wr.setd.wet.on $wr.setd.wet.off -side left -padx 2

# Preconfigured defaults

label $wr.type.lab -text "Preconfigure: " -fg blue
radiobutton $wr.type.p0 -text "None" -variable vpol -value "0"  \
  -command {reset_dep; set Identis [lreplace $Identis $Polid $Polid $Ident0]}
radiobutton $wr.type.p1 -text "Cs137" -variable vpol -value "1" \
  -command {set Particles_n "1.0 1.0 1.0"
            set Molecular_n "0.001 0.0 0.0 0.0 0.0"
            set Wet_remove_n "0.0 8.0E-05 8.0E-05"
            set Decay_day_n 10960.0
            set vpar 1; set vdry 1; set vwet 1
            set Identis [lreplace $Identis $Polid $Polid C137]}
radiobutton $wr.type.p2 -text "I131g" -variable vpol -value "2" \
  -command {set Particles_n "0.0 0.0 0.0"
            set Molecular_n "0.01 0.0 0.0 0.0 0.0"
            set Wet_remove_n "3.0 0.0 0.0"
            set Decay_day_n 8.0
            set vpar 0; set vdry 1; set vwet 1
            set Identis [lreplace $Identis $Polid $Polid I131]}
radiobutton $wr.type.p3 -text "I131p" -variable vpol -value "3" \
  -command {set Particles_n "1.0 1.0 1.0"
            set Molecular_n "0.001 0.0 0.0 0.0 0.0"
            set Wet_remove_n "0.0 4.0E-05 4.0E-05"
            set Decay_day_n 8.0
            set vpar 1; set vdry 1; set vwet 1
            set Identis [lreplace $Identis $Polid $Polid I131]}
radiobutton $wr.type.p4 -text "HTO" -variable vpol -value "4" \
  -command {set Particles_n "1.0 1.0 1.0"
            set Molecular_n "0.0004 0.0 0.0 0.0 0.0"
            set Wet_remove_n "0.0 8.0E-05 8.0E-05"
            set Decay_day_n 4490.0
            set vpar 1; set vdry 1; set vwet 1
            set Identis [lreplace $Identis $Polid $Polid HTO]}
radiobutton $wr.type.p5 -text "FMDV" -variable vpol -value "5" \
  -command {set Particles_n "1.0 1.0 1.0"
            set Molecular_n "0.01 0.0 0.0 0.0 0.0"
            set Wet_remove_n "0.0 8.0E-05 8.0E-05"
            set Decay_day_n 0.0
            set vpar 1; set vdry 1; set vwet 1
            set Identis [lreplace $Identis $Polid $Polid FM_7]}
pack  $wr.type.lab $wr.type.p0 $wr.type.p1 $wr.type.p2 $wr.type.p3 \
      $wr.type.p4 $wr.type.p5 -side left -padx 1

# Individual entry

label $wr.gravit.lab -text "Particle Diameter(um), Density(g/cc), Shape  :" 
entry $wr.gravit.ent -textvariable Particles_n -width 20
pack $wr.gravit.lab  $wr.gravit.ent -side left 

label $wr.flux.lab -text "Vel(m/s), Mol Wgt(g), A-Ratio, D-Ratio, Henry:"
entry $wr.flux.ent -textvariable Molecular_n -width 20
pack $wr.flux.lab $wr.flux.ent -side left

label $wr.wet.lab -text "Henry's(M/a), In-cloud(1/s), Below-cloud(1/s):"
entry $wr.wet.ent -textvariable Wet_remove_n -width 20
pack  $wr.wet.lab $wr.wet.ent -side left

label $wr.decay.lab -text "Radioactive decay half-life(days)            :" 
entry $wr.decay.ent -textvariable Decay_day_n -width 20
pack $wr.decay.lab $wr.decay.ent -side left 

label $wr.susp.lab -text "Pollutant Resuspension Factor(1/m)           :"
entry $wr.susp.ent -textvariable Resuspension_n -width 20
pack   $wr.susp.lab $wr.susp.ent -side left

button $wr.exit.quit -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.exit.help -text Help -width 8 \
       -command "load_html [file join $html_dir S314.htm ] "
button $wr.exit.reset -text Reset -width 20 -command {reset_dep; set Identis [lreplace $Identis $Polid $Polid $Ident0]}
button $wr.exit.done -bg green -text Save -width 8 -command "end_deposi $No"
pack $wr.exit.quit $wr.exit.help $wr.exit.reset $wr.exit.done -side left -padx 20
init_deposi_data $No
}

#-----------------------------------------------------------------------------------
proc reset_dep {} {
global vpar vdry vwet vpol
global Particles_n  Molecular_n Wet_remove_n Decay_day_n Resuspension_n 
set Particles_n     "0.0 0.0 0.0"
set Molecular_n     "0.0 0.0 0.0 0.0 0.0"
set Wet_remove_n    "0.0 0.0 0.0"
set Decay_day_n     "0.0"
set Resuspension_n  "0.0"
set vpar 0
set vdry 0
set vwet 0
set vpol 0
}

#-----------------------------------------------------------------------------------
proc init_deposi_data {No} {
global vpar vdry vwet vpol
global Particles  Molecular Wet_remove Decay_day Resuspension
global Particles_n  Molecular_n Wet_remove_n Decay_day_n Resuspension_n 
if {$No > [llength $Particles]} {set No [expr $No-1]}
set  Particles_n   [lindex $Particles [expr $No-1]]
if { $Particles_n == ""} { set Particles_n [lindex $Particles 0]}
set  Molecular_n  [lindex $Molecular [expr $No-1]]
if { $Molecular_n == ""} { set Molecular_n [lindex $Molecular 0]}
set  Wet_remove_n  [lindex $Wet_remove [expr $No-1]]
if { $Wet_remove_n == ""} { set Wet_remove_n [lindex $Wet_remove 0]}
set  Decay_day_n  [lindex $Decay_day [expr $No-1]]
if { $Decay_day_n == ""} { set Decay_day_n [lindex $Decay_day 0]}
set  Resuspension_n  [lindex $Resuspension [expr $No-1]]
if { $Resuspension_n == ""} { set Resuspension_n [lindex $Resuspension 0]}

if { $vpar == "" } {set vpar 0}
if { $vdry == "" } {set vdry 0}
if { $vwet == "" } {set vwet 0}
if { $vpol == "" } {set vpol 0}
}


#-----------------------------------------------------------------------------
proc end_deposi {No} {
global Particles  Molecular Wet_remove Decay_day Resuspension Num_deposi
global Particles_n  Molecular_n Wet_remove_n Decay_day_n Resuspension_n 
set t1 ""
set t2 ""
set t3 ""
set t4 ""
set t5 ""
for {set i 1} {$i<=$Num_deposi} {incr i} {
if {$i == $No} {
     lappend t1 $Particles_n
     lappend t2 $Molecular_n
     lappend t3 $Wet_remove_n
     lappend t4 $Decay_day_n
     lappend t5 $Resuspension_n
         }  else {
       lappend t1 [lindex $Particles [expr $i-1]]
       lappend t2 [lindex $Molecular [expr $i-1]]
       lappend t3 [lindex $Wet_remove [expr $i-1]]
       lappend t4 [lindex $Decay_day [expr $i-1]]
       lappend t5 [lindex $Resuspension [expr $i-1]]
                }
   }
set Particles $t1
set Molecular  $t2
set Wet_remove $t3
set Decay_day  $t4
set Resuspension $t5
destroy .depoinit
}


#------------------------------------------------------------------------------
proc save_to_control {} {
set f [open "default_conc" w]
write_prm_file $f
destroy .concinit 
}


#------------------------------------------------------------------------------
proc save_as_name {} {
if [winfo exists .savingdir] {destroy .savingdir}
global html_dir Save_path
set wr .savingdir
toplevel $wr
wm title $wr "Save Simulation by Name"
wm  geometry $wr +150+175

frame $wr.win
frame $wr.typ
frame $wr.bot
pack $wr.win $wr.typ $wr.bot -side top -pady 10

label $wr.win.lft -text "Enter Path/Name to save as CONTROL "
button $wr.win.rgt -text Browse -width 8 -command {
   set temp [tk_getSaveFile -initialdir $Save_path -title "File Selection"]
   if {[string length $temp] > 0} {
      set Name_saving $temp
      set Save_path [file dirname $temp]
      }
}
pack $wr.win.lft $wr.win.rgt -side left -padx 5

entry $wr.typ.ent -textvariable Name_saving -relief sunken -width 45
pack $wr.typ.ent -side left

button $wr.bot.dismiss -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help  -width 8 \
       -command "load_html [file join $html_dir S211.htm ] "
button $wr.bot.save -bg green -text "Save " -width 8 -command "save_as_name_s"
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 5 
bind $wr.typ.ent <Return> {save_as_name_s}
}


#-------------------------------------------------------------------------------
proc save_as_name_s {} {
global Name_saving
set f [open $Name_saving w]
write_prm_file $f
destroy .savingdir
}


#-------------------------------------------------------------------------------
proc retrieve_prm_file {} {
if [winfo exists .retridir] {destroy .retridir}
global html_dir Save_path
set wr .retridir
toplevel $wr
wm title $wr "Retrieve Previously Saved Simulation"
wm  geometry $wr +175+200

frame $wr.win
frame $wr.txt
frame $wr.bot
pack $wr.win $wr.txt $wr.bot -side top -pady 10

label $wr.win.lft -text "Enter Path/Name to load as CONTROL "
button $wr.win.rgt -text Browse -width 8 -command {
   set temp [tk_getOpenFile -initialdir $Save_path -title "File Selection"]
   if {[string length $temp] > 0} {
      set Name_retrive $temp
      set Save_path [file dirname $temp]
      }
}
pack $wr.win.lft $wr.win.rgt -side left -padx 5

entry $wr.txt.ent -textvariable Name_retrive -relief sunken -width 45
pack $wr.txt.ent -side left

button $wr.bot.dismiss -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help  -width 8 \
       -command "load_html [file join $html_dir S211.htm ] "
button $wr.bot.save -bg green -text "OK " -width 8 -command "retrive_prm_file_s"
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 5 
bind $wr.txt.ent <Return> {retrive_prm_file_s}
}


#---------------------------------------------------------------------------------
proc retrive_prm_file_s {} {
global Name_retrive 
set f [open $Name_retrive r]
.concinit.data.pick.src.list delete 0 end
read_prm_file $f 0
destroy .retridir
}


#---------------------------------------------------------------------------------
proc write_prm_file {f} {
global Top_model Grid_number
global Start_time Num_Start_location Totle_hours Use_vertical 
global Start_locn
puts $f "$Start_time"
puts $f "$Num_Start_location"

for { set i 1} {$i <= $Num_Start_location} {incr i} {   
    puts $f $Start_locn($i)
}

puts $f "$Totle_hours"
puts $f "$Use_vertical"
puts $f "$Top_model"
puts $f "$Grid_number"

for { set i 0} {$i <$Grid_number} {incr i} {   
    set tmp [.concinit.data.pick.dir.list get $i $i]
    if [ string match \{*\} $tmp ] {
       set Grids_path [string range $tmp 1 end-1]
    } else { 
       set Grids_path $tmp
    }
    puts $f "${Grids_path}/"

    set Grids_name [.concinit.data.pick.src.list get $i $i]
    puts $f "$Grids_name"
}

#writing data to pollut part
global Num_pollut Identis Emi_rates Emi_hours Rel_times
puts $f "$Num_pollut"
for {set ii 0} {$ii < $Num_pollut} {incr ii} {
if {[lindex $Identis $ii] == ""} {set i 0} else {set i $ii}
set tmpdata [lindex $Identis $i]
puts $f "$tmpdata"
set tmpdata [lindex $Emi_rates $i]
puts $f "$tmpdata"
set tmpdata [lindex $Emi_hours $i]
puts $f "$tmpdata"
set tmpdata [lindex $Rel_times $i]
puts $f "$tmpdata"
}
#third part
global Num_simult Cen_lalo Space_lalo Span_lalo Grid_dir Grid_name Num_ver
global Hei_agl Sam_start Sam_stop Sam_int
puts $f "$Num_simult"
for {set ii 0} {$ii < $Num_simult} {incr ii} {
if {[lindex $Cen_lalo $ii] == ""} {set i 0} else {set i $ii}
puts $f "[lindex $Cen_lalo $i]"
puts $f "[lindex $Space_lalo $i]"
puts $f "[lindex $Span_lalo $i]"
puts $f "[lindex $Grid_dir $i]"
puts $f "[lindex $Grid_name $i]"
puts $f "[lindex $Num_ver $i]"
puts $f "[lindex $Hei_agl $i]"
puts $f "[lindex $Sam_start $i]"
puts $f "[lindex $Sam_stop $i]"
puts $f "[lindex $Sam_int $i]"
}
# fourth prat parmeters store
global Num_deposi Particles Molecular Wet_remove  Decay_day  Resuspension 
puts $f "$Num_deposi"
for {set ii 0} {$ii < $Num_deposi} {incr ii} {
if {[lindex $Particles $ii] == ""} {set i 0} else {set i $ii}
puts $f "[lindex $Particles $i]"
puts $f "[lindex $Molecular $i]"
puts $f "[lindex $Wet_remove $i]"
puts $f "[lindex $Decay_day $i]"
puts $f "[lindex $Resuspension $i]"

}
close $f
}


#-----------------------------------------------------------------------------
proc retri_var {var_str num loop} {
set out ""
if { $loop > 1} {
    for { set i 0} {$i<$num} {incr i} {
      set N [expr $loop*$num-$loop-1] 
       set out [cancat  $out [lindex $var_str [expr $N+$i]]]
           }
    } else {
   set out $var_str
     }
return $out
}


#-----------------------------------------------------------------------------
proc start_loc {} {
global Num_Start_location kmsl
if [winfo exists .start_loc_setup] {destroy .start_loc_setup}
set wr .start_loc_setup
toplevel $wr
wm title $wr "Starting Location Setup"
wm  geometry $wr +100+100

set zunits(0) "(m-AGL)"
set zunits(1) "(m-MSL)"
set zunits(2) "(f-PBL)"
if [ info exists kmsl ] { } else {
   set kmsl 0
}

frame $wr.title
frame $wr.loc
frame $wr.end
pack $wr.title $wr.loc $wr.end -side top

label  $wr.title.lab1 -text "Set up $Num_Start_location Starting Locations"
label  $wr.title.lab2 -text "Latitude Longitude Height $zunits($kmsl)"
pack $wr.title.lab1 $wr.title.lab2

for {set d 1} {$d <= $Num_Start_location} {incr d} {
   frame $wr.loc.dat$d
   pack $wr.loc.dat$d -side top
   label $wr.loc.dat$d.lab -text "Location $d :"
   entry $wr.loc.dat$d.ent -textvariable Start_locn($d) -width 25
   button $wr.loc.dat$d.tab -text List -width 5 -command "set_start $d"
   pack $wr.loc.dat$d.lab $wr.loc.dat$d.ent $wr.loc.dat$d.tab -padx 2 -side left
}

button $wr.end.dismiss -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.end.help -bg green -text OK  -width 8 -command "destroy $wr"
pack $wr.end.dismiss $wr.end.help -side left  -padx 20
init_start_loc $Num_Start_location
}


#------------------------------------------------------------------------
proc init_start_loc {Num} {
global Start_locn zunits

for {set i 1} { $i <= $Num} { incr i} {
    if [info exists Start_locn($i)] {
       if {$Start_locn($i)==""} {
          set Start_locn($i) $Start_locn(1)
       } 
    }
}
}

#-----------------------------------------------------------------
proc conc_vert {} {
global html_dir
global Use_vertical Use_listing Use_text

if [winfo exists .vert_conc] {destroy .vert_conc}
set wr .vert_conc
toplevel $wr
wm title $wr "Vertical Motion Method"
wm  geometry $wr +100+100

frame $wr.select
frame $wr.end
pack  $wr.select $wr.end -pady 10

set temp $Use_vertical

#-->vertical motion
label $wr.select.lab -text "Vertical Motion Method"
radiobutton $wr.select.d0 -text [lindex $Use_listing 0] -variable Use_vertical -value "0" 
radiobutton $wr.select.d1 -text [lindex $Use_listing 1] -variable Use_vertical -value "1" 
radiobutton $wr.select.d2 -text [lindex $Use_listing 2] -variable Use_vertical -value "2" 
radiobutton $wr.select.d3 -text [lindex $Use_listing 3] -variable Use_vertical -value "3" 
radiobutton $wr.select.d4 -text [lindex $Use_listing 4] -variable Use_vertical -value "4" 
radiobutton $wr.select.d5 -text [lindex $Use_listing 5] -variable Use_vertical -value "5"
radiobutton $wr.select.d6 -text [lindex $Use_listing 6] -variable Use_vertical -value "6" 
radiobutton $wr.select.d7 -text [lindex $Use_listing 7] -variable Use_vertical -value "7"
radiobutton $wr.select.d8 -text [lindex $Use_listing 8] -variable Use_vertical -value "8"  

pack $wr.select.lab 
pack $wr.select.d0 $wr.select.d1 $wr.select.d2 $wr.select.d3 \
     $wr.select.d4 $wr.select.d5 $wr.select.d6 $wr.select.d7 $wr.select.d8

button $wr.end.dismiss -bg red -text Quit -width 10 -command "set Use_vertical $temp; destroy $wr"
button $wr.end.help -text Help -width 8 -command "load_html [file join $html_dir S212.htm]"
button $wr.end.reset -bg green -text OK -width 10 -command "set_cvert; destroy $wr"
pack $wr.end.dismiss $wr.end.help $wr.end.reset -side left -padx 5
}

proc set_cvert {} {
global Use_vertical Use_listing Use_text
set Use_text [lindex $Use_listing $Use_vertical]
}


#---------------------------------------------------------------------
proc set_start {Num} {
global set_loc start_loc_file
# select starting point from preselected list 
if [winfo exists .setstart] {destroy .setstart}
set wr .setstart
toplevel $wr
wm title $wr " Select Computational Starting Point "
wm  geometry $wr +50+15

frame $wr.top -width 40 
frame $wr.buttons
scrollbar $wr.scroll -command "$wr.list yview"
listbox $wr.list -yscroll "$wr.scroll set" -relief sunken -width 40

button $wr.cancel -bg red -text "Cancel" -width 10 -command {destroy .setstart}
button $wr.ok -bg green -text "Select" -width 10 -command putout

pack $wr.top -side top -fill x
pack $wr.buttons -side top -fill x
pack $wr.scroll -in $wr.top -side right -fill y
pack $wr.list -in $wr.top -fill x
pack $wr.cancel -in $wr.buttons -side left -fill x
pack $wr.ok -in $wr.buttons -side right 

if [file exists $start_loc_file] {
   set f [open $start_loc_file]
   while {[gets $f line] >=0} {$wr.list insert end $line}
   close $f
} else { 
   $wr.list insert end "ERROR: Source location file not found!"
   $wr.list insert end "Requires file in working directory." 
   $wr.list insert end "Format: Country Latitude Longitude Name" 
   $wr.list insert end "US 40.0 -90.0 Site_name" 
}
set set_loc $Num

$wr.list selection set 0
bind $wr.list <Double-Button-1> "putout"

proc putout {} {
global set_loc
global Start_locn
set location  [selection get]
set latitude  [lindex $location 1]
set longitude [lindex $location 2]
set height    [lindex $Start_locn($set_loc) 2]
set name      [lindex $location 3]
set location [list $latitude $longitude $height]
set Start_locn($set_loc) $location
destroy .setstart
}
}
