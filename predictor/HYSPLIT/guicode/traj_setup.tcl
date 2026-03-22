proc traj_init { } {

#----------------------------------------------------------------------------
# TRAJ_SETUP.TCL: trajectory configuration script
# Last Revised:   06 Feb 2002
#                 13 Aug 2002
#                 14 Apr 2004 - rename endpoints file variable
#                 02 Jul 2004 - test for exceeding GUI limit
#                 26 May 2005 - browse file menus
#                 16 May 2006 - variable starting point list
#                 21 May 2007 - start location to array
#                 30 Oct 2008 - added minutes to information
#                 18 Aug 2009 - handle directories with spaces
#                 23 Sep 2009 - removed delimiter from end of dir string
#                 15 Dec 2010 - refined delimiter for save/retrieve menu
#                 17 May 2012 - more vertical motion choices
#                 25 Jun 2012 - reverse sign vertical motion
#                 08 Nov 2013 - dynamically change AGL/MSL label
#                 13 Jun 2014 - strip comments after hash mark on retrieve
#                 16 Jun 2014 - syntax correction on hash test
#                 20 Nov 2015 - enclose mm in bracket rather than brace
#                 14 Sep 2016 - added Save_path
#                 15 Aug 2018 - fwrd/back button changes with sign of run
#----------------------------------------------------------------------------

global html_dir Trajpts_file Grids_path Grid_number
global Totle_hours Direct
global Use_vertical Use_listing Use_text

if [winfo exists .trajinit] {destroy .trajinit}
set wr .trajinit
toplevel $wr
wm title $wr "Trajectory Setup"
wm  geometry $wr +50+50

set Use_text ""

frame $wr.sttime
frame $wr.stloc
frame $wr.tottime
frame $wr.vertical
frame $wr.gridnum
frame $wr.data
frame $wr.sept
frame $wr.exit
pack $wr.sttime $wr.stloc $wr.tottime $wr.vertical $wr.gridnum $wr.data \
$wr.sept $wr.exit -side top -fill x -expand 1 -pady 10

#-->starting time and locations
label $wr.sttime.lab -text "      Starting time (YY MM DD HH \[mm\]): " 
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
label  $wr.vertical.lab -text "Vertical Motion Method: "
entry  $wr.vertical.ent -text Use_text -relief sunken -width 22
button $wr.vertical.val  -text "Select" -width 14 -command traj_vert
pack $wr.vertical.lab $wr.vertical.ent $wr.vertical.val -side left -padx 8

#-->output file
label $wr.gridnum.lab -text "Output (/path/file):"
entry $wr.gridnum.ent -textvariable Trajpts_file -relief sunken -width 25
button $wr.gridnum.win  -text Browse -width 14 -command {
   set temp [tk_getSaveFile -title "File Selection"]
   if {[string length $temp] > 0} {set Trajpts_file $temp}}
pack $wr.gridnum.lab $wr.gridnum.ent $wr.gridnum.win -side left -padx 10

#-->meteorology input
frame $wr.data.lab
frame $wr.data.pick
pack $wr.data.lab $wr.data.pick -side top
button $wr.data.lab.set  -text "Add Meteorology Files" -width 25 -command {
   do_file_list .trajinit.data.pick.src.list .trajinit.data.pick.dir.list
}
button $wr.data.lab.clr  -text "Clear" -width 5 -command { 
   .trajinit.data.pick.src.list delete 0 end
   .trajinit.data.pick.dir.list delete 0 end
   global Grid_number
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

label $wr.sept.lab \
-text " _________________________________________________________________"
pack $wr.sept.lab -side left

#-->termination
button $wr.exit.dismiss  -bg red -text Quit -width 10 -command "destroy $wr"
button $wr.exit.help -text Help -width 10 -command "load_html [file join $html_dir S210.htm]"
button $wr.exit.saveas  -text "Save as" -width 10 -command "save_as_def"
button $wr.exit.retr  -text Retrieve  -width 10 -command "retrieve_def"
button $wr.exit.save  -bg green -text "Save" -width 12 -command "save_control"

pack  $wr.exit.dismiss $wr.exit.help $wr.exit.saveas $wr.exit.retr $wr.exit.save\
-side left -padx 8

init_trajectory $wr.data.pick.dir.list $wr.data.pick.src.list
}


#--------------------------------------------------------------------------
proc init_trajectory {wd wf} {
global Start_time Num_Start_location Totle_hours 
global Use_vertical Use_listing Use_text
global Trajpts_file Top_model Grid_number Direct
global Start_locn

set f [open "default_traj" r]
gets $f Start_time
set Start_time [string trim $Start_time  ] 
gets $f Num_Start_location
for { set i 1} { $i <=$Num_Start_location} {incr i} {
    gets $f Start_loc
    set Start_locn($i) [string trim $Start_loc]
}
gets $f Totle_hours 
set Totle_hours [string trim $Totle_hours ] 
gets $f Use_vertical
set Use_vertical [string trim $Use_vertical ]  
gets $f Top_model
set Top_model [string trim $Top_model ]

gets $f Grid_number
set Grid_number [string trim $Grid_number ]
for { set i 1} {$i<=$Grid_number} {incr i} {   
    gets $f tmp
    set tmp [string trim $tmp]
    set Grids_path [string range $tmp 0 end-1]
    if {$wd!="notset"} {$wd insert end $Grids_path}

    gets $f Grids_name 
    set Grids_name [string trim $Grids_name]
    if {$wf!="notset"} {$wf insert end $Grids_name}
}

gets $f out_tmp
set Trajpts_dir [string trim $out_tmp ]
gets $f out_tmp 
set out_tmp [string trim $out_tmp]
set Trajpts_file  ""
append Trajpts_file $Trajpts_dir $out_tmp
close $f

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

#set direction flag
set Direct 0
if {$Totle_hours < 0} {set Direct 1}
}

#------------------------------------------------------------------------------
proc save_control {} {
global Start_time Num_Start_location Totle_hours Use_vertical Top_model Grid_number
global Trajpts_file
global Start_locn
set f [open "default_traj" w]
puts $f "$Start_time"
puts $f "$Num_Start_location"
for {set i 1} {$i <= $Num_Start_location} {incr i} {
    puts $f $Start_locn($i)
}
puts $f "$Totle_hours"
puts $f "$Use_vertical"
puts $f "$Top_model"
puts $f "$Grid_number"

for { set i 0} {$i <$Grid_number} {incr i} {   
    set tmp [.trajinit.data.pick.dir.list get $i $i]
    if [ string match \{*\} $tmp ] {
       set Grids_path [string range $tmp 1 end-1]
    } else { 
       set Grids_path $tmp
    }
    puts $f "${Grids_path}/"

    set Grids_name [.trajinit.data.pick.src.list get $i $i]
    puts $f "$Grids_name"
}
       
puts $f "[file dirname $Trajpts_file]/"
puts $f [file tail $Trajpts_file]
destroy .trajinit
close $f
}


#--------------------------------------------------------------------------
proc save_as_def {} {
if [winfo exists .savingdir] {destroy .savingdir}
global html_dir Save_path
set wr .savingdir
toplevel $wr
wm title $wr "Save Simulation by Name"
wm  geometry $wr +150+150

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
button $wr.bot.save -bg green -text "Save " -width 8 -command "save_save"
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 5 
bind $wr.typ.ent <Return> {save_save}
}


#-------------------------------------------------------------------------------
proc save_save {} {
global Start_time Totle_hours Use_vertical Top_model Grid_number
global Name_saving Trajpts_file Num_Start_location 
global Start_locn
set f [open $Name_saving w]
puts $f "$Start_time"
puts $f "$Num_Start_location "
for {set i 1} {$i <= $Num_Start_location} {incr i} {
    puts $f $Start_locn($i)
}
puts $f "$Totle_hours"
puts $f "$Use_vertical"
puts $f "$Top_model"
puts $f "$Grid_number"

for { set i 0} {$i <$Grid_number} {incr i} {  
    set tmp [.trajinit.data.pick.dir.list get $i $i] 
    regsub -all // "$tmp/" / Grids_path
    puts $f "$Grids_path"
    set Grids_name [.trajinit.data.pick.src.list get $i $i]
    puts $f "$Grids_name"
}
puts $f "[file dirname $Trajpts_file]/"
puts $f [file tail $Trajpts_file]
close $f
destroy .savingdir
}


#--------------------------------------------------------------------------------
proc retrieve_def {} {
if [winfo exists .retridir] {destroy .retridir}
global html_dir Save_path
set wr .retridir
toplevel $wr
wm title $wr "Retrieve Previously Save Simulation"
wm  geometry $wr +150+150

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
button $wr.bot.save -bg green -text "OK " -width 8 -command "retrive_retrive"
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 5 
bind $wr.txt.ent <Return> {retrive_retrive}
}

#----------------------------------------------------------------------------
proc retrive_retrive {} {
global Start_time Totle_hours Use_vertical Top_model Grid_number
global Grids_path Name_retrive Trajpts_file Num_Start_location 
global Start_locn Direct

set f [open $Name_retrive r]

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
  set Start_locn($i) [string trim $Start_loc] }

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
set hash [string first # $Grid_number]
if { $hash >= 0 } {set Grid_number [string range $Grid_number 0 $hash-1]}
set Grid_number [string trim  $Grid_number]
.trajinit.data.pick.dir.list delete 0 end
.trajinit.data.pick.src.list delete 0 end

for { set i 1} {$i<=$Grid_number} {incr i} {
    gets $f tmp
    set hash [string first # $tmp]
    if { $hash >= 0 } {set tmp [string range $tmp 0 $hash-1]}
    set tmp [string trim $tmp]
    set Grids_path [string range $tmp 0 end-1]
    .trajinit.data.pick.dir.list insert end $Grids_path

    gets $f Grids_name 
    set hash [string first # $Grids_name]
    if { $hash >= 0 } {set Grids_name [string range $Grids_name 0 $hash-1]}
    set Grids_name [string trim $Grids_name]
    .trajinit.data.pick.src.list insert end $Grids_name
}

gets $f out_tmp
set hash [string first # $out_tmp]
if { $hash >= 0 } {set out_tmp [string range $out_tmp 0 $hash-1]}
set Trajpts_dir [string trim $out_tmp]

gets $f out_tmp 
set hash [string first # $out_tmp]
if { $hash >= 0 } {set out_tmp [string range $out_tmp 0 $hash-1]}
set out_tmp [string trim $out_tmp]

set Trajpts_file  ""
append Trajpts_file $Trajpts_dir $out_tmp
destroy .retridir
}

#-----------------------------------------------------------------
proc traj_vert {} {
global html_dir
global Use_vertical Use_listing Use_text

if [winfo exists .vert_traj] {destroy .vert_traj}
set wr .vert_traj
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

button $wr.end.dismiss -bg red -text Quit -width 10 -command \
     "set Use_vertical $temp; destroy $wr"
button $wr.end.help -text Help -width 8 -command "load_html [file join $html_dir S212.htm]"
button $wr.end.reset -bg green -text OK -width 10 -command "set_tvert; destroy $wr"
pack $wr.end.dismiss $wr.end.help $wr.end.reset -side left  -padx 5
}

proc set_tvert {} {
global Use_vertical Use_listing Use_text
set Use_text [lindex $Use_listing $Use_vertical]
}

#-----------------------------------------------------------------
proc start_loc {} {
global Num_Start_location kmsl 
global Start_locn
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


#---------------------------------------------------------------------
proc init_start_loc {Num} {
global Start_locn Num_Start_location

for {set i 1} { $i <= $Num} { incr i} {
   if [info exists Start_locn($i)] {
      if {$Start_locn($i)==""} {
         set Start_locn($i) $Start_locn(1)
      }
   }
}
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


#---------------------------------------------------------------------
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
