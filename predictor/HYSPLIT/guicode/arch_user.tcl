proc user_arch {} {

#-----------------------------------------------------------------------------
# ARCH_USER.TCL: User created meteorological data file for a single location
# Last Revised: 04 Jul 2005 - original version
#               02 Nov 2012 - delete output file prior to running conversion
#               10 Jul 2013 - added repeat button for data entry
#               16 Mar 2017 - corrected missing dash in " -text"
#-----------------------------------------------------------------------------

global html_dir 
global Infile Outfile Inbase Outbase Clat Clon

if [winfo exists .usermet] {destroy .usermet}
set wr .usermet
toplevel $wr
wm title $wr " Create Single Station Meteorology File "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.mid0
frame $wr.mid1
frame $wr.mid2
frame $wr.mid3
frame $wr.mid4
frame $wr.mid5
frame $wr.bot
pack $wr.top $wr.mid0 $wr.mid1 $wr.mid2 $wr.mid3 $wr.mid4 \
     $wr.mid5 $wr.bot -side top -pady 5 -padx 10

#-->information
label $wr.top.lab -fg blue -justify left -wraplength 6i \
 -text "Create an ARL packed meteorological data file at 1 km resolution\
for user entered data at a single location for one or more time periods.\
Required input includes wind direction, speed, mixing depth, and stability,\
defined by categores 1 (unstable) through 7 (stable)."
pack $wr.top.lab

#-->input file    
label $wr.mid0.lab -text "Meteorological Data Input File: "
pack  $wr.mid0.lab

label $wr.mid1.lab1 -text "Latitude: "
entry $wr.mid1.ent1 -textvariable Clat -relief sunken -width 10
label $wr.mid1.lab2 -text "   Longitude: "
entry $wr.mid1.ent2 -textvariable Clon -relief sunken -width 10
pack  $wr.mid1.lab1 $wr.mid1.ent1 $wr.mid1.lab2 $wr.mid1.ent2 -side left

entry $wr.mid2.ent -textvariable Inbase -relief sunken -width 15
button $wr.mid2.but1  -text "Select file" -width 12 -command {
   set temp [tk_getOpenFile -title "File Selection"]
   if {[string length $temp] > 0} {set Infile $temp}
   set Inbase [file tail $Infile]}
button $wr.mid2.but2  -text "Create file" -width 12 -command {met_user}
pack $wr.mid2.ent $wr.mid2.but1 $wr.mid2.but2 -side left -padx 5

#-->output file    
label $wr.mid3.lab -text "Processed Data Output File: "
pack  $wr.mid3.lab
entry $wr.mid4.ent -textvariable Outbase -relief sunken -width 15
button $wr.mid4.but1  -text "Select file" -width 12 -command {
   set temp [tk_getOpenFile -title "File Selection"]
   if {[string length $temp] > 0} {set Outfile $temp}
   set Outbase [file tail $Outfile]}
button $wr.mid4.but2  -bg green -text "Run convert" -width 12 -command {
   run_user
   destroy .usermet}
pack $wr.mid4.ent $wr.mid4.but1 $wr.mid4.but2 -side left -padx 5

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help  -width 8 \
       -command "load_html [file join $html_dir S128.htm ] "
pack  $wr.bot.dismiss $wr.bot.help -side left -padx 10 
set_user
}

#--------------------------------------------------------------------------

proc set_user {} {

global YY MM DD HH mm WD WS MD ST
global Infile Outfile Inbase Outbase Clat Clon

set date [clock format [clock scan now] -format "%y %m %d %H %M"]
array set YY [list 0 [lindex [split $date] 0] 1 "" 2 "" 3 "" 4 "" 5 ""] 
array set MM [list 0 [lindex [split $date] 1] 1 "" 2 "" 3 "" 4 "" 5 ""]
array set DD [list 0 [lindex [split $date] 2] 1 "" 2 "" 3 "" 4 "" 5 ""]
array set HH [list 0 [lindex [split $date] 3] 1 "" 2 "" 3 "" 4 "" 5 ""]
array set mm [list 0 [lindex [split $date] 4] 1 "" 2 "" 3 "" 4 "" 5 ""]

array set WD [list 0 270    1 "" 2 "" 3 "" 4 "" 5 ""]
array set WS [list 0 5.0    1 "" 2 "" 3 "" 4 "" 5 ""]
array set MD [list 0 1500.0 1 "" 2 "" 3 "" 4 "" 5 ""]
array set ST [list 0 4      1 "" 2 "" 3 "" 4 "" 5 ""]

set Infile "stndata.txt"
set Outfile "stndata.bin"
set Inbase $Infile
set Outbase $Outfile
set Clat "40.0"
set Clon "-90.0"
}


#--------------------------------------------------------------------------

proc fill_user {} {

global YY MM DD HH mm WD WS MD ST

for {set d 1} {$d <= 5} {incr d} {
 set YY($d) $YY(0)
 set MM($d) $MM(0)
 set DD($d) $DD(0)
 set HH($d) $HH(0)
 set mm($d) $mm(0)
 set WD($d) $WD(0)
 set WS($d) $WS(0)
 set MD($d) $MD(0)
 set ST($d) $ST(0)
}

}


#--------------------------------------------------------------------------

proc run_user { } {

global X_dir exec_dir tcl_platform
global Infile Outfile Clat Clon

if [ file exists $Outfile ] {file delete $Outfile} 

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
      exec $exec_dir/stn2arl $Infile $Outfile $Clat $Clon
   } else {
      exec $X_dir/xterm -fn fixed -e $exec_dir/stn2arl $Infile $Outfile $Clat $Clon
   }
} else {
   exec $exec_dir/stn2arl.exe $Infile $Outfile $Clat $Clon
}

}


#--------------------------------------------------------------------------
proc met_user {} {

global YY MM DD HH mm WD WS MD ST

if [winfo exists .metuser] {destroy .metuser}
set wr .metuser
toplevel $wr
wm title $wr "Enter Meteorology Data"
wm  geometry $wr +150+150

frame $wr.top
frame $wr.dat0
frame $wr.dat1
frame $wr.dat2
frame $wr.dat3
frame $wr.dat4
frame $wr.dat5
frame $wr.bot
pack $wr.top $wr.dat0 $wr.dat1 $wr.dat2 $wr.dat3 $wr.dat4 $wr.dat5 \
             $wr.bot -side top -pady 2

label $wr.top.lab -text "Year Mon Day Hour  Min  Dir  Spd MixLayer Stab"
pack $wr.top.lab -anchor w 

for {set d 0} {$d <= 5} {incr d} {
  entry $wr.dat$d.ent0 -textvariable YY($d) -relief sunken -width 3
  entry $wr.dat$d.ent1 -textvariable MM($d) -relief sunken -width 3
  entry $wr.dat$d.ent2 -textvariable DD($d) -relief sunken -width 3
  entry $wr.dat$d.ent3 -textvariable HH($d) -relief sunken -width 3
  entry $wr.dat$d.ent4 -textvariable mm($d) -relief sunken -width 3
  entry $wr.dat$d.ent5 -textvariable WD($d) -relief sunken -width 4
  entry $wr.dat$d.ent6 -textvariable WS($d) -relief sunken -width 4
  entry $wr.dat$d.ent7 -textvariable MD($d) -relief sunken -width 6
  entry $wr.dat$d.ent8 -textvariable ST($d) -relief sunken -width 4
  pack $wr.dat$d.ent0 $wr.dat$d.ent1 $wr.dat$d.ent2 $wr.dat$d.ent3 $wr.dat$d.ent4 \
       $wr.dat$d.ent5 $wr.dat$d.ent6 $wr.dat$d.ent7 $wr.dat$d.ent8 -side left -padx 3
  }

button $wr.bot.dismiss  -text Quit -width 8 -command {destroy .metuser}
button $wr.bot.copy  -text "Repeat" -width 10 -command {fill_user}
button $wr.bot.save  -text "Save Data to File" -width 20 -command {
       save_user
       destroy .metuser}
pack  $wr.bot.dismiss $wr.bot.copy $wr.bot.save -side left -padx 3
}

#--------------------------------------------------------------------------

proc save_user {} {

global Infile
global YY MM DD HH mm WD WS MD ST

set f [open "$Infile" w]
for {set d 0} {$d <= 5} {incr d} {
   if { $YY($d) != "" } {
      puts $f "$YY($d) $MM($d) $DD($d) $HH($d) $mm($d) $WD($d) $WS($d) $MD($d) $ST($d) " 
   }
}
close $f
}
