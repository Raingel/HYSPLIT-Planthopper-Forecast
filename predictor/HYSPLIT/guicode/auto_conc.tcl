proc auto_conc { } {

#-----------------------------------------------------------------------------
# AUTO_CONC.TCL: Run multiple dispersion by looping by day and hour
# Last Revised: 27 May 2005 - initial version
#               14 Nov 2008 - renamed executables
#               18 Aug 2017 - generalized for TCM applications
#               13 Aug 2018 - simplified date increment function
#               07 Nov 2018 - specify gmt for date functions
#               12 May 2020 - fix Tcl runtime error
#-----------------------------------------------------------------------------

global html_dir DH NH ND Fnum tm_short
global Start_time

if [winfo exists .autoconc] {destroy .autoconc}
set wr .autoconc
toplevel $wr
wm title $wr " Automated Multiple Dispersion Simulations by Time "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.mid
frame $wr.low
frame $wr.tcm
frame $wr.bot
pack $wr.top $wr.mid $wr.tcm $wr.low $wr.bot -side top -pady 5 -padx 5

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "Execute a script to run multiple iterations of the concentration\
 calculation from the start time at the indicated interval. The CONTROL file\
 must have been configured in the setup menu using non-zero values for the\
 start time. Output file names are appended by the month day and hour\
 or sequentially numbered."
pack $wr.top.lab

#-->starting date
label $wr.mid.l1 -text "Start calculations at $Start_time and every"  
entry $wr.mid.e1 -textvariable DH -width 3
label $wr.mid.l2 -text "hr until"
entry $wr.mid.e2 -textvariable ND -width 3
label $wr.mid.l3 -text "days"
entry $wr.mid.e3 -textvariable NH -width 3
label $wr.mid.l4 -text "hr after start!"
pack  $wr.mid.l1 $wr.mid.e1 $wr.mid.l2 $wr.mid.e2 $wr.mid.l3 \
      $wr.mid.e3 $wr.mid.l4 -side left -padx 1

#--> 
checkbutton $wr.tcm.d0 -text "Shorten each new run duration by" -variable tm_short
entry $wr.tcm.e1 -textvariable DH -width 3
label $wr.tcm.l1 -text "hr"
pack  $wr.tcm.d0 $wr.tcm.e1 $wr.tcm.l1 -side left -pady 2 -padx 2

#-->file name convention
label $wr.low.lab -text "File name convention:"
radiobutton $wr.low.d0 -text "MMDDHH" -variable Fnum -value "0" 
radiobutton $wr.low.d1 -text ".000" -variable Fnum -value "1" 
pack $wr.low.lab $wr.low.d0 $wr.low.d1 -side left -padx 5
 
#-->bottom action buttons
button $wr.bot.dismiss -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help  -width 8 \
       -command "load_html [file join $html_dir S352.htm ] "
button $wr.bot.save -bg green -text "Execute Script" -width 20 -command {make_conc}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 
set_date
}

#======================================================
proc set_date {} {
global DH NH ND Fnum
global Totle_hours

if [ info exists DH ] { } else {set DH ""}

if { $DH == "" } {
#  use date as file numbering default
   set Fnum 0

   if { $Totle_hours >=24 } {
#     long-duration default start every 3h
      set DH 3
      set ND [expr $Totle_hours/24]
      set NH [expr $Totle_hours-$ND*24]
   } else {
#     short-duration start hourly
#     Delta_Hour
      set DH 1
#     Number_Days for all simulations
      set ND 0
#     Number_Hours for all simulations
      set NH $Totle_hours
   }
}
}

#======================================================
proc make_conc {} {

global X_dir exec_dir tcl_platform tm_short
global DH ND NH Fnum Totle_hours

set log [ScrollText .f]

# total time covered by all simulations
set NDH [expr {$ND*24 + $NH}]

if {$tm_short==1} {
   if {$NDH > $Totle_hours} {
      $log insert end "WARNING ... base simulation duration less than total simulation time!\n"
      $log insert end "Base= $Totle_hours  ...  All simulations= $NDH  (hours)\n"      
   }
}

set NN 0
while {$NN < $NDH} {

set f [open default_conc r]
set g [open CONTROL w]

# start time for the first simulation
gets $f line
set YY [lindex [split $line] 0]
set MM [lindex [split $line] 1]
set DD [lindex [split $line] 2]
set HH [lindex [split $line] 3]

if { $MM == 0 } {
   msg_box "Set explicit (non-zero) start time in CONTROL file"
   destroy .autoconc
   return
   }

set base [clock scan "$YY-$MM-$DD $HH:00:00" -format {%y-%m-%d %H:%M:%S} -gmt T]
set temp [clock add $base $NN hours -gmt T]
set date [clock format $temp -format {%y %m %d %H} -gmt T]

set YY [lindex [split $date] 0]
set MM [lindex [split $date] 1]
set DD [lindex [split $date] 2]
set HH [lindex [split $date] 3]

puts $g "$YY $MM $DD $HH"

# source locations
gets $f  Numb
puts $g $Numb
for { set i 1} { $i <=$Numb} {incr i} {
    gets $f  line
    puts $g $line
}

# duration, vertical, model-top
gets $f  hrun
if {$tm_short==1} {set hrun [expr {$hrun - $NN}]}
if {$hrun <= 0} {
   $log insert end "Run duration <=0 .... terminating simulation\n"
   destroy .autoconc
   return
   }

puts $g $hrun
gets $f  line 
puts $g $line
gets $f  line      
puts $g $line

# meteorology
gets $f  Numb
puts $g $Numb
for { set i 1} {$i<=$Numb} {incr i} {
    gets $f  line
    puts $g $line
    gets $f  line
    puts $g $line
}

# pollutants
gets $f  Numb
puts $g $Numb
for { set i 1} {$i<=$Numb} {incr i} {
    for { set j 1} {$j<=4} {incr j} {
        gets $f  line
        puts $g $line
    }
}

# concentration grids
gets $f  Numb
puts $g $Numb
for { set i 1} {$i<=$Numb} {incr i} {
    for { set j 1} {$j<=4} {incr j} {
        gets $f  line
        puts $g $line
    }

#   output file
    gets $f line 
    set file [string trim $line]
    if { $Fnum == 0 } {
       append file $MM$DD$HH
    } else {
       append file .[format "%3.3u" $Fnum]
       incr Fnum
    }
    puts $g $file

    for { set j 1} {$j<=5} {incr j} {
        gets $f  line
        puts $g $line
    }
}

# deposition 
gets $f  Numb
puts $g $Numb
for { set i 1} {$i<=$Numb} {incr i} {
    for { set j 1} {$j<=5} {incr j} {
        gets $f  line
        puts $g $line
    }
}

close $f
close $g

if { $Fnum == 0 } {set label ""} {set label $MM$DD$HH}

if {$tm_short==1} {
   $log insert end "Started calculation: $label $file  Duration: $hrun \n"
} else {
   $log insert end "Started calculation: $label $file \n"
}
update

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
      exec $exec_dir/hycs_std
   } else {
      exec $X_dir/xterm -fn fixed -e $exec_dir/hycs_std
   }
} else {
   exec $exec_dir/hycs_std.exe
}

# increment the NN hour counter to the next starting time
# set NN [expr {$NN + $DH}]
  incr NN $DH

}
$log insert end "Calculations completed!\n"
destroy .autoconc
}
