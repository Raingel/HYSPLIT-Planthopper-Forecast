proc auto_traj { } {

#-----------------------------------------------------------------------------
# AUTO_TRAJ.TCL: Run multiple trajectories looping by day and hour
# Last Revised: 27 May 2005 - initial version
#               30 Aug 2005 - add year month to output filename
#               14 Nov 2008 - renamed executables
#               18 Aug 2017 - generalized date funtion for multi-months
#               14 Aug 2018 - simplified date increment function
#               07 Nov 2018 - specify gmt for date functions
#-----------------------------------------------------------------------------

global html_dir Start_time

if [winfo exists .autotraj] {destroy .autotraj}
set wr .autotraj
toplevel $wr
wm title $wr " Automated Multiple Trajectories by Time "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.mid
frame $wr.bot
pack $wr.top $wr.mid $wr.bot -side top -pady 5 -padx 5

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "Execute a script to run multiple iterations of the trajectory\
 calculation from the start time at the indicated interval. The CONTROL\
 file should have previously configured in the setup menu. Output file\
 names are appended with the year/month/day/hour."
pack $wr.top.lab

#-->starting date
label $wr.mid.l1 -text "Start calculations at $Start_time and every"  
entry $wr.mid.e1 -textvariable DHT -width 3
label $wr.mid.l2 -text "hr until"
entry $wr.mid.e2 -textvariable NDT -width 3
label $wr.mid.l3 -text "days"
entry $wr.mid.e3 -textvariable NHT -width 3
label $wr.mid.l4 -text "hr after start!"
pack  $wr.mid.l1 $wr.mid.e1 $wr.mid.l2 $wr.mid.e2 $wr.mid.l3 \
      $wr.mid.e3 $wr.mid.l4 -side left -padx 1
 
#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help  -width 8 \
       -command "load_html [file join $html_dir S254.htm ] "
button $wr.bot.save  -bg green -text "Execute Script" -width 20 -command {make_traj}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 
set_date
}

#======================================================
proc set_date {} {

global DHT NHT NDT

if [ info exists DHT ] { } else {set DHT ""}

if { $DHT == "" } {
#  Delta_Hour
   set DHT 6 
#  Number_Days for all simulations
   set NDT 30
#  Number_Hours for all simulations
   set NHT 0
   }
}

#======================================================
proc make_traj {} {
global X_dir exec_dir tcl_platform
global DHT NDT NHT

set log [ScrollText .f]

set NN 0
set NDH [expr {$NDT*24 + $NHT}]
while {$NN < $NDH} {

      set f [open default_traj r]
      set g [open CONTROL w]

    # start time
      gets $f line
      set YY [lindex [split $line] 0]
      set MM [lindex [split $line] 1]
      set DD [lindex [split $line] 2]
      set HH [lindex [split $line] 3]

      if { $MM == 0 } {
      msg_box "Set explicit (non-zero) start time in CONTROL file"
      destroy .autotraj
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
      gets $f  line 
      puts $g $line
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

    # output file
      gets $f  line 
      puts $g $line
      gets $f line 
      set line [string trim $line]
      append line $YY$MM$DD$HH
      puts $g $line

      close $f
      close $g

      $log insert end "Started calculation: $line \n"
      update
      if { "$tcl_platform(platform)" == "unix" } {
         if { "$X_dir" == "" } {
            exec $exec_dir/hyts_std
         } else {
            exec $X_dir/xterm -fn fixed -e $exec_dir/hyts_std
         }
      } else {
        exec $exec_dir/hyts_std.exe
      }

#     increment the NN hour counter to the next starting time
#     set NN [expr {$NN + $DHT}]
      incr NN $DHT
}
$log insert end "Calculations completed! \n"
destroy .autotraj
}
