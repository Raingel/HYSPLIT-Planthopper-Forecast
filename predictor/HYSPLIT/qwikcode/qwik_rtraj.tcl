proc run_traj {SCR EXE} {

#--------------------------------------------------------------
# Read HYSP.INI and create CONTROL and SETUP for trajectories
# Last Revised: 08 Mar 2007
#--------------------------------------------------------------

global result log
      
#--------------------------------------------------------------
# set model simulation variables    

set init HYSP.INI
if [file exists $init] {
   set f [open "$init" r]
   set k 0
   while {[gets $f cline] >= 0} {
      incr k
      if {$k == 1}  {set title $cline}
      if {$k == 2}  {set site  $cline}
      if {$k == 3}  {set olat  $cline}
      if {$k == 4}  {set olon  $cline}
      if {$k == 5}  {set nfile $cline}
      if {$k == 6}  {set dir1  $cline}
      if {$k == 7}  {set met1  $cline}
      if {$k == 8}  {set dir2  $cline}
      if {$k == 9}  {set met2  $cline}
      if {$k == 10} {set year  $cline}
      if {$k == 11} {set month $cline}
      if {$k == 12} {set day   $cline}
      if {$k == 13} {set hour  $cline}
      if {$k == 14} {set run   $cline}
      if {$k == 15} {set dur   $cline}
      if {$k == 16} {set rate  $cline}
      if {$k == 17} {set rbot  $cline}
      if {$k == 18} {set rtop  $cline}
      if {$k == 19} {set poll  $cline}
      if {$k == 20} {set syr   $cline}
      if {$k == 21} {set smo   $cline}
      if {$k == 22} {set sda   $cline}
      if {$k == 23} {set shr   $cline}
      if {$k == 24} {set zcon  $cline}
   }
   close $f

} else {
   info_form "MISSING FILE: $init" error Quit Exit \
              Configure with Setup before Run.
   tkwait window .info
   return
}

#---------------------------------------------------------------
# create file to label graphics maps

set date [clock format [clock scan now] -format "%y %m %d %H" -gmt 1]

if [file exists MAPTEXT.traj] {file delete -force MAPTEXT.traj}
set f [open "MAPTEXT.traj" w]
puts $f "NOAA PLUME PREDICTION CENTER"
puts $f " " 
puts $f "Diagnostic Trajectory Analysis"
puts $f " "
puts $f "Source Location: $site   ($olat   $olon)"
puts $f "Start Month Day: $month  $day"
puts $f "Start Time (UTC): $hour"
puts $f "Meteorology Data: $met1"
puts $f "Starting heights: 10 m, 500 m, and 1000 m AGL"
puts $f " "
puts $f " "
puts $f " "
puts $f " "
puts $f " "
puts $f "Issue Date: $date "
puts $f " "
close $f


#----------------------------------------------------------
# create the namelist file for the trajectory simulation

set delt 0
set tratio 0.75
set mgmin 10
set khmax 9999
set kmsl 0
set nstr 0
set mhrs 9999
set nver 0
set tout 15
set tm_tpot 0
set tm_tamb 0
set tm_rain 0
set tm_mixd 0
set tm_relh 0
set tm_dswf 0
set tm_terr 0
set dxf 1.0
set dyf 1.0
set dzf 0.01

set f [open "SETUP.CFG" w]
puts $f " &SETUP"
puts $f " delt = $delt,"
puts $f " tratio = $tratio,"
puts $f " mgmin = $mgmin,"
puts $f " khmax = $khmax,"
puts $f " kmsl = $kmsl,"
puts $f " nstr = $nstr,"
puts $f " mhrs = $mhrs,"
puts $f " nver = $nver,"
puts $f " tout = $tout,"
puts $f " tm_tpot = $tm_tpot,"
puts $f " tm_tamb = $tm_tamb,"
puts $f " tm_rain = $tm_rain,"
puts $f " tm_mixd = $tm_mixd,"
puts $f " tm_relh = $tm_relh,"
puts $f " tm_dswf = $tm_dswf,"
puts $f " tm_terr = $tm_terr,"
puts $f " dxf = $dxf,"
puts $f " dyf = $dyf,"
puts $f " dzf = $dzf,"
puts $f " /"
close $f
file copy -force SETUP.CFG SETUP.traj



#----------------------------------------------------------
# create the control file for the trajectory simulation

if [file exists CONTROL.traj] {file delete -force CONTROL.traj}
set f [open "CONTROL.traj" w]
puts $f "$year $month $day $hour"
puts $f "3"
puts $f "$olat $olon   10.0"
puts $f "$olat $olon  500.0"
puts $f "$olat $olon 1000.0"
puts $f "3"
puts $f "0"
puts $f "10000"
puts $f "$nfile"
puts $f "$dir1"
puts $f "$met1"
puts $f "./"
puts $f "tdump"
close $f
file copy -force CONTROL.traj default_traj

#----------------------------------------------------------
# run the trajectory simulation

if [file exists tdump] {file delete tdump}

set xops "|${EXE}/hyts_std traj"

set log [ScrollText .tlog]
$log configure -cursor watch
$log insert end "Model started ...\n"
update  
if [catch {open $xops} result] {
   $log insert end $result
} else {
   fileevent $result readable Log
}
tkwait window .tlog
}


