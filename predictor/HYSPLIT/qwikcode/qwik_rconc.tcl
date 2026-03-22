proc run_conc {SCR EXE} {

#--------------------------------------------------------------
# Read HYSP.INI and create CONTROL and SETUP for concentration
# Last Revised: 08 Mar 2007
#               06 Jul 2011
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


#--------------------------------------------------------------
# custom configuration

set type [string range $title 0 4]


#---------------------------------------------------------------
# create file to label graphics maps

if [file exists MAPTEXT.conc] {file delete -force MAPTEXT.conc}

set date [clock format [clock scan now] -format "%y %m %d %H" -gmt 1]

set f [open "MAPTEXT.conc" w]
puts $f "NOAA PLUME PREDICTION CENTER"
puts $f " " 
puts $f "$title"
puts $f " "
puts $f "Source Location: $site  ($olat $olon)"
puts $f "Start Month Day: $month  $day"
puts $f "Start Time (UTC): $hour"
puts $f "Meteorology Data: $met1"
puts $f "Release heights: $rbot to $rtop meters"
puts $f "Emission Rate: $rate units per hr over $dur hrs"
if {$poll == "Test"} {
   puts $f "Deposition Options: none"
} else {
   puts $f "Deposition Options: default wet/dry/decay"
}
puts $f "Notes: hourly averaged concentration output"
puts $f " "
puts $f "Issue Date: $date "
puts $f " "
close $f


#----------------------------------------------------------
# set up control file for dispersion/concentration simulation

if [file exists CONTROL.conc] {file delete -force CONTROL.conc}
set f [open "CONTROL.conc" w]
puts $f "$year $month $day $hour"
puts $f "2"
puts $f "$olat $olon $rbot"
puts $f "$olat $olon $rtop"
puts $f "$run"
puts $f "0"
puts $f "10000"
puts $f "$nfile"
puts $f "$dir1"
puts $f "$met1"
puts $f "1"
puts $f "$poll"
puts $f "$rate"
puts $f "$dur"
puts $f "00 00 00 00 00"
puts $f "1"
puts $f "0.0  0.0"
switch $type {
   Local {puts $f "0.002 0.002"; puts $f "1.0  1.0"}
   Short {puts $f "0.005 0.005"; puts $f "2.0  2.0"}
   Urban {puts $f "0.002 0.002"; puts $f "1.0  1.0"}
   Long- {puts $f "0.010 0.010"; puts $f "5.0  5.0"}
}
puts $f "./"
puts $f "cdump"
puts $f "1"
puts $f "$zcon"
puts $f "00 00 00 00 00"
puts $f "00 00 00 00 00"
switch $type {
   Local {puts $f "00 $shr 00"}
   Short {puts $f "00 $shr 00"}
   Urban {puts $f "00 $shr 00"}
   Long- {puts $f "00 $shr 00"}
}
puts $f "1"
switch $poll {
   Test {
      puts $f "0.0 0.0 0.0"
      puts $f "0.0 0.0 0.0 0.0 0.0"
      puts $f "0.0 0.0 0.0"
      puts $f "0.0"}
   I131 {
      puts $f "1.0 1.0 1.0"
      puts $f "0.001 0.0 0.0 0.0 0.0"
      puts $f "0.0 4.0E+04 1.0E-04"
      puts $f "8.0"}
   C137 {
      puts $f "1.0 1.0 1.0"
      puts $f "0.001 0.0 0.0 0.0 0.0"
      puts $f "0.0 4.0E+04 5.0E-05"
      puts $f "10960.0"}
   }
puts $f "0.0"
close $f
file copy -force CONTROL.conc default_conc


#----------------------------------------------------------
# create the namelist file

set delt 0
set tratio 0.75
set initd 0
set kpuff 0 
set khmax 9999
set numpar 5000
set qcycle 0.0
set efile ""
set tkerd 0.18
switch $type {
   Local {set tkern 0.18}
   Short {set tkern 0.18}
   Urban {set tkern 0.22}
   Long- {set tkern 0.18}
}
set ninit 1
set ndump 1
set ncycl 1
set pinpf PARINIT
set poutf PARDUMP
set mgmin 10
set kmsl 0
set maxpar 5000
set cpack 1
set cmass 0
set dxf 1.0
set dyf 1.0
set dzf 0.01
set ichem 0
set maxdim 1
set p10f 1.0


set f [open "SETUP.CFG" w]
puts $f " &SETUP"
puts $f " delt = $delt,"
puts $f " tratio = $tratio,"
puts $f " initd = $initd," 
puts $f " kpuff = $kpuff,"
puts $f " khmax = $khmax,"
puts $f " numpar = $numpar,"
puts $f " qcycle = $qcycle,"
puts $f " efile = '$efile',"
puts $f " tkerd = $tkerd,"
puts $f " tkern = $tkern,"
puts $f " ninit = $ninit,"
puts $f " ndump = $ndump,"
puts $f " ncycl = $ncycl,"
puts $f " pinpf = '$pinpf',"
puts $f " poutf = '$poutf',"
puts $f " mgmin = $mgmin,"
puts $f " kmsl = $kmsl,"
puts $f " maxpar = $maxpar,"
puts $f " cpack = $cpack,"
puts $f " cmass = $cmass,"
puts $f " dxf = $dxf,"
puts $f " dyf = $dyf,"
puts $f " dzf = $dzf,"
puts $f " ichem = $ichem,"
puts $f " maxdim = $maxdim,"
puts $f " p10f = $p10f,"
puts $f " /"
close $f
file copy -force SETUP.CFG SETUP.conc


#----------------------------------------------------------
# run the concentration simulation

if [file exists cdump] {file delete cdump}

set xops "|${EXE}/hycs_std conc"

set log [ScrollText .clog]
$log configure -cursor watch
$log insert end "Model started ...\n"
update  
if [catch {open $xops} result] {
   $log insert end $result
} else {
   fileevent $result readable Log
}
tkwait window .clog
}


