#-----------------------------------------------------------------------------
# Auto_ftp.tcl
# for UNIX system uncomment the next line to start using wish \
# exec wish "$0" "$@"
#-----------------------------------------------------------------------------
# AUTO_FTP.TCL: ftp meteorological data 
# Last Revised: 1 Mar 2021
#-----------------------------------------------------------------------------

file delete MESSAGE
set f [open MESSAGE w]

set date [clock format [clock scan now] -format "%y %m %d %H" -gmt 1]
set MON [lindex [split $date] 1]
set DAY [lindex [split $date] 2]
set HRS [lindex [split $date] 3]

puts $f "Initiation time: $MON $DAY $HRS"
puts $f "-------------------------------"

if [info exists input] {unset input} {
   append input "user myaccountname mypassword \n"
   append input "lcd c:/hysplit4/working \n"
   append input "cd /pub/archives/eta12 \n"
   append input "binary\n"
   append input "prompt\n"
   append input "get ETA$DAY \n" 
   append input "bye\n"
   }
puts $f $input
close $f

exec ftp -n ftp.arl.noaa.gov << $input
destroy .
