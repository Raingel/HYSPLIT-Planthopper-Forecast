# the next line restarts using wish \
# for UNIX system uncomment the next line to start using wish \
# exec wish "$0" "$@"

ftp_act {NARR198212}
ftp_act {NARR198301}
ftp_act {NARR198302}
ftp_act {NARR198303}
ftp_act {NARR198304}
ftp_act {NARR198305}
ftp_act {NARR198306}
ftp_act {NARR198307}
ftp_act {NARR198308}

destroy .


#-----------------------------------------------------------------
proc ftp_act {data_file} {

global fsize

set host ftp.noaa.gov
set user anonymous
set pass user@email.net
set data_dir /pub/archives/narr

package require ftp
set handle [::ftp::Open     $host $user $pass -blocksize 1048576 -progress ftp_box 0]
set rcode  [::ftp::Cd       $handle $data_dir]
set rcode  [::ftp::Type     $handle binary]
set fsize  [::ftp::FileSize $handle $data_file]
set rcode  [::ftp::Get      $handle $data_file]
set rcode  [::ftp::Close    $handle]
if [winfo exists .ftp_win] {destroy .ftp_win}
}


#-----------------------------------------------------------------
proc ftp_box {dsize} {

global fsize

set ww .ftp_win
if [winfo exists $ww] {destroy $ww}

toplevel $ww
wm title $ww "Download Progress"
wm  geometry $ww +250+250

set tot 50
if {$fsize > 1000000} {
   set cbyte [expr $dsize / 1000000]
   set tbyte [expr $fsize / 1000000]
   set val   [expr $tot * $cbyte / $tbyte]
} else {
   set val   [expr $tot * $dsize / $fsize]
}

label $ww.totbox -text "$dsize" -bg yellow -relief sunken -width $tot
label $ww.valbox -bg blue   -relief raised -width $val
label $ww.botbox -text "$fsize" -bg yellow -relief sunken -width $tot
pack $ww.totbox $ww.valbox $ww.botbox -side top -anchor w
}
