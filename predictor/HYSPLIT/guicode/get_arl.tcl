proc get_arl {arch_fcst} {

#-----------------------------------------------------------------------------
# GET_ARL.TCL:  ftp/http meteorological data in Hysplit format from ARL server or from nomads
# see hymenu.tcl and get_config.tcl for default server and directory settings
#-----------------------------------------------------------------------------
# Last Revised: 07 Mar 2002
#               13 Aug 2002
#               11 Sep 2002
#               29 Jul 2003
#               16 Apr 2004
#               24 Aug 2004 - eta12 access
#               12 Oct 2004
#               18 Feb 2005 - new name convention, drop avn
#               23 Mar 2005 - added hemispheric GFS
#               05 Jun 2006 - minor modifications
#               08 Mar 2007 - global ftp host / tcl ftp
#               14 Feb 2008 - added namak and full grid nam
#               14 Oct 2008 - switch to new /pub/forecast directory
#               11 Dec 2008 - undefined variable fixed
#               16 Oct 2009 - added half degree
#               07 Mar 2012 - created FTP log file
#               16 Mar 2012 - ruc to rap
#               12 Jun 2015 - http from nomads
#               08 Sep 2016 - button text color changed to white
#               17 Jul 2017 - adjusted pady
#               29 Aug 2018 - major restructure, new data 0.25 and HRRR
#               13 Feb 2019 - corrected http to https
#               21 Aug 2019 - update per NCEP hysplit.v7.6 
#               18 Mar 2021 - expanded the ftp failure message
#               18 Apr 2022 - warn when the user requests a namsf tile from NOMADS.
#                             correct curl option to --insecure.
#               11 Aug 2023 - replaced ftp with wget.exe for Windows PC
#-----------------------------------------------------------------------------

global Type html_dir File Fcst_path Password model base_name exec_dir

if [winfo exists .getarl] {destroy .getarl}
set wr .getarl
toplevel $wr
wm title $wr " FTP Forecast Data from ARL or HTTPS from NOMADS "
wm  geometry $wr +50+25

set Type $arch_fcst
#-->select file (F - forecast or A - appended)
if {$Type == "F"} {
  set nfile 7
  set model(0)  "gfsf" ; set size(0)  "      1-deg  3P +240h ( 814 Mb)"
  set model(1)  "gfslrf" ; set size(1)  "    1-deg  6P +384h ( 251 Mb)"  
  set model(2)  "gfs0p25f" ; set size(2)  " 0.25-d  3H  +21h (2500 Mb)"
  set model(3)  "namf" ; set size(3)  "      12-km  3P  +84h (1616 Mb)" 
  set model(4)  "namsf" ; set size(4)  "     12-km  1H  +48h (2636 Mb)"  
  set model(5)  "namsf" ; set size(5)  "      3-km  1H   +6h (3314 Mb)" 
  set model(6)  "hrrrf" ; set size(6)  "      3-km  1S  +18h (3418 Mb)"
} else {
  set nfile 5
  set model(0)  "gfsa" ; set size(0)    "   1-deg  3P  -48h ( 161 Mb)"
  set model(1)  "nama" ; set size(1)    "   12-km  3P  -48h ( 892 Mb)"   
  set model(2)  "namsa" ; set size(2)    "   12-km  1H  -24h ( 1291 Mb)" 
  set model(3)  "namsa.AK" ; set size(3)    "   12-km  1H  -24h ( 746 Mb)"
  set model(4)  "namsa.HI" ; set size(4)    "   2.5-km 1H  -24h ( 505 Mb)"
}

frame $wr.top
frame $wr.mid0
frame $wr.mid1
frame $wr.mid2
frame $wr.mid3
frame $wr.mid4
frame $wr.mid5
frame $wr.mid6
frame $wr.mid7
frame $wr.mid8
frame $wr.mid9
frame $wr.mida
frame $wr.midb
frame $wr.midc
frame $wr.midd
frame $wr.mide
frame $wr.midf
frame $wr.midg
frame $wr.midh
frame $wr.midi
frame $wr.midj
frame $wr.midx

frame $wr.bot

pack $wr.top  -side top -pady 2 -padx 2
pack $wr.mid0 $wr.mid1 -side top -anchor w -padx 4
pack $wr.mid2 -anchor e -padx 8
pack $wr.mid3 $wr.mid4 $wr.mid5 -side top -anchor w -padx 4
pack $wr.mid6 -anchor e -padx 8
pack $wr.mid7 -side top -anchor w -padx 4
pack $wr.mid8 -anchor e -padx 8
pack $wr.mid9 $wr.mida $wr.midb -side top -anchor w -padx 4
pack $wr.midc -anchor e -padx 8
if {$Type == "F"} {
pack $wr.midd $wr.mide $wr.midf $wr.midx $wr.midh $wr.midi -side top -pady 4
} else {
pack $wr.midd $wr.mide $wr.midf $wr.midg $wr.midi -side top -pady 4
}
pack $wr.midj $wr.bot -side top -pady 4 -padx 2

#-->description

if {$Type == "F"} {
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 5i \
-text "All data files are derived from various forecast\
models. Abbreviations indicate: spatial resolution,\
temporal resolution, vertical coordinate, and duration.\
For some models a forecast is divided into multiple files\
and the forecast start time must be selected (blue). NAM\
options are available for the full-grid CONUS or\
sub-domains (green). Spatial resolution of NAM sub-domains vary.\
HRRR cycles are 1-hourly."
pack $wr.top.lab

} else {
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 5i \
-text "All data files are derived from various forecast\
models. Abbreviations indicate: spatial resolution,\
temporal resolution, vertical coordinate, and duration.\
Appended data are composed of a time sequence of analyses\
from the forecast model starting one or two days ago to\
the present time."
pack $wr.top.lab
}

if {$Type == "F"} {
  label $wr.mid0.lab -text "Global ________________________________>" 
  pack  $wr.mid0.lab
}

#-->select file to download (forecast or archive section)

if {$Type == "F"} {

#-->select file to download (forecast section)
for {set d 0} {$d <= 1} {incr d} {
  radiobutton $wr.mid1.f$d -variable File -text "$model($d) $size($d)" -value $d \
              -command {set base_name "$model($File)"}
  pack $wr.mid1.f$d -side top
  bind $wr.mid1.f$d <Leave> setFName
  }

} else {

#-->select file to download (archive section)
for {set d 0} {$d < $nfile} {incr d} {
  radiobutton $wr.mid1.f$d -variable File -text "$model($d) $size($d)" -value $d \
              -command {set base_name "$model($File)"}
  pack $wr.mid1.f$d -side top
  bind $wr.mid1.f$d <Leave> setFName
  }

}

#-->select file to download (forecast ONLY section)

if {$Type == "F"} {

# gfs0p25fXXX

  set d 2 
  radiobutton $wr.mid1.f$d -variable File -text "$model($d) $size($d)" -value $d \
              -command {set base_name "$model($File)${fcst1}"}
  pack $wr.mid1.f$d -side top
  bind $wr.mid1.f$d <Leave> setFName

  set d 0
  foreach item [list 000 024 048 072 096 120 144 168] {
     radiobutton $wr.mid2.$d -variable fcst1 -text $item -value $item -fg blue \
                 -command {set base_name "$model($File)${fcst1}"}
     pack $wr.mid2.$d -side left
     bind $wr.mid2.$d <Leave> setFName
     incr d
     }

# namf 

  label $wr.mid3.lab -text "North America _________________________>" 
  pack  $wr.mid3.lab

  set d 3
  radiobutton $wr.mid4.f$d -variable File -text "$model($d) $size($d)" -value $d \
              -command {set base_name "$model($File)"}
  pack $wr.mid4.f$d -side top
  bind $wr.mid4.f$d <Leave> setFName

# namsf.{optional} 

  set d 4
  radiobutton $wr.mid5.f$d -variable File -text "$model($d) $size($d)" -value $d \
              -command {set base_name "$model($File)"; set suffix " "}
  pack $wr.mid5.f$d -side top
  bind $wr.mid5.f$d <Leave> setFName

  set d 0
  foreach item [list AK HI FW NEtile NWtile SEtile SWtile] {
     radiobutton $wr.mid6.$d -variable suffix -text $item -value $item -fg green \
                 -command {set base_name "$model($File).${suffix}"}
     pack $wr.mid6.$d -side left
     bind $wr.mid6.$d <Leave> setFName
     incr d
     }

# namsfXX.CONUS

  set d 5
  radiobutton $wr.mid7.f$d -variable File -text "$model($d) $size($d)" -value $d \
              -command {set base_name "$model($File)${fcst2}.CONUS"}
  pack $wr.mid7.f$d -side top
  bind $wr.mid7.f$d <Leave> setFName

  set d 0
  foreach item [list 00 06 12 18 24 30 36 42] {
     radiobutton $wr.mid8.$d -variable fcst2 -text $item -value $item -fg blue \
                 -command {set base_name "$model($File)${fcst2}.CONUS"}
     pack $wr.mid8.$d -side left
     bind $wr.mid8.$d <Leave> setFName
     incr d
     }

# hrrrfxx

  set d 6
  radiobutton $wr.midb.f$d -variable File -text "$model($d) $size($d)" -value $d \
              -command {set base_name "hrrrf${fcst3}"}
  pack $wr.midb.f$d -side top
  bind $wr.midb.f$d <Leave> setFName

  set d 0
  foreach item [list 00 06 12 18] {
     radiobutton $wr.midc.$d -variable fcst3 -text $item -value $item -fg blue \
                 -command {set base_name "hrrrf${fcst3}"}
     pack $wr.midc.$d -side left
     bind $wr.midc.$d <Leave> setFName
     incr d
     }
}
label $wr.midd.lab -text "__________________________________________________________" 
pack  $wr.midd.lab

#-->anonymous ftp password
label $wr.mide.lab -text "FTP Password: "
entry $wr.mide.ent -textvariable Password -relief sunken -width 30
pack  $wr.mide.lab $wr.mide.ent -side left

#-->starting date
label $wr.midf.l1 -text " Year (YYYY):" 
entry $wr.midf.e1 -textvariable Year -width 4
pack $wr.midf.l1 $wr.midf.e1 -side left -padx 2
label $wr.midf.l2 -text " Month (MM):" 
entry $wr.midf.e2 -textvariable Month -width 2
pack $wr.midf.l2 $wr.midf.e2 -side left -padx 2
label $wr.midf.l3 -text " Day (DD):" 
entry $wr.midf.e3 -textvariable Day -width 2
pack $wr.midf.l3 $wr.midf.e3 -side left -padx 2
label $wr.midf.l4 -text " Cycle (CC):" 
entry $wr.midf.e4 -textvariable Cycle -width 2
pack $wr.midf.l4 $wr.midf.e4 -side left -padx 2
bind $wr.midf <Leave> setFName
 
if {$Type == "F"} {

#-->forecast cycle
label $wr.midx.lab -text "Forecast Cycle (UTC) (choose below, or for HRRR enter above):"
pack $wr.midx.lab -side left -padx 2

pack $wr.midh

radiobutton $wr.midh.c0 -variable Cycle -text "00" -value "00"
radiobutton $wr.midh.c1 -variable Cycle -text "06" -value "06" 
radiobutton $wr.midh.c2 -variable Cycle -text "12" -value "12" 
radiobutton $wr.midh.c3 -variable Cycle -text "18" -value "18" 
pack $wr.midh.c0 $wr.midh.c1 $wr.midh.c2 $wr.midh.c3 -side left

bind $wr.midh <Leave> setFName

} else {

#-->forecast cycle
label $wr.midg.lab -text "Forecast Cycle (UTC):" 
radiobutton $wr.midg.c1 -variable Cycle -text "00" -value "00"
radiobutton $wr.midg.c2 -variable Cycle -text "06" -value "06"
radiobutton $wr.midg.c3 -variable Cycle -text "12" -value "12"
radiobutton $wr.midg.c4 -variable Cycle -text "18" -value "18"
pack $wr.midg.lab $wr.midg.c1 $wr.midg.c2  $wr.midg.c3 $wr.midg.c4 -side left
bind $wr.midg <Leave> setFName
 
}

#-->set output file path
label $wr.midi.lab -text "Output path: "
entry $wr.midi.ent -textvariable Fcst_path -relief sunken -width 30
pack $wr.midi.lab $wr.midi.ent -side left

#-->show file name
label $wr.midj.txt -textvar data_file -width 45 -relief sunken
pack  $wr.midj.txt -padx 10 -side left

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help  -width 8 -command \
       {load_html [file join $html_dir S111.htm ]}
button $wr.bot.save -bg cyan  -text "FTP Data ARL" -width 20 -command {ftp_adata}
button $wr.bot.exe  -bg green -text "HTTPS Data NOMADS" -width 20 -command {http_adata}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save $wr.bot.exe -side left -padx 10
arl_set
}

#----------------------------------------------------------------------------
#-->set defaults

proc arl_set {} {
global Year Month Day Cycle Type data_file suffix fcst1 fcst2 fcst3
global env File Fcst_path Password Work_path model base_name nfile

set File 0
set base_name $model($File)

set date [clock format [clock scan now] -format "%Y %m %d %H" -gmt 1]
set Year [lindex [split $date] 0]
set Month [lindex [split $date] 1]
set Day [lindex [split $date] 2]
set Hrs [lindex [split $date] 3]

if {$Cycle == ""} {
   set Cycle "00"
   if {$Hrs>=10 && $Hrs<16} {set Cycle "06"}
   if {$Hrs>=16 && $Hrs<22} {set Cycle "12"}
   if {$Hrs>=22 || $Hrs<4}  {set Cycle "18"}
}

if {$Fcst_path == ""} {set Fcst_path "$Work_path"}

if {$Password == ""} {
   if {[array names env USERNAME] == "USERNAME"} {set Password $env(USERNAME)@hysplit}
   if {[array names env USER] == "USER"} {set Password $env(USER)@hysplit}
   if {[array names env SYSTEM] == "SYSTEM"} {append Password .$env(SYSTEM)}
   if {[array names env HOSTNAME] == "HOSTNAME"} {append Password .$env(HOSTNAME)}
}
if {$Password == ""} {set Password "username@hostname.org"}

set fcst1 "000"
set fcst2 "00"
set fcst3 "00"
set suffix " "
set data_file hysplit.t${Cycle}z.${base_name}
}

#-------------------------------------------------------------------------
#-->get arl data files

proc ftp_adata { } {

global Year Month Day Cycle 
global Type tcl_dir File Fcst_path Password
global ftpfor dirfor ftp_size base_name data_file
global tcl_platform exec_dir

if {$Password == "username@hostname.org"} {
   msg_box "FTP disabled: requires Email address for password"

} else {
   set data_dir  ${Year}${Month}${Day}
   if {"${dirfor(0)}" == "/pub/data/nccf/com/hysplit/prod"} {
      set data_dir hysplit.${Year}${Month}${Day}}

   cd $Fcst_path
   if [file exists $data_file] {file delete $data_file}
   set f [open "ftp_log.txt" w]

   if [catch {package require ftp} rcode] {
      if [info exists input] {unset input}

      msg_box " FTP transfer in progress ... please wait "
      update

      if { "$tcl_platform(platform)" == "unix" } {
         append input "user anonymous $Password\n"
         append input "lcd $Fcst_path\n"
         append input "cd ${dirfor(0)}/$data_dir\n"
         append input "binary\n"
         append input "prompt\n"
         append input "get $data_file\n" 
         append input "bye\n"
         puts $f $input

         exec ftp -n $ftpfor(0) << $input
      } else {
         set input "$exec_dir/wget.exe --prefer-family=IPv4 --unlink --ftp-user=anonymous --ftp-password=\"${Password}\" -O \"$Fcst_path/$data_file\" -o \"$Fcst_path/wget.log\" ftp://${ftpfor(0)}${dirfor(0)}/$data_dir/$data_file"
         puts $f $input
         puts $f "\n"

         exec $exec_dir/wget.exe \
               --prefer-family=IPv4 --unlink \
               --ftp-user=anonymous --ftp-password="${Password}" \
               -O "$Fcst_path/$data_file" \
               -o "$Fcst_path/wget.log" \
               ftp://${ftpfor(0)}${dirfor(0)}/$data_dir/$data_file
      }
   } else {
      set handle   [::ftp::Open     $ftpfor(0) anonymous $Password \
                   -mode passive -blocksize 4194304 -progress ftp_box 0]
#                  -blocksize 4194304 -progress ftp_box 0]
#                  -blocksize 1048576 -progress ftp_box 0]
          puts $f "Server connection: $handle"
      set ::ftp::VERBOSE 1
      set rcode    [::ftp::Cd       $handle ${dirfor(0)}/$data_dir]
          puts $f "Changed directory: $rcode"
      set rcode    [::ftp::Type     $handle binary]
          puts $f "Switch to  binary: $rcode"
      set ftp_size [::ftp::FileSize $handle $data_file]
          puts $f "Get filesize byte: $ftp_size"
      set rcode    [::ftp::Get      $handle $data_file]
          puts $f "Complete transfer: $rcode"
      set rcode    [::ftp::Close    $handle]
          puts $f "Close FTP command: $rcode"
      if [winfo exists .ftp_win] {destroy .ftp_win}
   }

   if [file exists $data_file] {
      msg_box " FTP complete: $ftpfor(0) -> $dirfor(0)/$data_dir/$data_file "
      puts $f  "FTP complete: $ftpfor(0) -> $dirfor(0)/$data_dir/$data_file "
   } else {
      msg_box "
 FTP failed: $ftpfor(0) -> $dirfor(0)/$data_dir/$data_file 
 
 The requested file does not exist or this FTP user interface might not have worked
 due to your computer configuration. Consider manually downloading the data file
 to your work directory $Fcst_path. Windows PC users might want to
 use a third-party ftp client that supports the passive mode. Run a ftp client and
 connect to $ftpfor(0). For username and password, use anonymous and your email
 address, respectively. Then change directory to $dirfor(0)/$data_dir and
 download $data_file."
      puts $f  "FTP failed: $ftpfor(0) -> $dirfor(0)/$data_dir/$data_file "
   }
   close $f
}

destroy .getarl
}

#----------------------------------------------------------------------------------
#-->get arl data files from nomads

proc http_adata { } {

global Year Month Day Cycle exec_dir tcl_platform X_dir
global Type tcl_dir File Fcst_path Password
global ftpfor dirfor ftp_size base_name data_file

# bail out if a namsf tile is requested
   if { "$base_name" == "namsf.NEtile" || "$base_name" == "namsf.NWtile" \
        || "$base_name" == "namsf.SEtile" || "$base_name" == "namsf.SWtile" } {
      msg_box "namsf tiles are unavailable from NOMADS. Push the 'FTP Data ARL' button instead"
      return
   }
   
# hardwired nomads server   
   set data_dir https://nomads.ncep.noaa.gov/pub/data/nccf/com/hysplit/prod/hysplit.${Year}${Month}${Day}

   cd $Fcst_path
   if [file exists $data_file] {file delete -force $data_file}
   if [ file exists STDOUT ]   {file delete -force STDOUT}   
   if [ file exists http.log ] {file delete -force http.log}   

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
      exec curl -s $data_dir/$data_file -o$data_file --insecure
   } else {
      exec $X_dir/xterm -fn fixed -e curl -s $data_dir/$data_file -o$data_file --insecure
   }

} else {
   if [catch {exec $exec_dir/wget.exe -t 45 $data_dir/$data_file -oSTDOUT --no-check-certificate} result] {
      msg_box "ERROR: file not available "}
}

if [file exists $data_file ] {
    set file_size [file size $data_file]
    if {$file_size >= 400 } {
        msg_box " HTTPS complete: $data_dir/$data_file "
    } else {
        msg_box " HTTPS failed: $data_dir/$data_file"
        file copy $data_file STDOUT
    }
}

if [file exists STDOUT] {
    set log [ScrollText .f]
    set fid [open "STDOUT" r]
    while {[eof $fid] != 1} {
      gets $fid cline
      $log insert end $cline\n
    }
    close $fid
    file delete -force STDOUT
}
#destroy .getarl
}


#-------------------------------------------------------
#-->set file name

proc setFName {} {
global data_file Cycle base_name

set data_file hysplit.t${Cycle}z.${base_name}
}

