proc get_arc {} {

#-----------------------------------------------------------------------------
# GET_ARC.TCL:  ftp meteorological data archives from the ARL server 
# Last Revised: 27 Apr 2004
#               19 Oct 2005 - added GDAS one degree
#               05 Jun 2005 - text changes
#               08 Mar 2007 - global ftp server name / tcl ftp
#               14 Feb 2008 - added 0.5 deg and sigma nam
#               14 Oct 2008 - FTP server update
#               13 Aug 2009 - automatically generate week from day for GDAS
#               16 Oct 2009 - added Alaska and Hawaii
#               20 Dec 2011 - remove FNL
#               07 Mar 2012 - created FTP log file
#               18 Apr 2013 - data directory change for nams only
#               11 Mar 2015 - added narr ftp; data directory changed to narr
#               08 Sep 2016 - button text color changed to white
#               27 Aug 2018 - additional data sets, cleaner interface
#               09 Aug 2019 - rename hrrr.v1 gfs0p25.v1, new hrrr and gfs0p25
#               12 May 2020 - add vertical scrollbar.
#               19 Nov 2020 - option for instaneous or averaged WRF
#               18 Mar 2021 - expanded the ftp failure message
#               11 Aug 2023 - replaced ftp with wget.exe for Windows PC
#-----------------------------------------------------------------------------

global html_dir data_file File Year Month Half Week Day Cyc Fcst_path Password
if [winfo exists .getarc] {destroy .getarc}
set topwin .getarc
toplevel $topwin
wm title $topwin " FTP Meteorological Archive Data from ARL  "
wm  geometry $topwin +50+25

# create a scrollable frame (wr).
frame $topwin.outerframe
canvas $topwin.outerframe.canvas -yscrollcommand "$topwin.outerframe.yscroll set"
scrollbar $topwin.outerframe.yscroll -command "$topwin.outerframe.canvas yview"
frame $topwin.outerframe.canvas.innerframe
set wr $topwin.outerframe.canvas.innerframe

frame $wr.top
frame $wr.mid4
frame $wr.mid5
frame $wr.mid0
frame $wr.mid1
frame $wr.mid2
frame $wr.mid3
frame $wr.mid6
frame $wr.bot
pack $wr.top -side top -pady 4 -padx 4
pack $wr.mid4          -side top -padx 4
pack $wr.mid0 $wr.mid1 $wr.mid2 $wr.mid5 $wr.mid3 $wr.mid6 $wr.bot -side top -pady 4 -padx 4

#-->description
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 4i \
-text "FTP Access to HYSPLIT meteorological archives. The data are \
available either by cycle (6H), daily (DA), weekly (WK), semi-monthly (SM), \
or monthly (M) on pressure (P), sigma (S), or hybrid (H) coordinates. \
The file name is constructed from the year, month, day, hour entries."
pack $wr.top.lab

#-->select base meteo file
set model(0)  "EDAS     40 km  3P (>=2004 SM  650 Mb)"  
set model(1)  "GDAS     1-deg  3P (>=2005 WK  600 Mb)"
set model(2)  "GDAS   0.5-deg  3H (>=2007 DA  500 Mb)"
set model(3)  "GFS.v1 0.25-deg 3H (2016-2019 DA 2500 Mb)"
set model(4)  "GFS    0.25-deg 3H (>=2019 DA 2500 Mb)"
set model(5)  "HRRR.v1   3 km  1S (2015-2019 6H 3500 Mb)"  
set model(6)  "HRRR      3 km  1S (>=2019 6H 3500 Mb)"
set model(7)  "NAM12    12-km  3P (>=2007 DA  450 Mb)" 
set model(8)  "NAMS     12 km  1H (>=2010 DA 1300 Mb)" 
set model(9)  "NAMS_AK  12 km  1H (>=2010 DA  750 Mb)"
set model(10) "NAMS_HI   2 km  1H (>=2010 DA  500 Mb)"
set model(11) "NARR     32 km  3P (>=1979 MO 3000 Mb)"
set model(12) "NCEP   2.5-deg  6P (>=1948 MO  125 Mb)"
set model(13) "WRF      27 km  1S (>=1980 DA  220 Mb)"
set model(14) "WRF_avg  27 km  1S (>=1980 DA  220 Mb)"


for {set d 0} {$d <= 14} {incr d} {
  radiobutton $wr.mid4.$d -variable File -text "$model($d)" -value $d  
  pack $wr.mid4.$d -side top
  bind $wr.mid4.$d <Leave> updateFName
  }

#-->cycle
label $wr.mid5.lab -text "     HRRR Hour:" 
radiobutton $wr.mid5.c1 -variable Cyc -text "00" -value "00"
radiobutton $wr.mid5.c2 -variable Cyc -text "06" -value "06"
radiobutton $wr.mid5.c3 -variable Cyc -text "12" -value "12"
radiobutton $wr.mid5.c4 -variable Cyc -text "18" -value "18"
pack $wr.mid5.lab $wr.mid5.c1 $wr.mid5.c2  $wr.mid5.c3 $wr.mid5.c4 -side left
bind $wr.mid5 <Leave> updateFName

#-->anonymous ftp password
label $wr.mid0.lab -text "FTP Password: "
entry $wr.mid0.ent -textvar Password -relief sunken -width 25
pack $wr.mid0.lab $wr.mid0.ent -side left

#-->select month
frame $wr.mid1.lab
frame $wr.mid1.top
frame $wr.mid1.bot
pack $wr.mid1.lab $wr.mid1.top $wr.mid1.bot -side top
label $wr.mid1.lab.txt -text "Select Month:"
pack $wr.mid1.lab.txt
 
set d 0
foreach item [list jan feb mar apr may jun] {
   radiobutton $wr.mid1.top.$d -variable Month -text $item -value $item
   pack $wr.mid1.top.$d -side left
   bind $wr.mid1.top.$d <Leave> updateFName
   incr d
   }

set d 0
foreach item [list jul aug sep oct nov dec] {
   radiobutton $wr.mid1.bot.$d -variable Month -text $item -value $item
   pack $wr.mid1.bot.$d -side left
   bind $wr.mid1.bot.$d <Leave> updateFName
   incr d
   }

#-->starting date
label $wr.mid2.lab1 -text "Year (YY):" 
entry $wr.mid2.ent1 -textvar Year -width 3
pack $wr.mid2.lab1 $wr.mid2.ent1 -side left -padx 15
bind $wr.mid2.ent1 <KeyRelease> updateFName

label $wr.mid2.lab2 -text "Day (DD):" 
entry $wr.mid2.ent2 -textvar Day -width 3
pack $wr.mid2.lab2 $wr.mid2.ent2 -side left -padx 15
bind $wr.mid2.ent2 <KeyRelease> updateFName
 
#-->set output file path
label $wr.mid3.lab -text "Output path: "
entry $wr.mid3.ent -textvar Fcst_path -relief sunken -width 25
pack $wr.mid3.lab $wr.mid3.ent -padx 8 -side left

#-->show file name
label $wr.mid6.txt -textvar data_file -width 40 -relief sunken
pack  $wr.mid6.txt -padx 10 -side left

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $topwin"
button $wr.bot.help -text Help  -width 8 \
       -command "load_html [file join $html_dir S112.htm ] "
button $wr.bot.save  -bg cyan -text "FTP Data File" -width 20 -command {ftp_cdata}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 

# scrollable frame business
pack $topwin.outerframe.canvas.innerframe -expand yes -fill both -side top
pack $topwin.outerframe.yscroll -side right -fill y
$topwin.outerframe.canvas create window 0 0 -anchor nw -window $topwin.outerframe.canvas.innerframe
$topwin.outerframe.canvas configure -scrollregion [$topwin.outerframe.canvas bbox all]
pack $topwin.outerframe.canvas -expand yes -fill both -side top
pack $topwin.outerframe -expand yes -fill both -side top
bind $topwin.outerframe <Map> {
   set topwin .getarc
   # assume only 80% of the screen height is available.
   set content_height [winfo height $topwin.outerframe.canvas.innerframe]
   set avail_scr_height [expr 0.80 * [winfo screenheight .]]
   if {$content_height >= $avail_scr_height} {
      set view_height $avail_scr_height
   } else {
      set view_height $content_height
   }
   $topwin.outerframe.canvas.innerframe configure -height $content_height
   $topwin.outerframe.canvas configure -width 480 -height $view_height
   $topwin.outerframe.canvas configure -scrollregion [$topwin.outerframe.canvas bbox all]
}

pub_set
}

#-->set directory defaults
proc pub_set {} {
global env data_file File Year Month Half Week Day Cyc Fcst_path Password Work_path

set date [clock format [clock scan now] -format "%y %b %d"]
set Year [lindex [split $date] 0]
set Month [string tolower [lindex [split $date] 1]]
set Day [lindex [split $date] 2]

set File 12
set Cyc "00"

set DD 0
foreach item [list 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 \
                   16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31] {
   incr DD
   if {"$Day" == "$item"} {
      set Week 5
      if { $DD < 29 } {set Week 4}
      if { $DD < 22 } {set Week 3}
      if { $DD < 15 } {set Week 2}
      if { $DD <  8 } {set Week 1}
      set Half "001"
      if { $DD > 15 } {set Half "002"}
   } 
}

if {$Fcst_path == ""} {set Fcst_path "$Work_path"}

if {$Password == ""} {
   if {[array names env USERNAME] == "USERNAME"} {set Password $env(USERNAME)@hysplit}
   if {[array names env USER] == "USER"} {set Password $env(USER)@hysplit}
   if {[array names env SYSTEM] == "SYSTEM"} {append Password .$env(SYSTEM)}
   if {[array names env HOSTNAME] == "HOSTNAME"} {append Password .$env(HOSTNAME)}
}
if {$Password == ""} {set Password "username@hostname.org"}

updateFName
}

#-------------------------------------------------------
#-->set file name

proc updateFName {} {
global data_file File Base Year Month Half Week Day Cyc data_dir

set data_dir [lindex {edas40  gdas1 gdas0p5    gfs0p25.v1 gfs0p25 \
                      hrrr.v1 hrrr  nam12      nams       nams \
                      nams    narr  reanalysis wrf27km    wrf27km} $File]

set Base [lindex {edas                  gdas1   gdas0p5 gfs0p25            gfs0p25 \
                  hysplit               hrrr    nam12   hysplit.t00z.namsa hysplit.t00z.namsa.AK \
                  hysplit.t00z.namsa.HI NARR    RP      wrfout_d01 wrfout_d01} $File]

set DD 0
foreach item [list 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 \
                   16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31] {
   incr DD
   if {"$Day" == "$item"} {
      set Week 5
      if { $DD < 29 } {set Week 4}
      if { $DD < 22 } {set Week 3}
      if { $DD < 15 } {set Week 2}
      if { $DD <  8 } {set Week 1}
      set Half "001"
      if { $DD > 15 } {set Half "002"}
   } 
}

set d 1
foreach item [list jan feb mar apr may jun jul aug sep oct nov dec] {
   if {"$Month" == "$item"} {set Mon $d} 
   incr d}
if {$Mon < 10}   {set Mon 0$Mon}
if {$Year < 100} {
   set Ye 20$Year
   if {$Year >= 48} {set Ye 19$Year}
} else {
   set Ye $Year
}

# EDAS (semi-monthly)
  if {$File == 0} {set data_file "${Base}.${Month}${Year}.${Half}"}
# GDAS (weekly)
  if {$File == 1} {set data_file "${Base}.${Month}${Year}.w${Week}"}
# GDAS1/GDAS0P5/GFS0P25.v1/GFS0P25 (YYYYMMDD_)
  if {$File > 1 && $File <= 4} {set data_file "20${Year}${Mon}${Day}_${Base}"}
# HRRR.v1 hysplit.yyyymmdd.xxz.hrrra
  if {$File == 5} {set data_file "${Base}.${Ye}${Mon}${Day}.${Cyc}z.hrrra"}
# HRRR YYYYMMDD_[00-05|06-11|12-17|18-23]_hrrr
  if {$File == 6} {
       if {$Cyc == 00} {
          set end 05
       } elseif {$Cyc == 06} {
          set end 11
       } elseif {$Cyc == 12} {
          set end 17
       } else {
          set end 23
       }
       set data_file "20${Year}${Mon}${Day}_${Cyc}-${end}_${Base}"
  }
# NAM12/NAMS/AK/HI (YYYYMMDD_)
  if {$File >= 7 && $File <= 10} {set data_file "20${Year}${Mon}${Day}_${Base}"}
      

# NARR (monthly)
  if {$File == 11} {set data_file "${Base}${Ye}${Mon}"}
# NCEP (monthly)
  if {$File == 12} {set data_file "${Base}${Ye}${Mon}.gbl"}
# wrf27km  (YYYY/wrfout_d01_yyyymmdd.ARL)
  if {$File == 13} {
     set data_dir  ${data_dir}/inst/${Ye}
     set data_file "${Base}_${Ye}${Mon}${Day}.ARL"}
# wrf27km  (YYYY/wrfout_d01_yyyymmdd.ARL)
  if {$File == 14} {
     set data_dir  ${data_dir}/avg/${Ye}
     set data_file "${Base}_${Ye}${Mon}${Day}.ARL"}


}

#------------------------------------------------------
#-->get arl data files

proc ftp_cdata {} {
global Year data_file data_dir tcl_dir Fcst_path Password
global ftparc dirarc ftp_size tcl_platform exec_dir

if {"${dirarc(0)}" == "/pub/data/nccf/com/hysplit/prod"} {
   msg_box "No archive data on this server: ${ftparc(0)}"
   return
}

if {$Password == "username@hostname.org"} {
   msg_box "FTP disabled: requires Email address for password"

} else {
   cd $Fcst_path
   if [file exists $data_file] {file delete $data_file}
   set f [open "ftp_log.txt" w]

   if {"$data_dir" == "nams"} {
      set temp_dir "$data_dir"
   } else {
   if {"$data_dir" == "narr"} {
      set temp_dir "$data_dir"
   } else {
      set temp_dir "${dirarc(0)}/$data_dir"
   } }

   if [catch {package require ftp} rcode] {
      if [info exists input] {unset input}

      msg_box " FTP transfer in progress ... please wait "
      update

      if { "$tcl_platform(platform)" == "unix" } {
         append input "user anonymous $Password\n"
         append input "lcd $Fcst_path\n"
         append input "cd $temp_dir\n"
         append input "binary\n"
         append input "prompt\n"
         append input "get $data_file\n" 
         append input "bye\n"
         puts $f $input

         exec ftp -n $ftparc(0) << $input
      } else {
         set input "$exec_dir/wget.exe --prefer-family=IPv4 --unlink --ftp-user=anonymous --ftp-password=\"${Password}\" -O \"$Fcst_path/$data_file\" -o \"$Fcst_path/wget.log\" ftp://${ftparc(0)}$temp_dir/$data_file"
         puts $f $input
         puts $f "\n"

         exec $exec_dir/wget.exe \
               --prefer-family=IPv4 --unlink \
               --ftp-user=anonymous --ftp-password="${Password}" \
               -O "$Fcst_path/$data_file" \
               -o "$Fcst_path/wget.log" \
               ftp://${ftparc(0)}$temp_dir/$data_file
      }

   } else {
      set handle [::ftp::Open $ftparc(0) anonymous $Password \
                  -mode passive -blocksize 4194304 -progress ftp_box 0]
#                 -blocksize 4194304 -progress ftp_box 0]
#                 -blocksize 1048576 -progress ftp_box 0]
          puts $f "Server connection: $handle"
      set rcode  [::ftp::Cd       $handle $temp_dir]
          puts $f "Changed directory: $rcode"
      set rcode  [::ftp::Type     $handle binary]
          puts $f "Switch to  binary: $rcode"
      set ftp_size [::ftp::FileSize $handle $data_file]
          puts $f "Get filesize byte: $ftp_size"
      set rcode  [::ftp::Get      $handle $data_file]
          puts $f "Complete transfer: $rcode"
      set rcode  [::ftp::Close    $handle]
          puts $f "Close FTP command: $rcode"
      if [winfo exists .ftp_win] {destroy .ftp_win}
    }

   if [file exists $data_file] {
      msg_box " FTP complete: $ftparc(0) -> $temp_dir/$data_file "
      puts $f  "FTP complete: $ftparc(0) -> $temp_dir/$data_file "
   } else { 
      msg_box "
 FTP failed: $ftparc(0) -> $temp_dir/$data_file
 
 The requested file does not exist or this FTP user interface might not have worked
 due to your computer configuration. Consider manually downloading the data file
 to your work directory $Fcst_path. Windows PC users might want to
 use a third-party ftp client that supports the passive mode. Run a ftp client and
 connect to $ftparc(0). For username and password, use anonymous and your email
 address, respectively. Then change directory to $temp_dir and
 download $data_file."
      puts $f  "FTP failed: $ftparc(0) -> $temp_dir/$data_file "
   }
   close $f

}
destroy .getarc
}

