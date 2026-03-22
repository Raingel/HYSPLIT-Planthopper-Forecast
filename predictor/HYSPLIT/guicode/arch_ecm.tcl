proc ecm_arch {} {

#-----------------------------------------------------------------------------
# ARCH_ECM.TCL: Convert grib global lat-lon archive data files to ARL format
# Last Revised: 09 Apr 2002
#               13 Aug 2002
#               20 Apr 2004
#               10 Jan 2005 - simplify xterm shell
#               08 Feb 2005 - zero hour initialization
#               21 Aug 2009 - trap standard output
#               30 Jun 2010 - windows7 compatibility issue with cmd.exe              
#               04 Oct 2010 - disable unused command line options
#-----------------------------------------------------------------------------

global sfcp levels html_dir Grid_number day_start day_end
global Year Month Day Cycle Fdur Lat Lon Fcst_path Fcst_file Method 
if [winfo exists .ecmarch] {destroy .ecmarch}
set wr .ecmarch 
toplevel $wr
wm title $wr " Converts ECMWF or NOAA archived GRIB-1 data to ARL format "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.data
frame $wr.mid1
frame $wr.mid4
frame $wr.mid5
frame $wr.mid6
frame $wr.bot
pack $wr.top -pady 4 -padx 2 
pack $wr.data -pady 2
pack $wr.mid1 $wr.mid4 $wr.mid5 -side top -pady 6 -anchor w
pack $wr.mid6 $wr.bot -side top -pady 10 -padx 10

#-->information

label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "Convert existing global lat-lon grid GRIB-1 data files to ARL format\
by selecting any file in the grib directory. Subsequent file names will be\
constructed by the script for each day and hour assuming those fields are\
the last four digits of the file name: base{DD}{HH}."
pack $wr.top.lab

#-->meteorology input

frame $wr.data.lab
frame $wr.data.pick
pack $wr.data.lab $wr.data.pick -pady 2 -side top
button $wr.data.lab.set  -text "Set File Name of GRIB Input Data" -width 55 -command {
   .ecmarch.data.pick.src.list delete 0 end
   .ecmarch.data.pick.dir.list delete 0 end
   set Grid_number 0
   do_file_list .ecmarch.data.pick.src.list .ecmarch.data.pick.dir.list 
   }
pack  $wr.data.lab.set -side left -padx 15

frame $wr.data.pick.dir
frame $wr.data.pick.src
pack $wr.data.pick.dir $wr.data.pick.src -side left -padx 5
listbox $wr.data.pick.dir.list -relief sunken -width 35 -height 1 -setgrid yes -bg LightCyan3
pack $wr.data.pick.dir.list -fill both -expand yes 
listbox $wr.data.pick.src.list -relief sunken -width 20 -height 1 -setgrid yes -bg LightCyan3
pack $wr.data.pick.src.list -fill both -expand yes

#-->select duration start and ending day (process max one month per execution)

label $wr.mid1.lab1 -text "  Start Day:"
entry $wr.mid1.ent1 -textvariable day_start -relief sunken -width 3
pack $wr.mid1.lab1 $wr.mid1.ent1 -side left

label $wr.mid1.lab2 -text "  End Day:"
entry $wr.mid1.ent2 -textvariable day_end -relief sunken -width 3
pack $wr.mid1.lab2 $wr.mid1.ent2 -side left
 
#-->temporal cycle

label $wr.mid1.lab3 -text "  Interval (hr):" 
pack $wr.mid1.lab3 -side left
set d 0
foreach item [list 1 3 6] {
   radiobutton $wr.mid1.but$d -variable Cycle -text $item -value $item
   pack $wr.mid1.but$d -side left
   incr d
   }

#-->output file name base

label $wr.mid4.lab1 -text "  Contains Sfc Pres: "
radiobutton $wr.mid4.but1 -variable sfcp -text Yes -value 1
radiobutton $wr.mid4.but2 -variable sfcp -text No  -value 0
pack $wr.mid4.lab1 $wr.mid4.but1 $wr.mid4.but2 -side left

label $wr.mid4.lab2 -text "           Number of Levels: "
entry $wr.mid4.ent2 -textvariable levels -relief sunken -width 4
pack $wr.mid4.lab2 $wr.mid4.ent2 -side left

#-->file content

label $wr.mid5.lab1 -text "  Output path: "
entry $wr.mid5.ent1 -textvariable Fcst_path -relief sunken -width 25
pack $wr.mid5.lab1 $wr.mid5.ent1 -side left

label $wr.mid5.lab2 -text "   Output File: "
entry $wr.mid5.ent2 -textvariable Fcst_file -relief sunken -width 10
pack $wr.mid5.lab2 $wr.mid5.ent2 -side left

#-->extract options 

frame $wr.mid6.lft
frame $wr.mid6.rgt
pack $wr.mid6.lft $wr.mid6.rgt -side left -padx 5 

label $wr.mid6.lft.lab0 -text "Select projection conversion method"
radiobutton $wr.mid6.lft.tab1 -variable Method -text "Extract (set slider bars)" -value "0"   
radiobutton $wr.mid6.lft.tab2 -variable Method -text "N-Hemisphere Polar Stereo" -value "1"   
radiobutton $wr.mid6.lft.tab3 -variable Method -text "S-Hemisphere Polar Stereo" -value "2"   
radiobutton $wr.mid6.lft.tab4 -variable Method -text "None - Input Lat-Lon Grid" -value "3" 
pack $wr.mid6.lft.lab0 -side top -anchor w
pack $wr.mid6.lft.tab1 $wr.mid6.lft.tab2 $wr.mid6.lft.tab3 \
     $wr.mid6.lft.tab4 -side top -anchor w

#-->center of extracted subgrid

label $wr.mid6.lft.lab3 -text "                       Longitude (W -> E)"
scale $wr.mid6.lft.lon -orient horizontal -length 360 -from -180 -to 180 \
  -tickinterval 45 -variable Lon -resolution 5 
pack $wr.mid6.lft.lab3 $wr.mid6.lft.lon -side top -anchor w

label $wr.mid6.rgt.lab -text "Latitude (S -> N)"
scale $wr.mid6.rgt.lat -orient vertical -length 180 -from 90 -to -90 \
  -tickinterval 30 -variable Lat -resolution 5
pack $wr.mid6.rgt.lat $wr.mid6.rgt.lab -side top  

#-->bottom action buttons

button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help  -width 8 \
       -command "load_html [file join $html_dir S125.htm ] "

button $wr.bot.save  -bg green -text "Process Data" -width 20 -command {
       set gdir  [.ecmarch.data.pick.dir.list get 0 0]
       set gbase [.ecmarch.data.pick.src.list get 0 0]
       xtrct_data $gdir $gbase
       }
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 
set_date
}

#-->set current date

proc set_date {} {
global sfcp levels Grids_path Grid_number Work_path day_start day_end
global Year Month Day Cycle Fdur Lat Lon Fcst_path Fcst_file Method 
set Grid_number 0
set date [clock format [clock scan now] -format "%y %m %d %H"]
set Year [lindex [split $date] 0]
set Month [lindex [split $date] 1]
if {$Fcst_path == ""} {set Fcst_path "$Work_path"}
set Grids_path $Work_path
set Fcst_file "ECM$Year$Month"
set Cycle 6
set day_start 1
set day_end 31
set Method 3
set Lat 60.0
set Lon -80.0
set sfcp 0
set levels 25
if [file exists DATA.ARL] {file delete -force DATA.ARL}
}

#-->process grib data files and append to output file

proc xtrct_data {gdir gbase} {

global sfcp levels day_start day_end X_dir exec_dir tcl_platform
global Year Month Day Cycle Fdur Lat Lon Fcst_path Fcst_file Method 

set log [ScrollText .msg]
$log configure -cursor watch
$log insert end "Processing started ...\n"
update

#-->individual forecasts are appended to this file: delete at start

if [ file exists CFG_GRIB ] {file delete CFG_GRIB}

if [file exists $Fcst_path/$Fcst_file] {file delete \
   -force $Fcst_path/$Fcst_file}

for {set day $day_start} {$day <= $day_end} {incr day} {

    if {$day < 10} {set DA 0$day} else {set DA $day}

for {set hour 0} {$hour <= 23} {incr hour $Cycle} {

    if {$hour < 10} {set HR 0$hour} else {set HR $hour}

#   construct file name according to convention
    set data_file [string range "$gdir/$gbase" 0 {end-4}]
    append data_file $DA
    append data_file $HR

#   decoder command line arguments
    set arg1 -i  ; append arg1 $data_file
    set arg2 -y  ; append arg2 $Lat 
    set arg3 -x  ; append arg3 $Lon 
    set arg4 -g  ; append arg4 $Method 
    set arg5 -p  ; append arg5 $sfcp     
    set arg6 -k  ; append arg6 $levels

    set arg7 -q0
#   if [ file exists CFG_GRIB ] {set arg7 -q1}

#   convert grib data to hysplit regional grid
    $log insert end "\n"
    if [file exists $data_file] {
       $log insert end "Starting to convert: $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7\n"
       $log insert end "Program directory  : $exec_dir \n\n"
    } else {
       $log insert end "Missing file: $data_file ... processing terminated \n"
       destroy .ecmarch
       return}

    if [ file exists STDOUT ] {file delete STDOUT} 
    if { "$tcl_platform(platform)" == "unix" } {
      if { "$X_dir" == "" } {
      exec $exec_dir/grib2arl $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 >STDOUT
      } else {
      exec $X_dir/xterm -fn fixed -e $exec_dir/grib2arl \
           $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7    >STDOUT
      }
    } else {
      exec $exec_dir/grib2arl.exe $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 >STDOUT
    }

    if [file exists STDOUT] {
       set fid [open "STDOUT" r]
       while {[eof $fid] != 1} {
          gets $fid cline
          $log insert end $cline\n
       }
       close $fid
       file delete STDOUT
    }

    if [file exists DATA.ARL] {
       $log insert end "Processed: $data_file ... created DATA.ARL \n"
       update
    } else {
       $log insert end "Missing file: DATA.ARL ... output from grib2arl \n"
       destroy .ecmarch
       return}

#   initialize summation file or append output file
    if [file exists $Fcst_path/$Fcst_file] {

       if { "$tcl_platform(platform)" == "unix" } {
#         exec /usr/bin/cat DATA.ARL >>$Fcst_path/$Fcst_file
          exec cat DATA.ARL >>$Fcst_path/$Fcst_file
       } else {
#         exec cmd.exe /c type DATA.ARL >>$Fcst_path/$Fcst_file    
          exec $exec_dir/file_copy.exe DATA.ARL $Fcst_path/$Fcst_file   
       }
       $log insert end "Added data: DATA.ARL to end of $Fcst_file \n"

    } else {
       file rename DATA.ARL $Fcst_path/$Fcst_file
       $log insert end "Initialized: $Fcst_file \n"
    }

#   dump diagnostic message with each time period
    $log insert end "Finished: $data_file \n"
    update
}
}
destroy .ecmarch
}
