#!/bin/sh
# the next line restarts using wish \
exec wish "$0" "$@"

#-----------------------------------------------------------------------------
# QWIK_CONFIG.TCL: main HYSPLIT configuration script
# Last Revised:    11 Apr 2007
#                  11 Jun 2007 - more Apple OSX friendly
#                  11 Mar 2008 - convert from CONUS NAM to quadrant tiles
#                  21 Oct 2010 - scale_form return
#                  29 Oct 2010 - auto directory configuration
#                  05 Jul 2011 - fix to quadrant computation
#                  01 Mar 2021 - change a site to ftp.arl.noaa.gov
#-----------------------------------------------------------------------------

eval destroy [winfo child .]
wm title . " HYSPLIT QWIK CONFIG Version 2.13 "
wm geometry . +25+25

frame .buttons
pack  .buttons 

option add *background grey85
option add *foreground black
option add *font -*-courier-bold-r-*-*-14-140-*-*-*-*-*-*
option add *highlightBackground black
option add *Button.background CadetBlue4
option add *activeBackground SeaGreen
set font -*-Helvetica-Bold-R-Normal--*-120-*-*-*-*-*-*

global tcl_platform
if { "$tcl_platform(platform)" == "unix" } {
   button .buttons.exit -text "Close all windows and Exit" -bg red -width 40 \
   -command "destroy ."
} else {
   button .buttons.exit -text "Close all windows and Exit" -bg red -width 40 \
   -command "destroy ."
#  -command "exec taskkill /IM wish84.exe"
}
pack   .buttons.exit -side left
after 100 run_prog

#--------------------------------------------------------------
proc run_prog {} {

global env
global ktime
global tcl_platform
global result count
global title site olat olon nfile dir1 met1 dir2 met2 quad tile ntyp
global year month day hour run dur rate rbot rtop poll syr smo sda shr zcon  

# directory prefix for local installation
if { "$tcl_platform(platform)" == "unix" } {
   if {[array names env USERNAME] == "USERNAME"} {set PRE /home/$env(USERNAME)}
   if {[array names env USER]     == "USER"}     {set PRE /home/$env(USER)}

   if { "$tcl_platform(os)" == "Darwin" } {
      if {[array names env USERNAME] == "USERNAME"} {set PRE /Users/$env(USERNAME)}
      if {[array names env USER]     == "USER"}     {set PRE /Users/$env(USER)}
   }

#  over-ride previous account name defined directory, use current startup
#  requires qwikcode directory in parallel to other hysplit2 directories
   set PRE [file dirname $env(PWD)]

} else {
#  default install
   set PRE "/hysplit"
#  custom install
   if [file exists /Home] {set PRE /Home/hysplit}
}


# scripts directory for this and other scripts 
set SCR ${PRE}/qwikcode

# hysplit executables directory
set EXE ${PRE}/exec 

# working directory for results   
set WRK ${PRE}/working

# installed system components
if { "$tcl_platform(platform)" == "unix" } {
   set PRG "/usr/local"

#  set MAG ${PRG}/bin/convert
   set MAG convert
#  set GSC ${PRG}/bin/gs 
   set GSC gs
   set GED ""

#  set MAG ${PRG}/ImageMagick-6.1.5/bin/convert
#  set GSC ${PRG}/ImageMagick-6.1.5/bin/display

   if { "$tcl_platform(os)" == "Darwin" } {set GSC open}

} else {

# set PRG "c:/Program Files (x86)"
# if {[array names env Program*] == "ProgramFiles"} {set PRG [file rootname $env(ProgramFiles)]}
# set PRG /non/standard/installation
# set MAG ${PRG}/ImageMagick-6.3.1-Q8/convert
# set GSC ${PRG}/Ghostgum/gsview/gsview32.exe

  if {[array names env ProgramFiles] == "ProgramFiles"} {
     set temp [file split $env(ProgramFiles)]
     set drive [lindex $temp 0]
     set PRG [file join $drive [lindex $temp 1] ]
  }

  set temp [glob -nocomplain -types d -directory ${PRG} ImageMagick*]
  set PGMDIR [lindex $temp end]
  if { "$PGMDIR" == "" } {
     set MAG [file join ${drive} ImageMagick convert.exe]
  } else {
      set MAG [file join $PGMDIR convert.exe]}

  set temp [glob -nocomplain -types d -directory ${PRG} Ghost*]
  set PGMDIR [lindex $temp end]
  if { "$PGMDIR" == "" } {
     set GSC [file join ${drive} Ghostgum gsview32.exe]
  } else {
     set GSC [file join $PGMDIR gsview gsview32.exe]}

  set GED ""
  if [file exists "${PRG}/Google/Google Earth/GoogleEarth.exe"] {
     set GED "${PRG}/Google/Google Earth/GoogleEarth.exe"}
  if [file exists "${PRG}/Google/Google Earth Pro/GoogleEarth.exe"] {
     set GED "${PRG}/Google/Google Earth Pro/GoogleEarth.exe"}
}

# all computations in working directory
cd $WRK 

# copy required files
file copy -force ${SCR}/logocon.gif $WRK

# set base name for meteorology quadrants
set quad(1) NEtile
set quad(2) NWtile
set quad(3) SEtile
set quad(4) SWtile
for {set numb 1} {$numb <= 4} {incr numb} {
    if {! [file exist cfg$quad($numb)]} {file copy ${SCR}/cfg$quad($numb) $WRK}
}

# check for previous simulations

if [file exists HYSP.INI.1] { 
    set choice [glob -nocomplain HYSP.INI.?]
    set count  [llength $choice]
} else {
    set count 0
}

if [file exists HYSP.INI] {

   menu_form "CLEAN-UP DIRECTORY" Select 0 \
      "Keep:   last configuration" \
      "Delete: all previous configurations" \
      "Check:  and load a saved configuation"

   tkwait window .menu
   set dash [string first : $result]
   set config [string range $result 0 $dash]
   
   switch $config {

      Delete: {
         set count 0
         foreach match [glob -nocomplain HYSP*]    {file delete $match}
         foreach match [glob -nocomplain MESSAGE*] {file delete $match}
         foreach match [glob -nocomplain MAPTEXT*] {file delete $match}
         foreach match [glob -nocomplain traj.???] {file delete $match}
         foreach match [glob -nocomplain conc.???] {file delete $match}
         foreach match [glob -nocomplain CONTROL*] {file delete $match}
         set init HYSP.INI
         if [file exists cdump] {file delete cdump}
         if [file exists tdump] {file delete tdump}
         if [file exists concplot.ps] {file delete concplot.ps}
         if [file exists trajplot.ps] {file delete trajplot.ps}
         }

      Check: {
         if {$count > 0} { 
            set choice [glob -nocomplain HYSP.INI.?]
            set fcmd {menu_form "SAVED SIMULATIONS" Select 0 } 
            for {set i 0} {$i < [llength $choice]} {incr i} {append fcmd [lindex $choice $i] " "}    
            eval $fcmd

            tkwait window .menu
            set init $result
            set save [string index $init 10]
            if [file exists HYSP_cdump.$save] {file copy HYSP_cdump.$save cdump} 
            if [file exists HYSP_tdump.$save] {file copy HYSP_tdump.$save tdump} 
            if [file exists HYSP_cplot.$save] {file copy HYSP_cplot.$save concplot.ps} 
            if [file exists HYSP_tplot.$save] {file copy HYSP_tplot.$save trajplot.ps} 
           
         } else {
            set init HYSP.INI}
         }

      Keep: {set init HYSP.INI}
   }

} else {

#  No previous simulations present ... clean up files
   set count 0
   set init HYSP.INI
   if [file exists cdump] {file delete cdump}
   if [file exists tdump] {file delete tdump}
   if [file exists PARDUMP] {file delete PARDUMP}
   if [file exists concplot.ps] {file delete concplot.ps}
   if [file exists trajplot.ps] {file delete trajplot.ps}
}

# initialize internal variables

  if ![file exists $init] {
#    Set the basic defaults ... no ini file is present
     set site "Siver Spring MD"
     set olat  38.9924
     set olon -77.0309
     set tile 1

#    meteo data file is available 4 hours after cycle time
     set ktime [clock scan "4 hours ago"]
     set date  [clock format $ktime -format "%y %m %d %H" -gmt 1]
     set day   [lindex [split $date] 2]
     set hour  [lindex [split $date] 3]
     set hh $hour
     if {$hour > 0 && $hour < 10} {regsub 0 $hour " " hh}
     set cycle [format "%2.2u" [expr [expr $hh/6]*6]]
     set dir1 ${WRK}/
     set met1 $quad($tile)${day}${cycle}

#    default start at current time
     set date  [clock format [clock scan now] -format "%y %m %d %H" -gmt 1]
     set year  [lindex [split $date] 0]
     set month [lindex [split $date] 1]
     set day   [lindex [split $date] 2]
     set hour  [lindex [split $date] 3]

     set title "Local scale < 10 km"
     set ntyp 0
     set run   1
     set dur   1.0
     set rate  1.0
     set rbot  10
     set rtop  50
     set poll  Test
     set zcon  100
     set syr   00
     set smo   00
     set sda   00
     set shr   01
     set nfile 1
     set dir2 /dev/null/
     set met2 undefined
 
  } else {

#    initialize current time
     set ktime [clock scan "4 hours ago"]

#    load choices from previous simulation
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
           if {$k == 25} {set tile  $cline}
           if {$k == 26} {set ntyp  $cline}
        }
        close $f

     } else {
        info_form "MISSING FILE" "warning" Continue Exit File $init not found!
        tkwait window .info
        if {$result == 0} {return}
     }
  }

set scode 0
set numb 0
while {$scode == 0} {

menu_form "MODEL OPTIONS LIST" Select $numb Setup Run Display Exit
tkwait window .menu
switch $result {
   Setup {
      setup_model $SCR $EXE
      incr numb}
   Run {
      run_model $SCR $EXE
      incr numb}
   Display {
      disp_model $SCR $EXE $PRG $MAG $GSC $GED
      set numb 0}
   Exit {set scode 1}
}
}
}

#---------------------------------------------------------
proc setup_model {SCR EXE} {

global result count plat plon
global title site olat olon nfile dir1 met1 dir2 met2 quad tile ntyp
global year month day hour run dur rate rbot rtop poll syr smo sda shr zcon  

set mfile 0
set rcode 0
while {$rcode == 0} {
      
menu_form "VIEW/CHANGE SIMULATION" Select 0 \
     "Exit:            Return to main menu" \
     "Type:            $title" \
     "Meteorology:     File=$met1" \
     "FTP_Meteo:       Force FTP now" \
     "Site_Lat_Lon:    Lat=$olat Lon=$olon" \
     "Site_Name:       $site" \
     "Start_Month_Day: Month=$month  Day=$day" \
     "Start_Hour:      UTC=$hour" \
     "Advanced:        Custom Options"

tkwait window .menu
set dash [string first : $result]
set setrun [string range $result 0 $dash]

switch $setrun {
              
Type: {
   menu_form2 "SIMULATION TYPE" Select $ntyp \
   "Local scale < 10 km" \
   "Urban scale < 10 km" \
   "Short range < 50 km" \
   "Long- range > 50 km"
   tkwait window .menu2
   set title $result 
   set type [string range $title 0 4]

   switch $type {
      Local {set run 1; set ntyp 0}
      Short {set run 3; set ntyp 1}
      Urban {set run 1; set ntyp 2} 
      Long- {set run 6; set ntyp 3} }}

FTP_Meteo: {
   get_data $SCR
   if [file exists $quad($tile)] {
       set f [open $quad($tile) r]
       set fdate [read $f 8]
       close $f

       set fda [format "%2.2u" [string range $fdate 4 5]]
       set fhr [format "%2.2u" [string range $fdate 6 7]]
       if [file exists $quad($tile)${fda}${fhr}] {file delete $quad($tile)${fda}${fhr}}
       file rename $quad($tile) $quad($tile)${fda}${fhr}
   } }

Meteorology: {
   set choice [glob -nocomplain $quad($tile)????]
   if {[llength $choice] > 0} {
      set fcmd {menu_form2 "METEOROLOGICAL DATA FILES" Select $mfile } 
      for {set i 0} {$i < [llength $choice]} {incr i} {append fcmd [lindex $choice $i] " "}    

      eval $fcmd
      tkwait window .menu2
      set met1 $result
      set dir1 "./" 
      for {set i 0} {$i < [llength $choice]} {incr i} {if {"[lindex $choice $i]" == "$met1"} {set mfile $i} }
    }} 

Site_Name: {
   set result $site
   prompt_form "SITE IDENTIFICATION NAME"
   tkwait window .prompt
   if {"$result" != ""} {set site $result} }

Site_Lat_Lon: {
   menu_form "RELEASE LOCATION" Select 0 \
   "Select: from file" \
   "Enter: from slider"

   tkwait window .menu
   set dash [string first : $result]
   set latlon [string range $result 0 $dash]

   switch $latlon {                                
      Select: {
         file_form LOCATIONS "${SCR}/plants.txt"
         tkwait window .file
         if {$result != 0} {
            set olat [lindex $result 1]
            set olon [lindex $result 2]
            set site [lrange $result 3 end] }}
      Enter: { 
         set plat $olat; set plon $olon             
         latlon_form "ENTER LATITIDE and LONGITIDE"
         tkwait window .latlon}
      } 

      info_form "RESET METEOROLOGY FILE" question Yes No Recompute Meteorology File Name
      tkwait window .info
      if {$result == 1} {
         set hh $hour
         if {$hour > 0 && $hour < 10} {regsub 0 $hour " " hh}
         set cycle [format "%2.2u" [expr [expr $hh/6]*6]]
         for {set numb 1} {$numb <= 4} {incr numb} {
             set ij [exec ${EXE}/metpoint ./ cfg$quad($numb) $olat $olon]
             set ii [lindex $ij 0]
             set jj [lindex $ij 1]
             if {$ii > 0 && $jj > 0} {set tile $numb}

         }    
         set met1  $quad($tile)${day}${cycle} }
   } 

Start_Month_Day: {
   timer_form "RELEASE START DATE"
   tkwait window .timer
   if {$result != 0} {
      set month [lindex $result 0]
      set day   [lindex $result 1]
      set year  [lindex $result 2]

      info_form "RESET METEOROLOGY FILE" question Yes No Recompute Meteorology File Name
      tkwait window .info
      if {$result == 1} {
         set hh $hour
         if {$hour > 0 && $hour < 10} {regsub 0 $hour " " hh}
         set cycle [format "%2.2u" [expr [expr $hh/6]*6]]
         set met1  $quad($tile)${day}${cycle} } 
   }}
                     
Start_Hour: {
   set result $hour
   scaler_form "RELEASE START HOUR" "UTC HOUR" 0 0 24
   tkwait window .scaler
   if {$result >= 0} {
      if {$result == 24} {set result 0}
      set hour [format "%2.2u" $result]

      info_form "RESET METEOROLOGY FILE" question Yes No Recompute Meteorology File Name
      tkwait window .info
      if {$result == 1} {
         set hh $hour
         if {$hour > 0 && $hour < 10} {regsub 0 $hour " " hh}
         set cycle [format "%2.2u" [expr [expr $hh/6]*6]]
         set met1  $quad($tile)${day}${cycle} }
   }}

Advanced: {
   set tcode 0
   while {$tcode == 0} {

   menu_form "CUSTOM VIEW/CHANGE SIMULATION" Select 0 \
   "Exit:       back to previous menu" \
   "Simulation: duration (hrs) = $run" \
   "Release:    duration (hrs) = $dur" \
   "Emission:   rate (mass/hr) = $rate" \
   "Bottom:     source hgt (m) = $rbot" \
   "Top:        source hgt (m) = $rtop" \
   "Species:    pollutant type = $poll" \
   "Sampling:   duration (hrs) = $shr" \
   "Reset:      values back to defaults"

   tkwait window .menu
   set dash [string first : $result]
   set setloc [string range $result 0 $dash]

   switch $setloc {

       Reset: {
          set run   1
          set dur   1.0
          set rate  1.0
          set rbot  10
          set rtop  50
          set poll  Test
          set shr   01}
        
       Simulation: {
           set result $run
           scaler_form "RUN DURATION" "HOURS" 0 0 12
           tkwait window .scaler
           if {$result >= 0} {set run $result}}
                          
       Release: {
           set result $dur
           scaler_form "RELEASE DURATION" "HOURS" 1 0 6
           tkwait window .scaler
           if {$result >= 0} {set dur $result} }
                    
       Emission: {
           set result $rate
           scaler_form "EMISSION RATE" "MASS/HOUR" 1 0 1000
           tkwait window .scaler
           if {$result >= 0} {set rate $result} }
          
       Bottom: {
           set result $rbot
           scaler_form "BASE OF RELEASE" "METERS" 0 0 1000
           tkwait window .scaler
           if {$result >= 0} {set rbot $result} }
                    
       Top: {
           set result $rtop
           scaler_form "TOP OF RELEASE" "METERS" 0 0 1000
           tkwait window .scaler
           if {$result >= 0} {set rtop $result} }
                    
       Species: {
          menu_form2 "POLLUTANT SPECIES" Select 0 \
          "Test: Generic inert" \
          "C137: Cesium 137" \
          "I131: Iodine 131"
          tkwait window .menu2
          set dash [expr [string first : $result] - 1]
          set poll [string range $result 0 $dash] } 

       Sampling: {
          set result $shr
          scaler_form "OUTPUT AVERAGING TIME" "HOURS" 0 0 18
          tkwait window .scaler
          if {$result >= 0} {set shr [format "%2.2u" $result]} }
                      
       Exit: {set tcode 1}

   }}}

   Exit: {set rcode 1} 
      
}} 


#----------------------------------------------------------
# complete simulation setup -> check for meteorology

  if [file exists ${met1}] {
     set f [open ${met1} r]
     set fdate [read $f 8]
     close $f

  } else {
     info_form "METEOROLOGY FILE NOT FOUND" warning "FTP current data" "Continue Anyway" \
     [pwd]$met1
     tkwait window .info

     if {$result == 1} {
        get_data $SCR
        if [file exists $quad($tile)] {
           set f [open $quad($tile) r]
           set fdate [read $f 8]
           close $f

           set fda [format "%2.2u" [string range $fdate 4 5]]
           set fhr [format "%2.2u" [string range $fdate 6 7]]
           if [file exists $quad($tile)${fda}${fhr}] {file delete $quad($tile)${fda}${fhr}}
           file rename $quad($tile) $quad($tile)${fda}${fhr}
        } else {
           info_form "FTP METEOROLOGY FILE FAILURE" error Continue " " Reason Unknown
           tkwait window .info
           set fdate " 0 0 0 0"
        }
     } else {
        set fdate " 0 0 0 0"}
  }
 
  set mm [string range $fdate 2 3]
  set dd [string range $fdate 4 5]
  set hh [string range $fdate 6 7]
  set ctime [expr $mm * 720 + $dd * 24 + $hh]

  set mm $month; if {$month > 0 && $month < 10} {regsub 0 $month " " mm}
  set dd $day;   if {$day   > 0 && $day   < 10} {regsub 0 $day   " " dd}
  set hh $hour;  if {$hour  > 0 && $hour  < 10} {regsub 0 $hour  " " hh}
  set stime [expr $mm * 720 + $dd * 24 + $hh]

  if {$stime < $ctime || $stime > [expr $ctime + 18]} {
     if {$fdate == " 0 0 0 0"} {set fdate "File Not Found"}
     info_form "METEOROLOGY DATE NOT FOUND IN FILE" error "Browse for File" \
     "Continue Anyway" Date in $met1 = $fdate
     tkwait window .info

     if {$result == 1} {
        set match [glob -nocomplain $quad($tile)????]
        if {[llength $match] > 0} {
        set fcmd {menu_form "METEOROLOGICAL DATA FILES" Select 0 } 
        for {set i 0} {$i < [llength $match]} {incr i} {append fcmd [lindex $match $i] " "}    
        eval $fcmd
         
        tkwait window .menu
        set met1 $result
        set dir1 "./" 
        set rcode 1}}
  }

#----------------------------------------------------------
# write simulation configuration file

  set f [open HYSP.INI w]
  puts $f $title
  puts $f $site 
  puts $f $olat  
  puts $f $olon  
  puts $f $nfile 
  puts $f $dir1  
  puts $f $met1  
  puts $f $dir2  
  puts $f $met2  
  puts $f $year
  puts $f $month
  puts $f $day 
  puts $f $hour 
  puts $f $run   
  puts $f $dur   
  puts $f $rate  
  puts $f $rbot  
  puts $f $rtop  
  puts $f $poll  
  puts $f $syr  
  puts $f $smo  
  puts $f $sda
  puts $f $shr
  puts $f $zcon
  puts $f $tile
  puts $f $ntyp
  close $f
}


#---------------------------------------------------------
proc run_model {SCR EXE} {

   global result

   info_form "RUN TRAJECTORY MODEL" question OK Cancel Submit trajectory job
   tkwait window .info
   if {$result == 1} {
      source ${SCR}/qwik_rtraj.tcl
      run_traj $SCR $EXE}
 
      
   info_form "RUN DISPERSION MODEL" question OK Cancel Submit dispersion job
   tkwait window .info
   if {$result == 1} {
      source ${SCR}/qwik_rconc.tcl
      run_conc $SCR $EXE}
}


#-----------------------------------------------------------
proc disp_model {SCR EXE PRG MAG GSC GED} {

global log result count zcon

set scode 0
set numb 0
while {$scode == 0} {
menu_form  "OUTPUT MENU OPTIONS" Select $numb \
   "Display:   postscript file" \
   "Convert:   postscript to GIF" \
   "Google:    for import into Google Earth" \
   "Shapefile: for import into ArcExplorer" \
   "Save:      the graphics to a unique file name" \
   "Restore:   the graphics from a previous run" \
   "Exit:      return to main menu"

tkwait window .menu
set dash [string first : $result]
set plot [string range $result 0 $dash]

switch $plot {

   Save: {
      incr count
      info_form  "SAVE GRAPHICS" question Save Cancel Saving graphic to HYSP_xxxxx.${count}
      tkwait window .info
      if {$result == 1} {
         if [file exists tdump] {file copy -force tdump HYSP_tdump.$count}
         if [file exists cdump] {file copy -force cdump HYSP_cdump.$count}
         if [file exists HYSP.INI] {file copy -force HYSP.INI HYSP.INI.$count}
         if [file exists trajplot.ps] {file copy -force trajplot.ps HYSP_tplot.$count}
         if [file exists concplot.ps] {file copy -force concplot.ps HYSP_cplot.$count} 
      }
      set numb 6}
 
   Restore: {
      set choice [glob -nocomplain HYSP.INI.?]
      if {[llength $choice] > 0} {
         set fcmd {menu_form "RESTORE GRAPHICS" Select 0 } 
         for {set i 0} {$i < [llength $choice]} {incr i} {append fcmd [lindex $choice $i] " "}    
         eval $fcmd

         tkwait window .menu
         set reset [string index $result 9]
         file copy -force HYSP.INI.$reset HYSP.INI
         if [file exists HYSP_tdump.$reset] {file copy -force HYSP_tdump.$reset tdump}
         if [file exists HYSP_cdump.$reset] {file copy -force HYSP_cdump.$reset cdump}
         if [file exists HYSP_tplot.$reset] {file copy -force HYSP_tplot.$reset trajplot.ps}
         if [file exists HYSP_cplot.$reset] {file copy -force HYSP_cplot.$reset concplot.ps}

      } else {
         info_form  "WARNING" warning Continue Exit No graphics saved to restore!
         tkwait window .info
         if {$result == 1} {set scode 1} 
      }
      set numb 0}
  
   Display: {

      info_form "DISPLAY TRAJECTORY" question OK Cancel Display Trajectory Graphic
      tkwait window .info
      if {$result == 1} {

      if [file exists tdump] {
         if [file exists trajplot.ps] {file delete trajplot.ps} 
         file copy -force MAPTEXT.traj MAPTEXT.CFG   
         set xops "|${EXE}/trajplot -itdump -l1 -h -g4:50 -j${SCR}/map_county"

         set log [ScrollText .clog]
         $log configure -cursor watch
         $log insert end "Trajectory graphics processing started ...\n"
         update  
         if [catch {open $xops} result] {
            $log insert end $result
         } else {
            fileevent $result readable Log
         }
         tkwait window .clog
         file delete MAPTEXT.CFG

      } else {
         info_form "MISSING FILE" error Quit Exit \
         HYSPLIT trajectory output file (tdump) not found!
         tkwait window .info
      }
      if [file exists trajplot.ps] {exec "${GSC}" trajplot.ps &}
      }

      info_form "DISPLAY CONCENTRATIONS" question OK Cancel Display Concentration Graphic
      tkwait window .info
      if {$result == 1} {

      if [file exists cdump] {
         if [file exists concplot.ps] {file delete concplot.ps} 
         file copy -force MAPTEXT.conc MAPTEXT.CFG   
         set xops "|${EXE}/concplot -icdump -z80 -k2 -h -g3:10 -j${SCR}/map_county"

         set log [ScrollText .clog]
         $log configure -cursor watch
         $log insert end "Concentration graphics processing started ...\n"
         update  
         if [catch {open $xops} result] {
            $log insert end $result
         } else {
            fileevent $result readable Log
         }
         tkwait window .clog
         file delete MAPTEXT.CFG

      } else {
         info_form "MISSING FILE" "error" Quit Continue \
         HYSPLIT concentration output file (cdump) not found!
         tkwait window .info
      }
      if [file exists concplot.ps] {exec "${GSC}" concplot.ps &}
      }
      set numb 6}

   Convert: {
      if [file exists trajplot.ps] {
         exec "${MAG}" -trim +repage trajplot.ps trajplot.gif
      }
      if [file exists concplot.ps] {
         exec "${MAG}" -trim +repage -delay 50 concplot.ps concplot.gif
      }
      set numb 6}

   Google: {
      if [file exists tdump] {
         set xops "|${EXE}/trajplot -itdump -z80 -a3 -ogoogle"
         set log [ScrollText .clog]
         $log configure -cursor watch
         $log insert end "Trajectory GoogleEarth conversion started ...\n"
         update  
         if [catch {open $xops} result] {
            $log insert end $result
         } else {
            fileevent $result readable Log
         }
         tkwait window .clog
         if [file exists google.ps] {file delete google.ps}

         exec ${EXE}/zip.exe HYSP_traj.kmz HYSPLITtraj_ps_01.kml logocon.gif
         file delete HYSPLITtraj_ps_01.kml
         info_form  "GE TRAJECTORY CONVERSION COMPLETE" info Continue "Open GE" \
                     HYSP_traj.kmz in [pwd]
         tkwait window .info
         if {$result != 1} {
            if {$GED != ""} {exec ${GED} [pwd]/HYSP_traj.kmz &
            after 100 destroy .}
         }
      }

      if [file exists cdump] {
         set xops "|${EXE}/concplot -icdump -z80 -a3 -ogoogle"
         set log [ScrollText .clog]
         $log configure -cursor watch
         $log insert end "Concentration shapefile conversion started ...\n"
         update  
         if [catch {open $xops} result] {
            $log insert end $result
         } else {
            fileevent $result readable Log
         }
         tkwait window .clog
         if [file exists google.ps] {file delete google.ps}

         exec ${EXE}/gelabel
         foreach match [glob -nocomplain GELABEL_??_ps.ps] {
            exec "${MAG}" -trim +repage $match [file rootname $match].gif}
         exec ${EXE}/zip.exe HYSP_conc.kmz HYSPLIT_ps.kml logocon.gif GELABEL_??_ps.gif
         foreach match [glob -nocomplain GELABEL*] {file delete $match}
         file delete HYSPLIT_ps.kml gistmp_ps.txt
         info_form  "GE CONCENTRATION CONVERSION COMPLETE" info Continue "Open GE" \
                     HYSP_conc.kmz in [pwd]
         tkwait window .info
         if {$result != 1} {
            if {$GED != ""} {exec ${GED} [pwd]/HYSP_conc.kmz &
            after 100 destroy .}
         }
      }
      set numb 6}

   Shapefile: {
      if [file exists tdump] {
         set xops "|${EXE}/trajplot -itdump -z80 -a1 -oshape"
         set log [ScrollText .clog]
         $log configure -cursor watch
         $log insert end "Trajectory shapefile conversion started ...\n"
         update  
         if [catch {open $xops} result] {
            $log insert end $result
         } else {
            fileevent $result readable Log
         }
         tkwait window .clog
         if [file exists shape.ps] {file delete shape.ps}
       
         exec ${EXE}/ascii2shp -d traj points <GIS_traj_ps_01.txt
         file delete GIS_traj_ps_01.txt
         file delete GIS_traj_ps_01.att

         if [file exists traj.shp] {
            info_form  "TRAJECTORY SHAPEFILE CONVERSION COMPLETE" info Continue Quit \
                        Created -> traj.(shp,shx,dbf)
            tkwait window .info}
      }

      if [file exists cdump] {
         set xops "|${EXE}/concplot -icdump -z80 -a1 -oshape"
         set log [ScrollText .clog]
         $log configure -cursor watch
         $log insert end "Concentration shapefile conversion started ...\n"
         update  
         if [catch {open $xops} result] {
            $log insert end $result
         } else {
            fileevent $result readable Log
         }
         tkwait window .clog
         if [file exists shape.ps] {file delete shape.ps}

         set level [format "%5.5d" $zcon]
         exec ${EXE}/ascii2shp -d conc polygons <GIS_${level}_ps_00.txt
         file delete GIS_${level}_ps_00.txt
         file delete GIS_${level}_ps_00.att
         file delete gistmp_ps.txt

         if [file exists conc.shp] {
            info_form  "CONCENTRATION SHAPEFILE CONVERSION COMPLETE" info Continue Quit \
                        Created -> conc.(shp,shx,dbf)
            tkwait window .info}
      }
      set numb 6}
   
   Exit: {set scode 1} 
}
}
}


#---------------------------------------------------------
proc info_form {{name Title} {icon info} {lftb Continue} {rgtb Cancel} args} {

global result
if [winfo exists .info] {destroy .info}
set wr .info
toplevel $wr
wm title $wr $name
wm geometry $wr +100+100

# icon options: error info question hourglass warning

frame  $wr.top
frame  $wr.bottom
button $wr.lftb -text $lftb -bg green -width 20 -command {set result 1; destroy .info}
button $wr.rgtb -text $rgtb -bg red   -width 20 -command {set result 0; destroy .info}
label  $wr.prompt -width 40 -text $args -relief sunken
label  $wr.flag -bitmap question -fg red
pack   $wr.top $wr.bottom -fill both -expand 1
pack   $wr.flag -anchor w -ipadx 1m -padx 1m -side left -in $wr.top
pack   $wr.prompt -in $wr.top -anchor e -pady 1m -fill both -expand 1
pack   $wr.lftb -in $wr.bottom  -side left  -pady 1m
pack   $wr.rgtb -in $wr.bottom  -side right -pady 1m
bind all <Return> {set result 1; destroy .info}
}

#----------------------------------------------------------------
proc menu_form {{name Title} {butt Select} {numb 0} args} {

global result

if [winfo exists .menu] {destroy .menu}
set wr .menu
toplevel $wr
wm title $wr $name
wm geometry $wr +100+100

frame $wr.top -width 20m
frame $wr.buttons
scrollbar $wr.scroll -command "$wr.list yview"
listbox $wr.list -yscroll "$wr.scroll set" -relief sunken -width 40 
button $wr.ok -bg green -text $butt -command {set result [selection get]; destroy .menu}

pack $wr.top -side top -fill x
pack $wr.buttons -side top -fill x
pack $wr.scroll -in $wr.top -side right -fill y
pack $wr.list -in $wr.top -fill both
pack $wr.ok -in $wr.buttons -fill x

foreach i $args {$wr.list insert end $i}
$wr.list selection set $numb
bind $wr.list <Double-Button-1> {set result [selection get]; destroy .menu}
}

#----------------------------------------------------------------
proc menu_form2 {{name Title} {butt Select} {numb 0} args} {

global result

if [winfo exists .menu2] {destroy .menu2}
set wr .menu2
toplevel $wr
wm title $wr $name
wm geometry $wr +100+100

frame $wr.top -width 20m
frame $wr.buttons
scrollbar $wr.scroll -command "$wr.list yview"
listbox $wr.list -yscroll "$wr.scroll set" -relief sunken -width 40 
button $wr.ok -bg green -text $butt -width 20 -command {set result [selection get]; destroy .menu2}
button $wr.can -bg red -text Cancel -width 20 -command {destroy .menu2}

pack $wr.top -side top -fill x
pack $wr.buttons -side top -fill x
pack $wr.scroll -in $wr.top -side right -fill y
pack $wr.list -in $wr.top -fill both
pack $wr.ok -in $wr.buttons -side left
pack $wr.can -in $wr.buttons -side right

foreach i $args {$wr.list insert end $i}
$wr.list selection set $numb
set result [lindex $args $numb]
bind $wr.list <Double-Button-1> {set result [selection get]; destroy .menu2}
}

#-----------------------------------------------------------------------------
proc latlon_form {name} {

global olat olon plat plon

if [winfo exists .latlon] {destroy .latlon}
set wr .latlon
toplevel $wr
wm title $wr $name
wm geometry $wr +100+100

scale $wr.longit -label "Longitude -W+E" -from -125 -to -65 \
   -tickinterval 20 -resolution .01 -orient horizontal \
   -variable plon

scale $wr.latit -label "Latitude +N-S" -from 50 -to 25 \
   -tickinterval 10 -resolution .01 \
   -variable plat

button $wr.ok -bg green -text "Accept" -width 20 \
   -command {set olat $plat; set olon $plon; destroy .latlon}
button $wr.can -bg red   -text "Cancel" -width 20 \
   -command {destroy .latlon}

pack $wr.longit -side top -fill x -pady 1m
pack $wr.latit  -fill y -pady 1m
pack $wr.ok  -side left  -pady 2m -padx 1m 
pack $wr.can -side right -pady 2m -padx 1m

bind all <Return> {destroy .latlon}
}


#-----------------------------------------------------------------------------
proc scaler_form {name message ndecs left_value right_value} {

global result

if [winfo exists .scaler] {destroy .scaler}
set wr .scaler
toplevel $wr
wm title $wr $name
wm geometry $wr +100+100

set res  [expr pow(.1,$ndecs)]
set tics [expr $left_value + $right_value / 2 ]
set incr [expr ($right_value - $left_value) / 10.0 / $res]

label $wr.prompt -width 30 -text $message -relief ridge
scale $wr.scale -label "" -from $left_value -to $right_value \
   -resolution $res -orient horizontal -variable temp \
   -tickinterval $tics -bigincrement $incr

button $wr.ok  -bg green -width 20 -text "Accept" -command {set result $temp; destroy .scaler}
button $wr.can -bg red   -width 20 -text "Cancel" -command {set result -1;    destroy .scaler}

pack $wr.prompt -side top -fill x
pack $wr.scale  -side top -fill x
pack $wr.ok  -side left  -pady 2m
pack $wr.can -side right -pady 2m 

bind all <Return> {destroy .scaler}
}


#-----------------------------------------------------------------------------
proc timer_form {name} {

global result
global ktime
global date dateent

if [winfo exists .timer] {destroy .timer}
set wr .timer
toplevel $wr
wm title $wr $name
wm geometry $wr +100+100

# Frames are for containing buttons in rows.  They must be declared
# first, or the buttons they contain will be hidden by them.

frame $wr.upper
frame $wr.lower
frame $wr.bottom

# Create increase (plus) and decrease (minus) buttons for each
# part of date (Month, Day, Year)

foreach i {Month Day Year} {
  button $wr.bplus$i  -bg yellow -text "+ $i" -command "chg add $i"
  pack   $wr.bplus$i  -in $wr.upper -side left -expand yes
  button $wr.bminus$i -bg yellow -text "- $i" -command "chg subt $i"
  pack   $wr.bminus$i -in $wr.lower -side left -expand yes
}

set result 0
set date [clock format $ktime -format "%m %d %y" -gmt 1]
set dateent [join $date /]

label  $wr.date -relief sunken -textvariable dateent 
button $wr.ok -bg green -width 15 -text "Accept" -command {set result $date; destroy .timer}
button $wr.cancel -bg red -width 15 -text "Cancel" -command {destroy .timer}
pack   $wr.ok $wr.cancel -side left -in $wr.bottom -expand yes

pack $wr.upper -fill x -expand yes
pack $wr.date -fill x -expand yes
pack $wr.lower -fill x -expand yes
pack $wr.bottom -fill both -expand yes -pady 2m

bind all <Control-c> {destroy .timer}
bind $wr.cancel <Return> {destroy .timer}
bind $wr.ok <Return> {set result $date; destroy .timer}
after 500 focus $wr.date
}

proc chg {type amount} {
  global ktime
  global date dateent

  switch $type {
     add {set direc 1}
     subt {set direc -1}
  }
  set ktime [clock scan "$direc $amount" -base $ktime]
  set date [clock format $ktime -format "%m %d %y"]
  set dateent [join $date /]
}

#-----------------------------------------------------------------------------
proc prompt_form {name} {

global result

if [winfo exists .prompt] {destroy .prompt}
set wr .prompt
toplevel $wr
wm title $wr $name
wm geometry $wr +100+100

frame $wr.bottom
entry $wr.entry -width 30 -relief sunken -textvar result
button $wr.ok  -bg green -width 20 -text "Enter"  -command {destroy .prompt}
button $wr.can -bg red   -width 20 -text "Cancel" -command {set result ""; destroy .prompt}
pack $wr.entry -padx 2m -pady 2m
pack $wr.bottom -fill both -expand 1
pack $wr.ok $wr.can -side left -in $wr.bottom  -expand yes -pady 1m
bind all <Return> {destroy .prompt}
}


#-----------------------------------------------------------------------------
proc file_form {name file} {

global result

if [winfo exists .file] {destroy .file}
set wr .file
toplevel $wr
wm title $wr $name
wm geometry $wr +100+100

set result 0

frame $wr.top -width 40 
frame $wr.buttons
scrollbar $wr.scroll -command "$wr.list yview"
listbox $wr.list -yscroll "$wr.scroll set" -relief sunken -width 40

button $wr.ok -bg green -width 20 -text "Select" -command {set result [selection get]; destroy .file}
button $wr.cancel -bg red -width 20 -text "Cancel" -command {destroy .file}

pack $wr.top -side top -fill x
pack $wr.buttons -side top -fill x
pack $wr.scroll -in $wr.top -side right -fill y
pack $wr.list -in $wr.top -fill x
pack $wr.ok -in $wr.buttons -side left -fill x
pack $wr.cancel -in $wr.buttons -side right 

set f [open $file]
while {[gets $f line] >=0} {
  $wr.list insert end $line
}
close $f
 
$wr.list selection set 0
bind $wr.list <Double-Button-1> {set result [selection get]; destroy .file}
}

#---------------------------------------------------------------------
proc ScrollText {fw} {

if [winfo exists $fw ] {destroy $fw}
toplevel $fw
wm title $fw "SIMULATION LOG"
wm geometry $fw +200+200
                       
set f $fw.f

frame $f
focus $f
text $f.text -width 60 -height 15 -setgrid true -wrap none \
   -yscrollcommand [list $f.yscroll set] \
   -xscrollcommand [list $f.xscroll set]
scrollbar $f.yscroll -orient vertical   -command [list $f.text yview]
scrollbar $f.xscroll -orient horizontal -command [list $f.text xview]
button $f.dismiss -bg yellow -text "Close only when Complete" -width 59 -command "destroy $fw"

pack $f -expand yes -fill both
pack $f.yscroll -side right -fill y
pack $f.xscroll -side bottom -fill x
pack $f.text -expand yes -fill both
pack $f.dismiss -side left -expand yes -fill x
return $f.text
}

proc Log {} {
global result log
if [eof $result] {
   catch {close $result}
   $log configure -cursor arrow
   bell
} else {
   gets $result line
   $log insert end $line\n
   $log see end
}
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


#---------------------------------------------------------
proc get_data {SCR} {

global env fsize result quad tile

# set host ftpprd.ncep.noaa.gov
  set host ftp.arl.noaa.gov
  set user anonymous
  set pass user@hysplit.org

if {[array names env USERNAME] == "USERNAME"} {set pass $env(USERNAME)@hysplit}
if {[array names env USER] == "USER"} {set pass $env(USER)@hysplit}
if {[array names env SYSTEM] == "SYSTEM"} {append pass .$env(SYSTEM)}
if {[array names env HOSTNAME] == "HOSTNAME"} {append pass .$env(HOSTNAME)}

set ktime [clock scan "4 hours ago"]
set date  [clock format $ktime -format "%Y %m %d %H" -gmt 1]
set year  [lindex [split $date] 0]
set month [lindex [split $date] 1]
set day   [lindex [split $date] 2]
set hour  [lindex [split $date] 3]
set hh $hour
if {$hour > 0 && $hour < 10} {regsub 0 $hour " " hh}
set cycle [format "%2.2u" [expr [expr $hh/6]*6]]
 
# set data_dir  "/pub/data/nccf/com/hysplit/prod/hysplit.${year}${month}${day}"
  set data_dir  "/pub/forecast/${year}${month}${day}"
  set data_file "hysplit.t${cycle}z.namsf.$quad($tile)"

if [catch {package require ftp} rcode] {
   info_form  "Alternate FTP: Progress Update Unavailable" info Continue Quit \
   Selection box will return when FTP done!
   tkwait window .info

   if {$result == 1} {
      if [info exists input] {unset input}
      append input "user $user $pass \n"
      append input "cd $data_dir \n"
      append input "binary \n"
      append input "prompt \n" 
      append input "get $data_file $quad($tile) \n" 
      append input "bye \n"
      exec ftp -nu $host << $input
   }

} else {
   set handle [::ftp::Open     $host $user $pass -blocksize 1048576 -progress ftp_box 0]
   set rcode  [::ftp::Cd       $handle $data_dir]
   set rcode  [::ftp::Type     $handle binary]
   set fsize  [::ftp::FileSize $handle $data_file]
   set rcode  [::ftp::Get      $handle $data_file $quad($tile)]
   set rcode  [::ftp::Close    $handle]
   if [winfo exists .ftp_win] {destroy .ftp_win}
}
}
