#!/bin/sh

#------------------------------------------------------------------------------
# HYSPLIT.TCL: Root script to start Hysplit GUI
# Last Revised: 14 Aug 2002
#               01 Apr 2003
#               17 Jun 2003 - shape directory
#               26 Jan 2004 - link to gs8.13
#               26 Aug 2004 - option to modify menu default directories
#               12 Oct 2004 - read environment to set default directories
#               18 Jan 2005 - powerpc/darwin compatibiity
#               28 Jan 2005 - improved initialization for non-standard dir
#               22 Mar 2006 - added location of zip.exe
#               12 Jun 2006 - support newer versions of gs/gsv/im
#               25 Sep 2006 - base directory in default_exec
#               20 Nov 2006 - download progress widget
#               13 Aug 2007 - directory initialization update
#               29 Oct 2007 - added version string for setup update 
#               18 Nov 2007 - force MAC directory structure
#               28 Feb 2008 - graphics initialization
#               10 Oct 2008 - version 4.9 compatibility
#               06 Jan 2009 - make new working directory
#               13 Aug 2009 - create sample and default files upon first start
#               17 Aug 2009 - simplified directory search for installed software
#               07 Oct 2009 - uses startup directory for working if no default
#               14 Oct 2009 - more unix friendly default startup directories
#               28 Oct 2009 - automatic teapot install of FTP package (tcl 8.5)
#               29 Apr 2010 - added trajectory balls for google earth
#               30 Jun 2010 - replaced windows cmd.exe with tcl command
#               24 Nov 2010 - extra search directory for xterm
#               05 Dec 2010 - revised default FTP server locations
#               14 Feb 2011 - release date simplification
#               07 Apr 2011 - xterm search on startup for Mac
#               16 Mar 2012 - testing for 32 or 64 bit programs
#               18 May 2012 - more vertical motion options
#               14 Sep 2016 - added global variable for save/retrieve path
#               12 Jan 2018 - added catps2ps -m to ps_box for Mac OS
#               28 Aug 2018 - moved ftp_box window further to the right
#               28 Feb 2019 - use gswin for display if gsview is missing
#               16 Jan 2020 - add option to use FORTRAN or Python programs.
#               04 Apr 2020 - changed from hysplit4 to hysplit.
#               07 Sep 2020 - improved directory search for Mac install
#               21 Jul 2021 - add web browser to view svg-embedded html.
#               27 Jul 2021 - remove ghostscript and ghostview dependencies.
#               26 Aug 2021 - query Windows registry to locate web browser.
#------------------------------------------------------------------------------
# If the GUI is to be run from a directory other than the working directory
# under the main hysplit directory, then it is necessary to edit the pointers
# below from relative to absolute directory locations.
#------------------------------------------------------------------------------

# the next line restarts using wish which should be in path \
exec wish "$0" "$@"

eval destroy [winfo child .]
wm title . " Hysplit "
wm geometry . +150+50

#-------------------------------------------------------------------
proc read_exec_file {file match n} {
set f [open $file r]
set default_string " "
while {![eof $f]} {
  gets $f default_string
  if { $default_string == $match } break
}
for {set i 1} {$i <=$n} { incr i} {
  gets $f default_string
}
close $f
return $default_string
}

#-------------------------------------------------------------------

global ftparc ftpfor dirarc dirfor hostfor hostarc
global env start_loc_file html_dir exec_dir clus_dir
global X_dir tcl_platform magick_pgm home_dir Grids_path Save_path
global zip_pgm tcl_dir maps_dir fixed_dir Work_path
global anaconda3_env wb_pgm PyPreferred
global version

# Use FORTRAN programs by default
set PyPreferred false

set version " November 2023 Release (Version 5.3.0)"

#--------------------------------------------------------
# when exec configuration file exists then load directories

if [file exists default_exec] { 

   set start_loc_file [read_exec_file "default_exec" "#Starting_locations" 1] 
   set       html_dir [read_exec_file "default_exec" "#Help_file_dir" 1]
   set       exec_dir [read_exec_file "default_exec" "#Exec_files_dir" 1]
   set          X_dir [read_exec_file "default_exec" "#Xwindows_dir" 1]
   set     magick_pgm [read_exec_file "default_exec" "#Image_Magick_pgm" 1]
   set        zip_pgm [read_exec_file "default_exec" "#Zip_pgm" 1]
   set       home_dir [read_exec_file "default_exec" "#Home_dir" 1]
   set     Grids_path [read_exec_file "default_exec" "#Metdata_dir" 1]
   set        tcl_dir [read_exec_file "default_exec" "#Tcl_source_dir" 1]
   set       maps_dir [read_exec_file "default_exec" "#Mapping_dir" 1]
   set      fixed_dir [read_exec_file "default_exec" "#Fixed_files_dir" 1]
   set       clus_dir [read_exec_file "default_exec" "#Cluster_directory" 1]
   set      Work_path [read_exec_file "default_exec" "#Working_directory" 1]
   set  anaconda3_env [read_exec_file "default_exec" "#Anaconda3_env" 1]
   set         wb_pgm [read_exec_file "default_exec" "#Web_browser_pgm" 1]
   set    PyPreferred [string is true -strict [read_exec_file "default_exec" "#PyPreferred" 1]]

   set fnams [pwd]
   cd $home_dir
   set home_dir [pwd] 
   cd $fnams     

   if [file exists $Work_path] {
      cd $Work_path
      set Work_path [pwd]

   } else {
      file mkdir $Work_path
      if [file exists ASCDATA.CFG]  {file copy ASCDATA.CFG  $Work_path}
      if [file exists default_conc] {file copy default_conc $Work_path}
      if [file exists default_traj] {file copy default_traj $Work_path}
      if [file exists default_exec] {file copy default_exec $Work_path}
      if [file exists default_ftp]  {file copy default_ftp  $Work_path}
      cd $Work_path
      set Work_path [pwd]
   }

#--------------------------------------------------------
#  no configuration file then try to figure out

} else {

   if { "$tcl_platform(platform)" == "unix" } {

      if [file exists default_init] { 
        set fnams [read_exec_file "default_init" "#Tcl_source_dir" 1]
      } else {
        if [file exists "../guicode/normalfile.tcl"] {
           set fnams "../guicode"
        } else { 
           set fw [open default_init w]
           puts $fw "#Tcl_source_dir" 
           puts $fw $env(PWD)/hysplit/guicode
           close $fw
           puts "Edit default_init file for location of tcl code"
           exit
        }
      }

   } else {

#     When configure is working, will have unix insert a line like
#     set infoScript @infoScript@ 
#     to guarantee soft-linked scripts will find the right spot.
      source [file join [file dirname [info script] ] .. guicode normalfile.tcl]

#     locate absolute directories
      set infoScriptDir [file dirname [info script] ]
      switch [file pathtype $infoScriptDir ] {
         absolute -
         volumerelative {
            set fnams [normalFile  $infoScriptDir ] }
         relative {
            set fnams [normalFile [file join [pwd] $infoScriptDir .. guicode] ] }
         }
   }

#  directory location of tcl GUI source code
#  determines the default hysplit directory structure
   set tcl_dir $fnams

   set fnams [file split $fnams]
   if {[llength $fnams] > 1} {
     set hysplitDirAbs [eval file join [lrange $fnams 0 end-1] ]
   } else {
     set hysplitDirAbs $fnams
   }

#  directory location of help files
   set html_dir [file join $hysplitDirAbs html]

#  directory location of HYSPLIT executables  
   set exec_dir [file join $hysplitDirAbs exec]

#  upper level directory for the HYSPLIT installation 
#  should always be set to the absolute path
   set fnams [pwd]
   set home_dir $hysplitDirAbs
   cd ${home_dir}
   set home_dir [pwd] 
   cd $fnams

#  directory location of meteorological data 
   set Grids_path [file join $hysplitDirAbs working]

#  directory location of map background files 
   set maps_dir [file join $hysplitDirAbs graphics]

#  directory location of fixed boundary files 
   set fixed_dir [file join $hysplitDirAbs bdyfiles]

#  default directory location of output
   set Work_path [file join $hysplitDirAbs working]
   if {! [file exists $Work_path] } {set Work_path [pwd]}

#  clustering directory
   set clus_dir [file join $hysplitDirAbs cluster]

#  --------------------------------------------------
#  default directories for ancillary software

   if { "$tcl_platform(platform)" == "unix" } {

#     directory of Xwindows terminal
      set X_dir ""
      if [file exists /usr/bin/X11/xterm] {
         set X_dir "/usr/bin/X11" }
      if [file exists /usr/X11R6/bin/xterm] {
         set X_dir "/usr/X11R6/bin" }
      if [file exists /usr/X11/bin/xterm] {
         set X_dir "/usr/X11/bin" }
      if [file exists /usr/X/bin/xterm] {
         set X_dir "/usr/X/bin" }
      if [file exists /usr/bin/xterm] {
         set X_dir "/usr/bin" }

#     default directory location of web browser
      set wb_pgm "firefox"
      if [file exists /usr/local/bin/firefox] {set wb_pgm "/usr/local/bin/firefox"}
      if [file exists /usr/bin/firefox] {set wb_pgm "/usr/bin/firefox"}

#     directory location of ImageMagick convert utility
      set magick_pgm "convert"
      if [file exists /usr/local/bin/convert] {set magick_pgm "/usr/local/bin/convert"}
      if [file exists /usr/bin/convert] {set magick_pgm "/usr/bin/convert"}

#     default directory location of Info-ZIP
      set zip_pgm "zip"
      if [file exists /usr/local/bin/zip] {set zip_pgm "/usr/local/bin/zip"}
      if [file exists /usr/bin/zip] {set zip_pgm "/usr/bin/zip"}

#     default directory location of an anaconda3 environment
      set anaconda3_env "$env(HOME)/anaconda3/envs/hysplit"

#     default code-base preferrence for plot programs: FORTRAN or Python
      set PyPreferred false

#     Apple unix system defaults
      if { "$tcl_platform(os)" == "Darwin" } { 
         cd ..
         set home_dir   [pwd]
         set html_dir   [file join [pwd] html] 
         set exec_dir   [file join [pwd] exec]
         set tcl_dir    [file join [pwd] guicode]
         set maps_dir   [file join [pwd] graphics]
         set fixed_dir  [file join [pwd] bdyfiles]
         set clus_dir   [file join [pwd] cluster]
         set Work_path  [file join [pwd] working] 
         set Grids_path [file join [pwd] working]

         set X_dir ""
         if [file exists /opt/X11/bin/xterm] {set X_dir "/opt/X11/bin"}
         set magick_pgm "convert"
         if [file exists /opt/local/bin/convert] {set magick_pgm "/opt/local/bin/convert"}
         set wb_pgm    ""
         if [file exists /opt/local/bin/firefox] {set wb_pgm "/opt/local/bin/firefox"}
         set zip_pgm    ""
         if [file exists /usr/local/zip] {set zip_pgm "/usr/local/zip"}
         if [file exists /usr/bin/zip] {set zip_pgm "/usr/bin/zip"}
         if {![file exists $anaconda3_env]} {set anaconda3_env ""}
      }

#  default WINPC directories ==>
   } else {

#    Xwindows not used in this context
     set X_dir ""

#    Default system directory for installations
     if {[array names env ProgramFiles] == "ProgramFiles"} {
        set temp  [file split $env(ProgramFiles)]
        set drive [lindex $temp 0]
        set prog  [file join $drive [lindex $temp 1] ]
     }

#    define 32-bit and 64-bit directories 

     set temp [string first "(x86)" $prog]
     if { $temp > 0 } {
#       64-bit installation when x86 defined
        set prog32 $prog
        set prog64 [string range $prog 0 [expr $temp-2]]
     } else {
#       when x86 not defined must be 32-bit
        if [file exists "${prog} (x86)"] {
           set prog32 "${prog} (x86)"
        } else {
           set prog32 $prog
        }
        set prog64 $prog
     }

#    -----------------------------------------------------
#    Image Magick

     set temp [glob -nocomplain -types d -directory $prog32 ImageMagick*]
     set PGMDIR [lindex $temp end]
     set magick_pgm [file join $PGMDIR convert.exe]
     if [file exists $magick_pgm] { } else {
        set temp [glob -nocomplain -types d -directory $prog64 ImageMagick*]
        set PGMDIR [lindex $temp end]
        set magick_pgm [file join $PGMDIR convert.exe]
        if [file exists $magick_pgm] { } else {
           set magick_pgm [file join ${drive} ImageMagick convert.exe]}
     }

#    -----------------------------------------------------     
#    Web browser - look for Chrome or Firefox.

     set wb_pgm ""

     proc split_reg_value {value_pattern result} {
        # find a line matching the value_pattern.
        set matched_ln [regexp -inline $value_pattern $result]

        # replace '{', '   ', and '}' with a vertical bar
        set normalized_ln [regsub -all {   +|\{|\}} $matched_ln "|"]

        # separate words delineated by '|'.
        set split_words [split $normalized_ln "|"]

        return $split_words
     }

     proc split_quoted_str {str} {
        set str_list [regexp -inline -all {(?:[^ "]|\"[^"]*\")+} $str]
        return $str_list 
     }

     # Find the default web browser by querying the system registry
     if {[catch {exec reg QUERY "HKCU\\SOFTWARE\\Microsoft\\Windows\\Shell\\Associations\\UrlAssociations\\http\\UserChoice" "/v" "ProgId"} result] == 0} {
        set split_words [split_reg_value "ProgId.*\[^\n\]" $result]
        set prog_id [lindex $split_words 3]
        if {[catch {exec reg QUERY "HKEY_CLASSES_ROOT\\${prog_id}\\shell\\open\\command" "/ve"} result] == 0} {
           set split_words [split_reg_value "REG_SZ.*\[^\n\]" $result]
           set wb_pgm [lindex $split_words 2]
           # get the first word that might be enclosed with double quotes
           set wb_pgm [lindex [split_quoted_str $wb_pgm] 0]
           # remove double quotes
           set wb_pgm [string range $wb_pgm 1 [expr [string length $wb_pgm] - 2]]
        }
     }

     if {[string length $wb_pgm] == 0} {
        # In an unlikely event where the default web browser cannot be located
        # via the system registry.

        set wb_pgm [file join "${prog32}" Google Chrome Application chrome.exe]

        if {![file exists $wb_pgm]} {
           set wb_pgm [file join "${prog64}" Google Chrome Application chrome.exe]
        }

        if {![file exists $wb_pgm]} {
           set wb_pgm [file join "${prog32}" "Mozilla Firefox" firefox.exe]
        }

        if {![file exists $wb_pgm]} {
           set wb_pgm [file join "${prog64}" "Mozilla Firefox" firefox.exe]
        }

        if {![file exists $wb_pgm]} {
           set wb_pgm [file join ${drive} firefox firefox.exe]
        }
     }

#    -----------------------------------------------------     
#    Info-ZIP (tested with version 2.3.1)
     set zip_pgm [file join $hysplitDirAbs exec zip.exe]

#    -----------------------------------------------------     
#    Python
     set local_app_path [file normalize $::env(LOCALAPPDATA)]
     set anaconda3_env "$local_app_path/Continuum/anaconda3/envs/hysplit"
     set PyPreferred false
  }

# if not already there move to working directory for all OS
  cd $Work_path
  set Work_path [pwd]

# file of potential starting locations
  if {! [file exists plants.txt]} {file copy ../guicode/plants.txt plants.txt}
  set start_loc_file "plants.txt"

#---------------------------------------------------------
# write exec file for the first time

  set fw [open "default_exec" w]
  puts $fw "#Starting_locations"
  puts $fw  $start_loc_file 
  puts $fw "#Help_file_dir"
  puts $fw  $html_dir
  puts $fw "#Exec_files_dir"
  puts $fw  $exec_dir
  puts $fw "#Xwindows_dir"
  puts $fw  $X_dir 
  puts $fw "#Image_Magick_pgm"
  puts $fw  $magick_pgm
  puts $fw "#Zip_pgm"
  puts $fw  $zip_pgm 
  puts $fw "#Home_dir"
  puts $fw  $home_dir 
  puts $fw "#Metdata_dir" 
  puts $fw  $Grids_path 
  puts $fw "#Tcl_source_dir" 
  puts $fw  $tcl_dir
  puts $fw "#Mapping_dir" 
  puts $fw  $maps_dir
  puts $fw "#Fixed_files_dir"
  puts $fw  $fixed_dir
  puts $fw "#Cluster_directory"
  puts $fw  $clus_dir 
  puts $fw "#Working_directory"
  puts $fw  $Work_path
  puts $fw "#Anaconda3_env"
  puts $fw  $anaconda3_env
  puts $fw "#PyPreferred"
  puts $fw  $PyPreferred
  puts $fw "#Web_browser_pgm"
  puts $fw  $wb_pgm
  close $fw

#---------------------------------------------------------
# graphics initialization

  if {! [file exists icon63.png]} \
        {file copy ../graphics/icon63.png icon63.png}
  if {! [file exists particle.png]} \
        {file copy ../graphics/particle.png particle.png}
  if {! [file exists particlelegend.png]} \
        {file copy ../graphics/particlelegend.png particlelegend.png}
  if {! [file exists redball.png]} \
        {file copy ../graphics/redball.png redball.png}
  if {! [file exists blueball.png]} \
        {file copy ../graphics/blueball.png blueball.png}
  if {! [file exists greenball.png]} \
        {file copy ../graphics/greenball.png greenball.png}


#---------------------------------------------------------
# write required files for GUI : ASCDATA.CFG                      

  if {! [file exists ASCDATA.CFG]} {
     set fw [open "ASCDATA.CFG" w]
     puts $fw "-90.0   -180.0  lat/lon of lower left corner" 
     puts $fw "1.0     1.0     lat/lon spacing in degrees"
     puts $fw "180     360     lat/lon number of data points"
     puts $fw "2               default land use category"
     puts $fw "0.2             default roughness length (m)"
     puts $fw "'[file join $hysplitDirAbs bdyfiles]/'  directory of files"
     close $fw
  }

#---------------------------------------------------------
# write required files for GUI : sample_traj                      

  if {! [file exists sample_traj]} {
     set fw [open "sample_traj" w]

     puts $fw "00 00 00 00"
     puts $fw "3"
     puts $fw "40.0 -90.0   10.0"
     puts $fw "40.0 -90.0  500.0"
     puts $fw "40.0 -90.0 1000.0"
     puts $fw "12"
     puts $fw "0"
     puts $fw "10000.0"
     puts $fw "1"
     puts $fw "./"
     puts $fw "oct1618.BIN"
     puts $fw "./"
     puts $fw "tdump"
     close $fw
  }
  if {! [file exists default_traj]} {file copy sample_traj default_traj}


#---------------------------------------------------------
# write required files for GUI : sample_conc                   

  if {! [file exists sample_conc]} {
     set fw [open "sample_conc" w]

     puts $fw "00 00 00 00"
     puts $fw "1"
     puts $fw "40.0 -90.0 10.0"
     puts $fw "12"
     puts $fw "0"
     puts $fw "10000.0"
     puts $fw "1"
     puts $fw "./"
     puts $fw "oct1618.BIN"
     puts $fw "1"
     puts $fw "TEST"
     puts $fw "1.0"
     puts $fw "1.0"
     puts $fw "00 00 00 00 00"
     puts $fw "1"
     puts $fw "0.0 0.0"
     puts $fw "0.05 0.05"
     puts $fw "30.0 30.0"
     puts $fw "./"
     puts $fw "cdump"
     puts $fw "1"
     puts $fw "100"
     puts $fw "00 00 00 00 00"
     puts $fw "00 00 00 00 00"
     puts $fw "00 12 00"
     puts $fw "1"
     puts $fw "0.0 0.0 0.0"
     puts $fw "0.0 0.0 0.0 0.0 0.0"
     puts $fw "0.0 0.0 0.0"
     puts $fw "0.0"
     puts $fw "0.0"
     close $fw
  }
  if {! [file exists default_conc]} {file copy sample_conc default_conc}


#--------------------------------------------------------
# cleanup
  if [file exists default_init] {file delete default_init} 

}

#  save and retrieve directory
   set Save_path $Work_path

#--------------------------------------------------------
# load default FTP server locations and directory

  set hostfor 1
  set hostarc 1

  if [file exists default_ftp] { 
     set f [open default_ftp r]
     for {set n 0} {$n <= 3} {incr n} {
         gets  $f  ftpfor($n)
         gets  $f  dirfor($n)
         gets  $f  ftparc($n)
         gets  $f  dirarc($n)
     }
     close $f

  } else {
 
#    active server
     set  ftpfor(0) ftp.arl.noaa.gov
     set  dirfor(0) /pub/forecast
     set  ftparc(0) ftp.arl.noaa.gov
     set  dirarc(0) /pub/archives

#    primary server
     set  ftpfor(1) ftp.arl.noaa.gov
     set  dirfor(1) /pub/forecast
     set  ftparc(1) ftp.arl.noaa.gov
     set  dirarc(1) /pub/archives

#    backup server
     set  ftpfor(2) ftp.arl.noaa.gov
     set  dirfor(2) /pub/forecast
     set  ftparc(2) ftp.arl.noaa.gov
     set  dirarc(2) /pub/archives

#    alternate server (not all files available)
     set  ftpfor(3) ftpprd.ncep.noaa.gov
     set  dirfor(3) /pub/data/nccf/com/hysplit/prod
     set  ftparc(3) ftp.arl.noaa.gov
     set  dirarc(3) /pub/archives

     set f [open default_ftp w]
     for {set n 0} {$n <= 3} {incr n} {
         puts  $f $ftpfor($n)
         puts  $f $dirfor($n)
         puts  $f $ftparc($n)
         puts  $f $dirarc($n)
     }

#    check for installation of FTP package
     puts $f "-------------------------------------------"
     if [catch {package require ftp} rcode] {
        puts $f "FTP: $rcode"
        if [catch {exec teacup install ftp} scode] {
           puts $f "teacup FTP install failed: $scode"
        } else {
           puts $f "$scode"
        }
     } else {
        puts $f "FTP package installed version: $rcode"
     }
     close $f
  }

#---------------------------------------------------------
# tcl html file browser

namespace eval html {
  source [file join $tcl_dir .. guicode htmlbrws.tcl ]
  namespace export load_html
}
namespace import ::html::load_html

#---------------------------------------------------------

option add *background grey85
option add *foreground black
option add *font -*-courier-bold-r-*-*-14-140-*-*-*-*-*-*
ttk::style configure TNotebook.Tab -font -*-courier-bold-r-*-*-14-140-*-*-*-*-*-*
option add *highlightBackground black
option add *Button.background CadetBlue4
option add *activeBackground SeaGreen
set font -*-Helvetica-Bold-R-Normal--*-120-*-*-*-*-*-*

catch {image delete image1a}
image create photo image1a -file $tcl_dir/hylogos.gif
# some unix systems have trouble with gif - replace with bitmap
# image create bitmap image1a -file $tcl_dir/logo.xbm
label .imgwin -image image1a 
pack .imgwin -side top

frame .buttons
pack .buttons -side bottom 

button .buttons.bexit  -text "Exit" -command "exit" -bg red -width 8
button .buttons.help -text "Help" -width 6 \
       -command "load_html [file join $html_dir S000.htm ] "
button .buttons.cont  -text "Menu" -command "Continuebm" -bg green -width 8
pack .buttons.bexit .buttons.help .buttons.cont -side left -padx 6


#---------------------------------------------------------
proc Continuebm {} {
global tcl_dir version
source $tcl_dir/hymenu.tcl
if [winfo exists .imgwin] {destroy .imgwin}
if [winfo exists .buttons] {destroy .buttons}
wm geometry . +0+50
menu_pro
message .msggg -text "  $version  " -width 700 \
  -font -Adobe-times-medium-r-normal--*-140* -bg yellow -pady 10
pack .msggg
}

# ----------------------------------------------------------------
proc positionWindow w {
wm geometry $w +300+300
}

#-----------------------------------------------------------------
proc ftp_box {dat_size} {

global ftp_size

set ww .ftp_win
if [winfo exists $ww] {destroy $ww}

toplevel $ww
wm title $ww "Download Progress"
wm  geometry $ww +500+300

set tot 50
if {$ftp_size > 1000000} {
   set cbyte [expr $dat_size / 1000000]
   set tbyte [expr $ftp_size / 1000000]
   set val   [expr $tot * $cbyte / $tbyte]
} else {
   set val   [expr $tot * $dat_size / $ftp_size]
}

label $ww.totbox -text "$dat_size" -bg yellow -relief sunken -width $tot
label $ww.valbox -bg blue   -relief raised -width $val
label $ww.botbox -text "$ftp_size" -bg yellow -relief sunken -width $tot
pack $ww.totbox $ww.valbox $ww.botbox -side top -anchor w
}

#-----------------------------------------------------------------
proc msg_box {msg} {
global result
if [winfo exists .msg_win] {destroy .msg_win}
set ww .msg_win
toplevel $ww
wm title $ww "Hysplit System Message"
wm  geometry $ww +200+200
message $ww.msgbox -textvariable display_msg -bg honeydew3 \
  -relief raised -aspect 250  -width 900
button $ww.quit  -bg red -text Quit -width 10 -relief raised -command "set result 1; destroy $ww"
button $ww.exit  -bg green -text Continue -width 20 -relief raised -command "set result 0; destroy $ww"
pack $ww.msgbox -side top -padx 2 -pady 4
pack $ww.quit $ww.exit -side left -anchor center -padx 8 -pady 4
set ddddd "$msg"
read_msg $ddddd   
}


#-------------------------------------------------------------------
proc read_msg {msg} {
global display_msg
set display_msg $msg
}


#-------------------------------------------------------------------
proc svg_box {svgfile} {
global X_dir wb_pgm tcl_platform exec_dir

set cur_dir [pwd]
set svg_path [file normalize [file join "${cur_dir}" "${svgfile}"]]

if [file exists $svgfile] {
   if { "$tcl_platform(os)" == "Darwin" } {
#       Apple Mac opens with OS supplied Preview program
        exec open $svgfile

   } else {
#      All other platforms use Web Browswer for display
       if [file exists $wb_pgm] {
          if { "$X_dir" == "" } {
             exec $wb_pgm "file://${svg_path}" &
          } else {
             exec $X_dir/xterm -fn fixed -e $wb_pgm $svgfile & 
          }
       } else { 
          msg_box "Web Browser Required: $wb_pgm"
       }
   }
} else { 
  msg_box "File not found: $svgfile" 
}
}
