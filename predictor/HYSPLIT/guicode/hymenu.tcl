# hymenu.tcl

#-----------------------------------------------------------------------------
# Main Hysplit menu that calls all other applications
# Last Revised: 30 Sep 2002
#               15 May 2003 - particle position display
#               14 Apr 2004 - quick start position viewer
#               18 Aug 2004 - exec directory config option
#               10 Jan 2005 - revised xterm link
#               28 Feb 2005 - more quick start options
#               27 May 2005 - get ncep nomads gdas
#               02 Dec 2005 - MCIP IOAPI
#               23 May 2006 - point source emissions file
#               15 Nov 2006 - ensemble range, ftp addresses
#               04 May 2007 - trajectory frequency plot
#               21 May 2007 - call magick renamed to psc2gif
#               20 Jul 2007 - geolocation option
#               25 Oct 2007 - integrated update procedure
#               27 Nov 2007 - added gridplot to menu
#               07 Dec 2007 - box plot display for ensemble
#               14 Feb 2008 - removed NCEP GRIB1 FTP links
#               15 Oct 2008 - version 4.9 compatibility
#               14 Aug 2009 - trajectory endpoints file listing
#               18 Aug 2009 - select multiple files in proc do_file_list
#               25 Sep 2009 - new gui menus for concadd, labels, datem
#               07 Oct 2009 - test for undefined simulations
#               19 Oct 2009 - added concentration extract menu
#               28 May 2010 - particle shifting option
#               01 Jul 2010 - directory cleanup option
#               05 Dec 2010 - removed link to NCEP NMM egrd3d files
#               14 Jan 2014 - ensemble cleanup script
#               29 Mar 2011 - con2rem for dose
#               03 Nov 2011 - new matrix source statistics (conc_rank.tcl)
#               22 Feb 2012 - ensemble statistical analysis (ens_rank.tcl)
#               15 Aug 2012 - move test meteorology during working cleanup
#               16 Apr 2013 - horizontal concentration smoothing utility
#               25 Jun 2013 - moved matrix solve from display to utilities
#               22 Jul 2013 - rename working file GBL->BIN
#               03 Sep 2014 - added time-varying TCM section
#               25 Nov 2014 - polar concentration grid display
#               23 Jan 2015 - moved ensemble reduction to display menu
#               09 Apr 2015 - restructured Advanced file edit menus
#               25 Jan 2016 - enhanced reset button with more variables
#               06 Jul 2016 - advanced option test_cfg.tcl for hysptest
#               10 Aug 2016 - new cost function minimization for TCM
#               16 Sep 2016 - delete default_?plot files with reset
#               28 Jul 2017 - removed unneeded DATEM variables 
#               18 Aug 2017 - added grid defined test for auto_???? scripts
#               20 Aug 2018 - reset forces all namelist variables to default
#               25 Aug 2018 - removed setpoint option from quick menu
#                           - eliminated FTP and display of TOMS data
#                           - eliminated wincpick concentration display option
#                           - eliminated scan for updates
#               27 Aug 2018 - added data to archive download
#               17 Dec 2019 - add a common script for running plot programs
#               10 Apr 2020 - changed HYSPLIT4 to HYSPLIT
#               07 May 2020 - fix quick start examples.
#               24 Jun 2020 - fix a macOS error when converting WRF-ARW to ARL
#               02 Sep 2020 - added Dscale to initialization
#                           - added inverse option to binary file utility menu
#               03 Sep 2020 - Split probability file creation into its own menu
#               11 May 2021 - changed how Linux/macOS starts Python for plotting
#               27 Jul 2021 - remove ghostscript and ghostview dependencies.
#               02 Jun 2023 - add the RunWatchLog proc.
#-----------------------------------------------------------------------------
# HYSPLIT Executable File Name Convention - hy{c|t}{m|s}_{xxx}
# {c|t} where c=concentration and t=trajectory
# {m|s} where m=multi-processor and s=single-processor
# {xxx} where xxx=compilation-variation
#
# Flag  Old_name   New_name	Description
#
# STD - hymodelc - hycs_std 	concentration single-processor
# MPI - hymodelm - hycm_std	concentration  multi-processor 
# ENS - hymodele - hycm_ens	concentration  multi-processor ensemble  
#     - hymodels - hycs_ens	concentration single-processor ensemble
# VAR - hymodelv - hycs_var	concentration single-processor variance 
# GEM - hymodelg - hycs_gem	concentration single-processor with GEM 
# IER - hysp_ier - hycs_ier	concentration single-processor IER ozone 
# GRS - hysp_grs - hycs_grs	concentration single-processor GRS ozone
# SO2 - hysp_so2 - hycs_so2	concentration single-processor SO2 to SO4 
# CB4 - hysp_cb4 - hycs_cb4	concentration single-processor CB4 ozone
# STD - hymodelt - hyts_std	trajectory single-processor
# MPI - hymodelp - hytm_std	trajectory  multi-processor
# ENS - hymodelf - hyts_ens	trajectory single-processor ensemble            
#-----------------------------------------------------------------------------

# the next line restarts using wish \
exec wish "$0" "$@"

proc menu_pro {} {
global tcl_dir exec_dir exec_dir zip_pgm
global html_dir Grid_dir Grid_name Trajpts_file
global member1 member2

set Grid_name ""
set Trajpts_file ""
set member1 1
set member2 27

set font -*-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-*
option add *highlightBackground black
option add *Menubutton.background CadetBlue4
option add *Button.background CadetBlue4
option add *Checkbutton.background  CadetBlue4
option add *activeBackground Navyblue
option add *activeForeground yellow
option add *Label.background gray85
option add *Entry.background  cornsilk
option add *selectbackground Seagreen

if [winfo exists .tomenu] {destroy .tomenu}
set w .tomenu
catch {destroy $w}
toplevel $w
wm title $w "Menus of Hysplit"
wm iconname $w "To menu Functions"
positionWindow $w
wm geometry $w +1+1

frame $w.menus
frame $w.msg
frame $w.but
pack $w.menus $w.msg $w.but -side top -fill x

label $w.msg.lab -foreground blue -font $font -wraplength 5i -justify left -text \
"                              HYSPLIT \
\n           An integrated system for computing \
\n Trajectories, Air Concentration, and Deposition"
pack $w.msg.lab -side top
set bg "background"
set menu_color "LightCyan1"

#---------------------------------------------------------------
# METEOROLOGICAL DATA CONVERSION MENUS
#---------------------------------------------------------------

set m $w.menus.convert.m
menubutton $w.menus.convert  -text "Meteorology" -menu $m -underline 0 \
-width 18 -relief raised
menu $m -$bg $menu_color

$m add cascade -label "ARL Data FTP " -menu $m.sub2
set m2 [menu $m.sub2 -tearoff 0]
$m2 add command -label "Forecast" -command {ftp_arl F}
$m2 add separator
$m2 add command -label "Appended" -command {ftp_arl A}
$m2 add separator
$m2 add command -label "Archive" -command {ftp_arc}
$m2 add separator
$m2 add command -label "Set Server" -command {ftp_config}
$m add separator

$m add cascade -label "Convert to ARL " -menu $m.sub3
set m3 [menu $m.sub3 -tearoff 0]
$m3 add command -label "WRF-ARW" -command {arch_arw}
$m3 add separator
$m3 add command -label "Global Lat-Lon" -command {arch_ecm}
$m3 add separator
$m3 add command -label "ECMWF ERA" -command {arch_era}
$m3 add separator
$m3 add command -label "User entered" -command {arch_user}
$m add separator

$m add cascade -label "Display Data" -menu $m.sub4
set m4 [menu $m.sub4 -tearoff 0]
$m4 add command -label "Check File" -command {disp_file}
$m4 add separator
$m4 add command -label "Contour Map" -command {disp_map}
$m4 add separator
$m4 add command -label "Text Profile" -command {disp_prof}
$m4 add separator
$m4 add command -label "Grid Domain" -command {disp_grid}
$m add separator

$m add cascade -label "Utilities" -menu $m.sub5
set m5 [menu $m.sub5 -tearoff 0]
$m5 add command -label "GIS to Shapefile" -command {shapefile lines}
$m5 add separator
$m5 add command -label "SVG to Image" -command {convert contour}
$m add separator

$m add command -label "Meteorology Help" \
       -command "load_html [file join $html_dir S140.htm ] "

#---------------------------------------------------------------
# TRAJECTORY MENUS
#---------------------------------------------------------------

set m $w.menus.trajec.m
menubutton $w.menus.trajec  -text "Trajectory" -menu $m -underline 0 \
-width 18 -relief raised
menu $m -$bg $menu_color

$m add cascade -label "Quick Start" -menu $m.sub1
set m1 [menu $m.sub1 -tearoff 0]
$m1 add command -label "Run Example" -command {Traj_quick E}
$m1 add separator
$m1 add command -label "Run Previous" -command {Traj_quick P}
$m1 add separator
$m1 add command -label "Quick Help" \
       -command "load_html [file join $html_dir S261.htm ] "
$m add separator

$m add command -label "Setup Run" -command {Traj_setup}
$m add separator
$m add command -label "Run Model" -command {Traj_run hyts_std S}
$m add separator

$m add cascade -label "Display" -menu $m.sub5
set m5 [menu $m.sub5 -tearoff 0]
$m5 add command -label "Trajectory" -command {Traj_display}
$m5 add separator
$m5 add command -label "Frequency" -command {Traj_grid}
$m  add separator

$m add cascade -label "Utilities" -menu $m.sub2
set m2 [menu $m.sub2 -tearoff 0]
$m2 add command -label "Simple Listing" -command {Traj_file}
$m2 add separator
$m2 add command -label "GIS to Shapefile" -command {shapefile points}
$m2 add separator
$m2 add command -label "Endpoints to IOAPI" -command {traj_api}
$m2 add separator
$m2 add command -label "SVG to Image" -command {convert trajplot}
$m add separator

$m add cascade -label "Special Runs" -menu $m.sub3
set m3 [menu $m.sub3 -tearoff 0]
$m3 add command -label "Test Inputs" -command {Config_test T}
$m3 add separator
$m3 add command -label "Ensemble" -command {Traj_run hyts_ens E}
$m3 add separator
$m3 add command -label "Matrix" -command {Traj_run hyts_std M}
$m3 add separator
$m3 add command -label "Daily" -command {Traj_auto}
$m3 add separator
$m3 add cascade -label "Clustering" -menu $m.sub4
  set m4 [menu $m.sub4 -tearoff 0]
  $m4 add command -label "Example" -command {Clus_analysis E}
  $m4 add separator
  $m4 add command -label "Standard" -command {Clus_analysis S}
$m3 add separator
$m3 add command -label "GeoLocation" -command {Geol_traj}
$m3 add separator
$m3 add command -label "Special Help" \
       -command "load_html [file join $html_dir S253.htm ] "
$m add separator

$m add command -label "Trajectory Help" \
       -command "load_html [file join $html_dir S260.htm ] "

#---------------------------------------------------------------
# CONCENTRATION MENUS
#---------------------------------------------------------------

set m $w.menus.concen.m
menubutton $w.menus.concen  -text "Concentration" -menu $m -underline 0 \
-width 18 -relief raised
menu $m -$bg $menu_color

$m add cascade -label "Quick Start" -menu $m.sub1
set m1 [menu $m.sub1 -tearoff 0]
$m1 add command -label "Run Example" -command {Conc_quick E}
$m1 add separator
$m1 add command -label "Run Previous" -command {Conc_quick P}
$m1 add separator
$m1 add command -label "Quick Help" \
       -command "load_html [file join $html_dir S361.htm ] "
$m add separator

$m add command -label "Setup Run" -command {Conc_setup}
$m add separator
$m add command -label "Run Model" -command {Conc_run hycs_std S}
$m add separator

$m add cascade -label "Display" -menu $m.sub2
set m2 [menu $m.sub2 -tearoff 0]
$m2 add cascade -label "Concentration" -menu $m2.sub0
    set mw [menu $m2.sub0 -tearoff 0]
    $mw add command -label "Contours" -command {Conc_display}
    $mw add separator
    $mw add command -label "Grid Values" -command {Conc_grid}
    $mw add separator
    $mw add command -label "Global Grid" -command {Conc_gem}
    $mw add separator
    $mw add command -label "Polar Grids" -command {Conc_pole}
$m2 add separator
$m2 add command -label "Particle" -command {Part_display}
$m2 add separator
$m2 add command -label "Arrival" -command {Conc_time}
$m2 add separator
$m2 add cascade -label "Source-Receptor" -menu $m2.sub1
    set mx [menu $m2.sub1 -tearoff 0]
    $mx add command -label "View" -command {Matrix_display}
    $mx add separator
    $mx add command -label "Stats" -command {Matrix_stats}
$m2 add separator
$m2 add cascade -label "Ensemble" -menu $m2.sub2
   set my [menu $m2.sub2 -tearoff 0]
   $my add command -label "Create Files" -command {Prob_files}
   $my add separator
   $my add command -label "View Map" -command {Prob_display}
   $my add separator
   $my add command -label "Box Plot" -command {Box_display}
   $my add separator
   $my add command -label "Statistics" -command {rank_ens}
   $my add separator
   $my add command -label "Reduction" -command {ens_reduc}
$m add separator
$m add cascade -label "Utilities" -menu $m.sub3
set m3 [menu $m.sub3 -tearoff 0]
$m3 add command -label "Simple Listing" -command {Conc_file}
$m3 add separator
$m3 add cascade -label "Binary File" -menu $m3.sub1
    set mx [menu $m3.sub1 -tearoff 0]
    $mx add command -label "Merge" -command {Conc_add}
    $mx add separator
    $mx add command -label "Extract" -command {Conc_xtrct}
    $mx add separator
    $mx add command -label "Average" -command {conc_havg}
    $mx add separator
    $mx add command -label "Inverse" -command {conc_invr}
    $mx add separator
    $mx add command -label "Apply Source" -command {tcm2sum}
$m3 add separator
$m3 add cascade -label "Convert to" -menu $m3.sub2
    set my [menu $m3.sub2 -tearoff 0]
    $my add command -label "ASCII" -command {Conc_ascii}
    $my add separator
    $my add command -label "DATEM" -command {conc_datem}
    $my add separator
    $my add command -label "Dose" -command {conc_dose}
    $my add separator
    $my add command -label "IOAPI" -command {conc_api}
    $my add separator
    $my add command -label "Station" -command {Conc_2stn}
$m3 add separator
$m3 add command -label "GIS to Shapefile" -command {shapefile polygons}
$m3 add separator
$m3 add command -label "Particle Adjustment" -command {Part_shift}
$m3 add separator
$m3 add command -label "SVG to Image" -command {convert concplot}
$m3 add separator
$m3 add cascade -label "Transfer Coefficient" -menu $m3.sub3
    set mz [menu $m3.sub3 -tearoff 0]
    $mz add command -label "SVD Solution" -command {Matrix_solve}
    $mz add command -label "Cost Function" -command {Matrix_lbfgsb}
$m add separator

$m add cascade -label "Special Runs" -menu $m.sub4
set m4 [menu $m.sub4 -tearoff 0]
$m4 add command -label "Test Inputs" -command {Config_test C}
$m4 add separator
$m4 add command -label "Daily" -command {Conc_auto}
$m4 add separator
$m4 add command -label "Dust Storm" -command {Conc_run hycs_std D}
$m4 add separator
$m4 add cascade -label "Ensemble" -menu $m4.sub1
    set mx [menu $m4.sub1 -tearoff 0]
    $mx add command -label "Meteorology" -command {Conc_run hycs_ens E}
    $mx add separator
    $mx add command -label "Turbulence" -command {Conc_run hycs_var V}
    $mx add separator
    $mx add command -label "Physics" -command {Conc_run hycs_std P}
    $mx add separator
    $mx add command -label "Cleanup" -command {ens_clean}
$m4 add separator
$m4 add command -label "GeoLocation" -command {Geol_conc}
$m4 add separator
$m4 add command -label "Global" -command {Conc_run hycs_gem G}
$m4 add separator
$m4 add command -label "Matrix" -command {Conc_run hycs_std M}
$m4 add separator
$m4 add command -label "Daughter Products" -command {Daug_conc}
$m4 add separator
$m4 add command -label "Special Help" \
       -command "load_html [file join $html_dir S356.htm ] "
$m add separator

$m add cascade -label "Multi-Processor" -menu $m.sub5
set m5 [menu $m.sub5 -tearoff 0]
$m5 add command -label "Run MPI Model" -command {Set_mpi hycm_std S}
$m5 add separator
$m5 add command -label "Run MPI Matrix" -command {Set_mpi hycm_std M}
$m5 add separator
$m5 add command -label "Run MPI Ensemble" -command {Set_mpi hycm_ens E}
$m5 add separator
$m5 add command -label "MPI Help" \
       -command "load_html [file join $html_dir S358.htm ] "
$m add separator

$m add command -label "Concentration Help" \
       -command "load_html [file join $html_dir S360.htm ] "

#---------------------------------------------------------------
# ADVANCED CONFIGURATION MENUS
#---------------------------------------------------------------

set m $w.menus.config.m
menubutton $w.menus.config  -text "Advanced" -menu $m -underline 0 \
-width 18 -relief raised
menu $m -$bg $menu_color

$m add cascade -label "Configuration Setup" -menu $m.sub1
set m1 [menu $m.sub1 -tearoff 0]
$m1 add command -label "Trajectory" -command {Config_traj}
$m1 add separator
$m1 add command -label "Concentration" -command {Config_conc}
$m1 add separator
$m1 add command -label "Set Directories" -command {Config_exec}
$m1 add separator
$m1 add command -label "Config Help" \
       -command "load_html [file join $html_dir S413.htm ] "
$m add separator

$m add cascade -label "File Edit" -menu $m.sub2
set m2 [menu $m.sub2 -tearoff 0]
$m2 add command -label "Global Module" -command {Config_gem}
$m2 add separator
$m2 add command -label "Dynamic Sampling" -command {Config_lags}
$m2 add separator
$m2 add command -label "Emissions File" -command {Config_locs}
$m2 add separator
$m2 add command -label "Border Labels" -command {Config_labels}
$m2 add separator
$m2 add command -label "Panel Labels" -command {Config_maptxt}
$m2 add separator
$m2 add command -label "FMDV Decay" -command {Config_fmdv}
$m add separator

$m add command -label "View MESSAGES" -command {view_msg}
$m add separator
$m add command -label "Cleanup Working" -command {Config_cleanup}
$m add separator
$m add command -label "Advanced Help" \
       -command "load_html [file join $html_dir S440.htm ] "

#-->final layout

pack $w.menus.convert $w.menus.trajec $w.menus.concen \
$w.menus.config -side left

button $w.but.dismiss  -text Exit  -width 12 -command "exit"
button $w.but.reset  -text Reset  -width 12 -command {reset_all}
button $w.but.help  -text Help  -width 12 \
       -command "load_html [file join $html_dir S100.htm ] "
pack $w.but.dismiss $w.but.reset $w.but.help -side left -anchor center -padx 50
}

#---------------------------------------------------------------
# METEOROLOGY PROCS
#---------------------------------------------------------------

proc ftp_config {} {
global tcl_dir
source $tcl_dir/get_config.tcl
getcfg
}

#---------------------------------

proc ftp_arc {} {
global tcl_dir
source $tcl_dir/get_arc.tcl
get_arc
}

#---------------------------------

proc ftp_arl {Type} {
global tcl_dir
source $tcl_dir/get_arl.tcl
get_arl $Type
}


#---------------------------------

proc arch_arw {} {
global tcl_dir tcl_platform
# 24 JUN 2020 - arw2arl is included in a macOS distribution.
#if { "$tcl_platform(os)" == "Darwin" } {
#  msg_box "NetCDF I/O not available under Mac!"
#} else {
source $tcl_dir/arch_arw.tcl
arw_arch
#}
}

#---------------------------------

proc arch_ecm {} {
global tcl_dir
source $tcl_dir/arch_ecm.tcl
ecm_arch
}

#---------------------------------

proc arch_era {} {
global tcl_dir
source $tcl_dir/arch_era.tcl
era_arch
}

#---------------------------------

proc arch_user {} {
global tcl_dir
source $tcl_dir/arch_user.tcl
user_arch
}


#---------------------------------

proc disp_map {} {
global tcl_dir
source $tcl_dir/disp_map.tcl
map_disp
}

#---------------------------------

proc disp_file {} {
global tcl_dir
source $tcl_dir/disp_file.tcl
file_disp
}

#---------------------------------

proc disp_prof {} {
global tcl_dir
source $tcl_dir/disp_prof.tcl
prof_disp
}

#---------------------------------

proc disp_grid {} {
global tcl_dir
source $tcl_dir/disp_grid.tcl
grid_disp
}


#---------------------------------------------------------------
# TRAJECTORY PROCS
#---------------------------------------------------------------

proc Config_traj {} {
global tcl_dir
  source [file join $tcl_dir traj_cfg.tcl ]
  cfgtraj_init
}

#---------------------------------

proc Traj_setup {} {
global tcl_dir
  source [file join  $tcl_dir traj_setup.tcl ]
  traj_init
}


#---------------------------------

proc reset_all {} {
  global Cinp_file Cnumb Conbase Cscale Dscale Fcst_path Force_name
  global Gembase havrg_file kstat Label Lev name_datem name_plot name_stats Password
  global pinpf Plat polid poutf Psout_file Ptype srm_flist Stnid Tfreq_file Timesum Zero
  global Igis Concapi_file Grid_name
  global tcl_dir
  global cmtfn

  if [file exists SETUP.CFG] {file delete SETUP.CFG}
  if [file exists CONC.CFG]  {file delete CONC.CFG}
  if [file exists TRAJ.CFG]  {file delete TRAJ.CFG}
  if [file exists LABELS.CFG]  {file delete LABELS.CFG}
  if [file exists MAPTEXT.CFG]  {file delete MAPTEXT.CFG}

  file copy -force sample_traj default_traj
  file copy -force sample_conc default_conc

  if [file exists default_cplot]  {file delete default_cplot}
  if [file exists default_tplot]  {file delete default_tplot}

# various scripts test variables shown below for a null field to 
# determine whether variables in those scripts need to be initialized
  set Cinp_file ""
  set Cnumb ""
  set Conbase ""
  set Concapi_file ""
  set Cscale ""
  set Dscale ""
  set Fcst_path ""
  set Force_name ""
  set Gembase ""
  set Grid_name ""
  set havrg_file ""
  set Igis ""
  set kstat ""
  set Label ""
  set Lev ""
  set name_datem ""
  set name_plot ""
  set name_stats ""
  set Password ""
  set pinpf ""
  set Plat ""
  set polid "" 
  set poutf ""
  set Psout_file ""
  set Ptype "" 
  set srm_flist ""
  set Stnid ""
  set Tfreq_file ""
  set Timesum ""
  set Zero ""
  set cmtfn ""

# when pressing reset from the main menu also
# reset all the namelist variables (8/20/2018)

  source [file join  $tcl_dir conc_cfg.tcl ]
  reset_config

  source [file join  $tcl_dir traj_cfg.tcl ]
  reset_config
}

#---------------------------------

proc Traj_quick {type} {
global exec_dir tcl_dir tcl_platform Label
   if { "$type" == "E" } {
      set Label ""
      if [file exists SETUP.CFG] {file delete SETUP.CFG}
      file copy -force sample_traj CONTROL
      file copy -force sample_traj default_traj
   } else {
      file copy -force default_traj CONTROL
   }
   exec $exec_dir/hyts_std

   source [file join  $tcl_dir traj_setup.tcl ]
   init_trajectory notset notset

   source [file join  $tcl_dir traj_disp.tcl ]
   set_default
   run_traj_plot "adhoc"
}

#---------------------------------

proc traj_api {} {
global tcl_dir tcl_platform

if { "$tcl_platform(platform)" == "unix" } {
} else {
   msg_box "NetCDF IOAPI Option not yet available under Windows!"
}
   source $tcl_dir/traj_ioapi.tcl
   traj_ioapi

}

#---------------------------------

proc Geol_traj {} {
global tcl_dir
source $tcl_dir/traj_geol.tcl
traj_geol
}


#---------------------------------

proc Traj_auto {} {
global tcl_dir Trajpts_file
if {$Trajpts_file == ""} {
   msg_box "Grid undefined -- execute setup first!"
   } else {
   source $tcl_dir/auto_traj.tcl
   auto_traj
   }
}

#---------------------------------

proc Traj_run {Model Start} {
global result Trajpts_file X_dir exec_dir result log tcl_platform

if {$Trajpts_file == ""} {
   msg_box "Simulation parameters undefined -- execute setup first!"
   return
}

file copy -force default_traj CONTROL
if [file exists MESSAGE] {file delete MESSAGE}

if [file exists SETUP.CFG] {runcfg}
if [file exists CONTROL] { } else { return }

#  matrix configuration defined as option = M
if {"$Start" == "M"} {
   msg_box "Three sources configured as matrix?"
   tkwait window .msg_win
   if {$result != 0} {
      return
   } else {

   if { "$tcl_platform(platform)" == "unix" } {
      if { "$X_dir" == "" } {
      exec $exec_dir/latlon
      } else {
      exec $X_dir/xterm -fn fixed -e $exec_dir/latlon
      }
   } else {
      exec $exec_dir/latlon.exe
   }
   if [file exists CONTROL] { } else {return}
   }
}

# ensemble calculation
if {"$Start" == "E"} {
   msg_box "Ensemble trajectory calculation?"
   tkwait window .msg_win
   if {$result != 0} {return}
}

if { "$tcl_platform(platform)" == "unix" } {
   set code "${exec_dir}/${Model}"
   set xops "|$code |& cat"
   } else {
   set code ${exec_dir}/${Model}.exe
   set xops "|$code"
   }

if [file exists $code] {

   set log [ScrollText .f]
   $log configure -cursor watch
   $log insert end "Model started ...\n"
   update

   if [catch {open $xops} result] {
      $log insert end $result
      } else {
      fileevent $result readable Log
      }

   } else {
   msg_box "Trajectory executable $Model not available!"
   }
}

#-------------------------------

proc Traj_display {} {
global tcl_dir Trajpts_file
if {$Trajpts_file == ""} {
   msg_box "Grid undefined -- execute setup first!"
   } else {
   source $tcl_dir/traj_disp.tcl
   traj_plot
   }
}

#-------------------------------

proc Traj_file {} {
global Trajpts_file

if {$Trajpts_file == ""} {
      msg_box "Grid undefined -- execute setup first!"
   } else {
      set log [ScrollText .f]
      $log insert end "TRAJECTORY ENDPOINTS FILE LISTING ...\n"
      set fileid [open $Trajpts_file r]
      while {[eof $fileid] != 1} {
         gets $fileid cline
         $log insert end $cline\n
      }
   close $fileid
   }
}

#-------------------------------

proc Traj_grid {} {
global tcl_dir 
  source $tcl_dir/traj_freq.tcl
  traj_freq
}

#---------------------------------

proc Clus_analysis {type} {
global tcl_dir
  source [file join  $tcl_dir trajclus_run.tcl ]
  traj_clus_analysis $type
}

#---------------------------------------------------------------
# PARTICLE PROCS
#---------------------------------------------------------------

proc Part_display {} {
global tcl_dir

source $tcl_dir/disp_part.tcl
part_plot

}

#---------------------------------------------------------------
# CONCENTRATION PROCS
#---------------------------------------------------------------

proc Conc_setup {} {
global tcl_dir
source $tcl_dir/conc_setup.tcl
conc_init
}

#------------------------------

proc Config_conc {} {
global tcl_dir
source $tcl_dir/conc_cfg.tcl
cfgconc_init
}

#------------------------------

proc Config_gem {} {
global tcl_dir
source $tcl_dir/gem_cfg.tcl
gem_cfg
}

#-------------------------------

proc Conc_quick {type} {
global exec_dir tcl_dir tcl_platform Lev Fixed
   if { "$type" == "E" } {
      set Lev ""
      if [file exists SETUP.CFG] {file delete SETUP.CFG}
      file copy -force sample_conc CONTROL
      file copy -force sample_conc default_conc
   } else {
      file copy -force default_conc CONTROL
   }
   exec $exec_dir/hycs_std

   source [file join  $tcl_dir conc_setup.tcl ]
   init_concentration {1}

   source [file join  $tcl_dir conc_disp.tcl ]
   set_defaultc
   if { "$type" == "E" } {set Fixed 1}
   run_conc_plot "adhoc"
}

#---------------------------------

proc Conc_auto {} {
global tcl_dir Grid_name

if {$Grid_name == ""} {
   msg_box "Grid undefined -- execute setup first!"
   } else {
   source $tcl_dir/auto_conc.tcl
   auto_conc
   }
}


#---------------------------------

proc Geol_conc {} {
global tcl_dir
source $tcl_dir/conc_geol.tcl
conc_geol
}

#---------------------------------

proc Daug_conc {} {
global tcl_dir Grid_name
if {$Grid_name == ""} {
   msg_box "Grid undefined -- execute setup first!"
   } else {
source $tcl_dir/conc_daug.tcl
conc_daug
}
}
#---------------------------------

proc ens_clean {} {
global tcl_dir
source $tcl_dir/cleanup.tcl
cleanup
}


#---------------------------------

proc ens_reduc {} {
global tcl_dir
source $tcl_dir/ens_reduc.tcl
ens_reduc
}


#---------------------------------

proc rank_ens {} {
global tcl_dir Grid_name
if {$Grid_name == ""} {
   msg_box "Grid undefined -- execute setup first!"
   } else {
   source $tcl_dir/ens_rank.tcl
   ens_rank
}
}


#---------------------------------

proc Conc_run {Model Start} {
global result member1 member2 Grid_name 
global X_dir exec_dir exec_dir result log tcl_platform tcl_dir

if {$Grid_name == ""} {
   msg_box "Simulation parameters undefined -- execute setup first!"
   return   
}
 
file copy -force default_conc CONTROL
if [file exists MESSAGE] {file delete MESSAGE}

if [file exists SETUP.CFG] {runcfg}
if [file exists CONTROL] { } else { return }

# dust storm PM10 configuration defined as option = D
if {"$Start" == "D"} {
   msg_box "Three point source area defined?\nAdvanced menu configured for dust?"
   tkwait window .msg_win
   if {$result != 0} {
      return
   } else {

   if { "$tcl_platform(platform)" == "unix" } {
      if { "$X_dir" == "" } {
      exec $exec_dir/dustbdy
      } else {
      exec $X_dir/xterm -fn fixed -e $exec_dir/dustbdy
      }
   } else {
      exec $exec_dir/dustbdy.exe
   }
   if [file exists CONTROL] { } else {return}
   }
}

# matrix configuration defined as option = M
if {"$Start" == "M"} {
   msg_box "Three sources configured for simple or source-receptor matrix?"
   tkwait window .msg_win
   if {$result != 0} {
      return
   } else {

   if { "$tcl_platform(platform)" == "unix" } {
      if { "$X_dir" == "" } {
      exec $exec_dir/latlon
      } else {
      exec $X_dir/xterm -fn fixed -e $exec_dir/latlon
      }
   } else {
      exec $exec_dir/latlon.exe
   }
   if [file exists CONTROL] { } else {return}
   }
}

# single processor ensemble option = E
if {"$Start" == "E"} {
   msg_box "Start meteorological ensemble calculation?"
   tkwait window .msg_win
   if {$result != 0} {
      return
   } else {

   set log [ScrollText .f]
   $log configure -cursor watch
   $log insert end "Ensemble calculation started ...\n"
   update  

   for {set run $member1} {$run <= $member2} {incr run} {
      if { "$tcl_platform(platform)" == "unix" } {
         exec ${exec_dir}/${Model} $run
      } else {
         exec ${exec_dir}/${Model}.exe $run
      }
      $log insert end "Finished member: $run \n"
      update
   }
   msg_box " Meteorology ensemble run completed! "
   return
   }
}

# single processor variance option = V
if {"$Start" == "V"} {
   msg_box "Start turbulence ensemble calculation?"
   tkwait window .msg_win
   if {$result != 0} {
      return
   } else {

   set log [ScrollText .f]
   $log configure -cursor watch
   $log insert end "Variance calculation started ...\n"
   update  

   for {set run $member1} {$run <= $member2} {incr run} {
      if { "$tcl_platform(platform)" == "unix" } {
         exec ${exec_dir}/${Model} $run
      } else {
         exec ${exec_dir}/${Model}.exe $run
      }
      $log insert end "Finished member: $run \n"
      update
   }
   msg_box " Variance ensemble run completed! "
   return
   }
}

# physics ensemble = P
if {"$Start" == "P"} {
   source $tcl_dir/conc_phys.tcl
   conc_phys
   return
}

# global routines = G
if {"$Start" == "G"} {
   msg_box "Advanced menu configured for Global Eulerian Model?"
   tkwait window .msg_win
   if {$result != 0} {return}
}

# standard simulation
if { "$tcl_platform(platform)" == "unix" } {
   set code "${exec_dir}/${Model}"
   set xops "|$code |& cat"
   } else {
   set code ${exec_dir}/${Model}.exe
   set xops "|$code"
   }

if [file exists $code] {
   set log [ScrollText .f]
   $log configure -cursor watch
   $log insert end "Model started ...\n"
   update  

   if [catch {open $xops} result] {
      $log insert end  $result
      } else {
      fileevent $result readable Log
      }

   } else {
   msg_box "Concentration executable for $Model not available!"
   }
}

#-----------------------------

proc Conc_display {} {
global tcl_dir Grid_name
if {$Grid_name == ""} {
   msg_box "Grid undefined -- execute setup first!"
   } else {
   source $tcl_dir/conc_disp.tcl
   conc_plot
   }
}

#-------------------------------

proc Conc_file {} {
global tcl_dir
source $tcl_dir/conc_file.tcl
conc_file
}

#-------------------------------

proc Conc_ascii {} {
global tcl_dir Grid_name
if {$Grid_name == ""} {
   msg_box "Grid undefined -- execute setup first!"
   } else {
   source $tcl_dir/conc_asc.tcl
   conc_2asc
   }
}

#-------------------------------

proc Conc_2stn {} {
global tcl_dir Grid_name
if {$Grid_name == ""} {
   msg_box "Grid undefined -- execute setup first!"
   } else {
   source $tcl_dir/conc_stn.tcl
   conc_2stn
   }
}

#---------------------------------

proc conc_api {} {
global tcl_dir tcl_platform Grid_name
if {$Grid_name == ""} {
   msg_box "Grid undefined -- execute setup first!"
   } else {
      if { "$tcl_platform(platform)" == "unix" } {
      } else {
         msg_box "NetCDF IOAPI Option not yet available under Windows!"
      }
         source $tcl_dir/conc_ioapi.tcl
         conc_ioapi
   }
}

#--------------------------------

proc Conc_grid {} {
global tcl_dir Grid_name
if {$Grid_name == ""} {
   msg_box "Grid undefined -- execute setup first!"
   } else {
   source $tcl_dir/conc_grid.tcl
   conc_grid
   }
}

#--------------------------------

proc Conc_gem {} {
global tcl_dir Grid_name
if {$Grid_name == ""} {
   msg_box "Grid undefined -- execute setup first!"
   } else {
   source $tcl_dir/gem_disp.tcl
   gem_disp
   }
}

#--------------------------------

proc Conc_pole {} {
global tcl_dir Grid_name
if {$Grid_name == ""} {
   msg_box "Grid undefined -- execute setup first!"
   } else {
   source $tcl_dir/pole_disp.tcl
   pole_disp
   }
}

#--------------------------------

proc Conc_time {} {
global tcl_dir Grid_name
if {$Grid_name == ""} {
   msg_box "Grid undefined -- execute setup first!"
   } else {
   source $tcl_dir/conc_time.tcl
   conc_time
   }
}

#--------------------------------

proc Conc_add {} {
global tcl_dir
source $tcl_dir/conc_add.tcl
conc_add
}

#--------------------------------

proc conc_datem {} {
global tcl_dir Grid_name
if {$Grid_name == ""} {
   msg_box "Grid undefined -- execute setup first!"
   } else {
   source $tcl_dir/datem.tcl
   datem
}
}

#--------------------------------

proc conc_dose {} {
global tcl_dir Grid_name
if {$Grid_name == ""} {
   msg_box "Grid undefined -- execute setup first!"
   } else {
   source $tcl_dir/con2rem.tcl
   con2rem
}
}

#--------------------------------

proc conc_havg {} {
global tcl_dir Grid_name
if {$Grid_name == ""} {
   msg_box "Grid undefined -- execute setup first!"
   } else {
   source $tcl_dir/conc_havg.tcl
   conhavg
}
}

#--------------------------------

proc conc_invr {} {
global tcl_dir Grid_name
if {$Grid_name == ""} {
   msg_box "Grid undefined -- execute setup first!"
   } else {
   source $tcl_dir/conc_invr.tcl
   coninvr
}
}

#-------------------------------

proc Conc_xtrct {} {
global tcl_dir
source $tcl_dir/conc_xtrct.tcl
conc_xtrct
}

#-------------------------------

proc Part_shift {} {
global tcl_dir
source $tcl_dir/par_shift.tcl
par_shift
}


#---------------------------------------------------------------
# SPECIAL SIMULATION PROCS
#---------------------------------------------------------------

proc Set_mpi {prog prep} {

global result tcl_platform tcl_dir
global Nproc Model Start

if [ info exists Nproc ] { } else {set Nproc ""}

if { "$tcl_platform(platform)" == "unix" } {
   if { $Nproc == "" } {set Nproc 2}
   set Model $prog
   set Start $prep 
   source $tcl_dir/conc_mpi.tcl
   conc_mpi 
} else {
   msg_box "MPI Option for $prog not available under Windows!"
   tkwait window .msg_win
   if {$result != 0} {
      return
   } else {
      if { $Nproc == "" } {set Nproc 1}
      set Model $prog
      set Start $prep 
      source $tcl_dir/conc_mpi.tcl
      conc_mpi       
   }
}

}

#----------------------------------

proc Prob_display {} {
global tcl_dir 
if [file exists prob50] {
   source $tcl_dir/disp_prob.tcl
   prob_plot
   } else {
   msg_box "Probability files not found ... run ensemble create files first!"
   }
}

#----------------------------------

proc Prob_files {} {
global tcl_dir Grid_name
if {$Grid_name == ""} {
   msg_box "Grid undefined -- execute setup first!"
   } else {
   source $tcl_dir/prob_files.tcl
   probfiles
   }
}

#----------------------------------

proc Box_display {} {
global tcl_dir
if [file exists prob50] {
   source $tcl_dir/disp_boxp.tcl
   boxp_disp
   } else {
   msg_box "Probability files not found ... run ensemble create files first!"
   }
}


#------------------------------------

proc Matrix_display {} {
global tcl_dir Grid_name
if {$Grid_name == ""} {
   msg_box "Grid undefined -- execute setup first!"
   } else {
   source $tcl_dir/srm_disp.tcl
   srm_plot
   }
}


#------------------------------------

proc Matrix_solve {} {
global tcl_dir Grid_name
if {$Grid_name == ""} {
   msg_box "Grid undefined -- execute setup first!"
   } else {
   source $tcl_dir/srm_solve.tcl
   srm_solve
   }
}

#------------------------------------

proc Matrix_lbfgsb {} {
global tcl_dir Grid_name
if {$Grid_name == ""} {
   msg_box "Grid undefined -- execute setup first!"
   } else {
   source $tcl_dir/srm_lbfgsb.tcl
   srm_lbfgsb
   }
}


#------------------------------------

proc Matrix_stats {} {
global tcl_dir Grid_name
if {$Grid_name == ""} {
   msg_box "Grid undefined -- execute setup first!"
   } else {
   source $tcl_dir/conc_rank.tcl
   conc_rank
   }
}


#-------------------------------

proc tcm2sum {} {
global tcl_dir Grid_name
if {$Grid_name == ""} {
   msg_box "Grid undefined -- execute setup first!"
   } else {
   source $tcl_dir/srm_sum.tcl
   srm2sum
   }
}



#---------------------------------------------------------------
# ADVANCED PROCS
#---------------------------------------------------------------

proc Config_cleanup {} {
 global Work_path result
 set dnumb [format %03.0f [expr {rand()*1000}]]

 msg_box "Rename $Work_path to $Work_path.${dnumb}?"
 tkwait window .msg_win
 if {$result != 0} {
    return
 } else {
    cd ..
    file rename  $Work_path ${Work_path}.${dnumb}
    file mkdir $Work_path

    if [ file exists ${Work_path}.${dnumb}/oct1618.BIN] {
       file rename ${Work_path}.${dnumb}/oct1618.BIN $Work_path} 
    if [ file exists ${Work_path}.${dnumb}/oct1718.BIN] {
       file rename ${Work_path}.${dnumb}/oct1718.BIN $Work_path} 

    exit
 }
}

#---------------------------------

proc Config_lags {} {
global tcl_dir
source $tcl_dir/lags_cfg.tcl
cfg_lags
}

#---------------------------------

proc Config_test {mdl} {
global Trajpts_file Grid_name tcl_dir Mtype

set Mtype $mdl

if {"$Mtype" == "T"} {
   if {$Trajpts_file == ""} {
      msg_box "Simulation parameters undefined -- execute setup first!"
      return
   }
    file copy -force default_traj CONTROL

} else {
   if {$Grid_name == ""} {
      msg_box "Simulation parameters undefined -- execute setup first!"
      return   
   }
   file copy -force default_conc CONTROL
}

source $tcl_dir/test_cfg.tcl
cfg_test
}

#--------------------------------- 

proc Config_locs {} {
global tcl_dir
source $tcl_dir/emit_cfg.tcl
cfg_emit
}

#---------------------------------

proc Config_exec {} {
global tcl_dir
source $tcl_dir/exec_cfg.tcl
exec_cfg
}

#---------------------------------

proc Config_maptxt {} {
global tcl_dir
source $tcl_dir/disp_txt.tcl
disp_txt
}

#---------------------------------

proc Config_labels {} {
global tcl_dir
source $tcl_dir/disp_lab.tcl
disp_lab
}

#---------------------------------

proc Config_fmdv {} {
global tcl_dir
source $tcl_dir/edit_fmdv.tcl
edit_fmdv
}

#----------------------------------

proc view_msg {} {
global log
set log [ScrollText .f]
$log insert end "MESSAGE FILE LISTING ...\n"
   if [file exists MESSAGE] {
      set fileid [open "MESSAGE" r]
      while {[eof $fileid] != 1} {
         gets $fileid cline
         $log insert end $cline\n
      }
      close $fileid
   }
}


#---------------------------------------------------------------
# COMMON PROCS
#---------------------------------------------------------------

proc Log {} {
global result log pipe_closed
if [eof $result] {
   catch {close $result}
   $log configure -cursor arrow
   set pipe_closed 1
   bell
} else {
   gets $result line
   $log insert end $line\n
   $log see end
}
}

proc RunWatchLog {cmd prog} {
   global X_dir tcl_dir exec_dir tcl_platform
   global result log pipe_closed

   if { "$tcl_platform(platform)" == "unix" } {
      set xops "|$cmd |& cat"
   } else {
      set xops "|$cmd"
   }

   set log [ScrollText .f]
   set frm [winfo parent $log]
   set fw  [winfo parent $frm]
   wm title $fw "Log"
   $log configure -width 80
   $log configure -cursor watch
   $log insert end "Program $prog started ...\n\n"
   update
   set pipe_closed 0
   if [catch {open $xops} result] {
      $log insert end $result
   } else {
      fileevent $result readable Log
   }
   vwait pipe_closed
}

#----------------------------------

proc ScrolledCanvas {f args} {
frame $f
  eval {canvas $f.c \
     -yscrollcommand [list $f.sy set] \
     -xscrollcommand [list $f.sx set] } $args
  scrollbar $f.sy -command [list $f.c yview]
  scrollbar $f.sx -orient horizontal -command [list $f.c xview]
  grid $f.c $f.sy -sticky news
  grid $f.sx -sticky ew
  grid rowconfigure $f 0 -weight 1
  grid columnconfigure $f 0 -weight 1
}

#- canv_xy: convert coordinates from displayed window to coords on canvas ---#
proc canv_xy {canv x y } {
  return [list [$canv canvasx $x] [$canv canvasy $y] ]
}
#----------- end canv_xy ----------------------------------------------------#

#
proc ScrollText {fw} {

   if [winfo exists $fw ] {destroy $fw} ;# avoids window conflicts
   toplevel $fw
   wm title $fw "SIMULATION LOG"                        
   set f $fw.f
   frame $f
   focus $f
   text $f.text -width 60 -height 15 -setgrid true -wrap none \
      -yscrollcommand [list $f.yscroll set] \
      -xscrollcommand [list $f.xscroll set]
   scrollbar $f.yscroll -orient vertical   -command [list $f.text yview]
   scrollbar $f.xscroll -orient horizontal -command [list $f.text xview]
   button $f.dismiss -text Exit  -width 59 -command "destroy $fw"

   pack $f -expand yes -fill both
   pack $f.yscroll -side right -fill y
   pack $f.xscroll -side bottom -fill x
   pack $f.text -expand yes -fill both
   pack $f.dismiss -side left -expand yes -fill x
   return $f.text
}

#---------------------------------

proc do_file_list {wf wd} {
global Grids_path Grid_number Work_path

# disabled 9/24/2009 to save value of Grids_path from default_exec
# if { [ catch {set dummy $Grids_path} ] } {
#    set Grids_path $Work_path
#    set Grid_number 0
#    }

set tmp [tk_getOpenFile -multiple 1 -title "File Selection" -initialdir $Grids_path]

if [string length $tmp] {
   foreach i $tmp {
      $wf insert end [file tail $i]
      $wd insert end [file dirname $i]
      incr Grid_number }
   }
}

#---------------------------------

proc convert { {base_file myfile} args} {
global tcl_dir Psout_file
source $tcl_dir/svg2gif.tcl

if [ info exists Psout_file ] {
   svg2gif $Psout_file
} else { 
   svg2gif $base_file
}
}

#---------------------------------

proc shapefile {type} {
global tcl_dir Psout_file Shape_file
source $tcl_dir/asc2shp.tcl

if [ info exists Psout_file ] {
   set Shape_file $Psout_file
   asc2shp $type
} else {
   switch $type {
      points   {set Shape_file traj_sh01}
      lines    {set Shape_file metv_sh01}
      polygons {set Shape_file conc_sh01}
   }
   asc2shp $type
}
}


#------------------------------------------------------------------------------

proc runcfg {} {
global tcl_dir
source $tcl_dir/run_cfg.tcl
run_cfg
}

#------------------------------------------------------------------------------

proc run_python_program {pyscript clargs} {
global X_dir exec_dir result log tcl_platform pipe_closed anaconda3_env

if {! [file isdirectory $anaconda3_env] } {
   msg_box "The hysplit anaconda3 environment is not found!
 
To use python graphics:
1) Install anaconda3 to manage a python environment.
2) Create an anaconda3 environment named hysplit.
3) Install required python packages in the hysplit environment:
   see the README file in the python directory of this hysplit
   distribution.
4) Set path to your anaconda3 hysplit environment at Advanced /
   Configuration Setup / Set Directories / Anaconda3 environment
   path."
   return
}

if { "$tcl_platform(platform)" == "unix" } {
   # activate the hysplit anaconda3 environment and execute python.exe
   set fh [open "pyhook.sh" w]
   puts $fh "#!/bin/bash"
   puts $fh "source \"$anaconda3_env/../../etc/profile.d/conda.sh\""
   puts $fh "\"$anaconda3_env/bin/python\" \"$pyscript\" $clargs"
   close $fh
   file attributes "pyhook.sh" -permissions a+x
   set xops "| ./pyhook.sh"
   set code "$pyscript"
} elseif { "$tcl_platform(platform)" == "windows" } {
   # activate the hysplit anaconda3 environment and execute python.exe
   set fh [open "pyhook.bat" w]
   puts $fh "\"$anaconda3_env/../../Scripts/activate.bat\" \"$anaconda3_env\" && python.exe \"$pyscript\" $clargs"
   close $fh
   set xops "| pyhook.bat"
   set code "$pyscript"
} else {
   set xops "| $anaconda3_env/bin/python \"$pyscript\" $clargs"
   set code "$pyscript"
}

if [file exists $code] {
   set log [ScrollText .f]                         

   set frm [winfo parent $log]
   set fw  [winfo parent $frm]
   wm title $fw "Log"
   $log configure -width 120

   $log configure -cursor watch
   set pipe_closed 0
   if [catch {open $xops} result] {
      $log insert end $result
      if {[lindex $::errorCode 0] ne "NONE"} {
         msg_box "Unexpected plot failure with error code $::errorCode"
      }
   } else {
      fileevent $result readable Log
   }
   vwait pipe_closed
} else {
   msg_box "Python executable $code not available!"
}
} 
