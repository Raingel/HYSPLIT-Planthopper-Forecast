proc exec_cfg { } {

#-----------------------------------------------------------------------------
# EXEC_CFG.TCL: updates the default_exec file with directory locations
# Last Revised: 18 Aug 2004 - initial version
#               09 Jun 2006 - added help
#               25 Sep 2006 - base directory
#               03 Nov 2008 - simplified structure
#               06 Jan 2008 - permit dynamic change of working directory
#               30 Jun 2010 - replaced windows cmd.exe with tcl command
#               21 Jul 2021 - add wb_pgm for web browser program
#               27 Jul 2021 - remove ghostscript and ghostview dependencies.
#-----------------------------------------------------------------------------

global start_loc_file html_dir exec_dir clus_dir
global X_dir magick_pgm home_dir Grids_path zip_pgm
global tcl_dir maps_dir fixed_dir Work_path Temp_path
global anaconda3_env wb_pgm

if [winfo exists .configexec] {destroy .configexec}
set wr .configexec
toplevel $wr
wm title $wr "Directory and Executables Default Locations"
wm  geometry $wr +50+50

set Temp_path $Work_path

frame $wr.info
frame $wr.home
frame $wr.webbrowser
frame $wr.magick
frame $wr.zip 
frame $wr.exec 
frame $wr.tcltk
frame $wr.clus
frame $wr.xwin 
frame $wr.metd
frame $wr.maps
frame $wr.fixed
frame $wr.help   
frame $wr.work  
frame $wr.start
frame $wr.python
frame $wr.exit  

pack $wr.info $wr.home $wr.webbrowser $wr.magick \
     $wr.zip $wr.exec $wr.tcltk $wr.clus $wr.xwin $wr.metd \
     $wr.maps $wr.fixed $wr.help $wr.work $wr.start $wr.python \
     $wr.exit -side top -pady 5

label $wr.info.lab -fg blue -relief raised -justify left -wraplength 8i \
 -text "Edits the default_exec file that defines the GUI directory structure\
 and location of executables. If not found, the file is created automatically\
 during startup."
pack $wr.info.lab

label $wr.home.lab -text    "HYSPLIT programs directory:     "
entry $wr.home.ent -textvariable home_dir -width 40
pack $wr.home.lab $wr.home.ent -side left

label $wr.webbrowser.lab -text "Web browser path/executable:    "
entry $wr.webbrowser.ent -textvariable wb_pgm -width 40
pack $wr.webbrowser.lab $wr.webbrowser.ent -side left 

label $wr.magick.lab -text  "ImageMagick path/executable:    "
entry $wr.magick.ent -textvariable magick_pgm -width 40
pack $wr.magick.lab $wr.magick.ent -side left

label $wr.zip.lab -text     "Zip program path/executable:    "
entry $wr.zip.ent -textvariable zip_pgm -width 40
pack $wr.zip.lab $wr.zip.ent -side left

label $wr.exec.lab -text    "Executable programs directory:  "
entry $wr.exec.ent -textvariable exec_dir -width 40
pack $wr.exec.lab $wr.exec.ent -side left

label $wr.tcltk.lab -text   "Tcl/Tk GUI script directory:    "
entry $wr.tcltk.ent -textvariable tcl_dir -width 40
pack $wr.tcltk.lab $wr.tcltk.ent -side left 

label $wr.clus.lab -text    "Clustering directory:           "
entry $wr.clus.ent -textvariable clus_dir -width 40
pack $wr.clus.lab $wr.clus.ent -side left

label $wr.xwin.lab -text    "UNIX X-windows directory:       "
entry $wr.xwin.ent -textvariable X_dir -width 40
pack $wr.xwin.lab $wr.xwin.ent -side left

label $wr.metd.lab -text    "Meteorological data directory:  "
entry $wr.metd.ent -textvariable Grids_path -width 40
pack $wr.metd.lab $wr.metd.ent -side left

label $wr.maps.lab -text    "Map Background directory:       "
entry $wr.maps.ent -textvariable maps_dir -width 40
pack $wr.maps.lab $wr.maps.ent -side left

label $wr.fixed.lab -text   "Fixed files directory:          "
entry $wr.fixed.ent -textvariable fixed_dir -width 40
pack $wr.fixed.lab $wr.fixed.ent -side left

label $wr.help.lab -text    "Help files directory:           "
entry $wr.help.ent -textvariable html_dir -width 40
pack $wr.help.lab $wr.help.ent -side left

label $wr.work.lab -text    "Working directory:              "
entry $wr.work.ent -textvariable Temp_path -width 40
pack $wr.work.lab $wr.work.ent -side left

label $wr.start.lab -text   "Starting locations file:        "
entry $wr.start.ent -textvariable start_loc_file -width 40
pack $wr.start.lab $wr.start.ent -side left

label $wr.python.lab -text  "Anaconda3 environment path:     "
entry $wr.python.ent -textvariable anaconda3_env -width 40
pack $wr.python.lab $wr.python.ent -side left

button $wr.exit.quit  -bg red -text Quit -width 12 -command "destroy $wr"
button $wr.exit.help -text Help -width 14 \
       -command "load_html [file join $html_dir S416.htm ] "
button $wr.exit.done  -bg green -text "Update file: default_exec"  \
      -width 40 -command "config_done $wr"
pack $wr.exit.quit $wr.exit.help $wr.exit.done -side left -padx 10
}

#-----------------------------------------------------------------------------
proc config_done {wr} {

global start_loc_file html_dir exec_dir clus_dir
global X_dir magick_pgm home_dir Grids_path zip_pgm tcl_platform
global tcl_dir maps_dir fixed_dir Work_path Temp_path
global anaconda3_env wb_pgm

if { "$Work_path" != "$Temp_path" } {
if {! [file exists $Temp_path]} {
   file mkdir $Temp_path
   if [file exists ASCDATA.CFG]  {file copy ASCDATA.CFG  $Temp_path}
   if [file exists default_conc] {file copy default_conc $Temp_path}
   if [file exists default_traj] {file copy default_traj $Temp_path}
   if [file exists default_exec] {file copy default_exec $Temp_path}
   if [file exists default_ftp]  {file copy default_ftp  $Temp_path}
}
}
set Work_path $Temp_path
cd $Work_path

set fw [open "default_exec" w]
puts $fw "#Starting_locations"
puts $fw    $start_loc_file
puts $fw "#Help_file_dir"
puts $fw    $html_dir
puts $fw "#Exec_files_dir"
puts $fw    $exec_dir
puts $fw "#Xwindows_dir"
puts $fw    $X_dir
puts $fw "#Image_Magick_pgm"
puts $fw    $magick_pgm
puts $fw "#Zip_pgm"
puts $fw    $zip_pgm
puts $fw "#Home_dir"
puts $fw    $home_dir
puts $fw "#Metdata_dir"
puts $fw    $Grids_path
puts $fw "#Tcl_source_dir"
puts $fw    $tcl_dir
puts $fw "#Mapping_dir"
puts $fw    $maps_dir
puts $fw "#Fixed_files_dir"
puts $fw    $fixed_dir
puts $fw "#Cluster_directory"
puts $fw    $clus_dir
puts $fw "#Working_directory"
puts $fw    $Work_path
puts $fw "#Anaconda3_env"
puts $fw    $anaconda3_env
puts $fw "#Web_browser_pgm"
puts $fw    $wb_pgm
close $fw
destroy $wr
}
