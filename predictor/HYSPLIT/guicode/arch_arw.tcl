proc arw_arch {} {

#-----------------------------------------------------------------------------
# ARCH_ARW.TCL: Convert pre-existing NetCDF ARW-WRF output file to ARL format
# Last Revised: 15 Oct 2008 - initial version from MM5
#               21 Aug 2009 - trap standard output
#-----------------------------------------------------------------------------

global html_dir Grid_number 
global Year Month Fcst_path Fcst_file
if [winfo exists .arwarch] {destroy .arwarch}
set wr .arwarch 
toplevel $wr
wm title $wr " Convert WRF-ARW output file to ARL format "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.data
frame $wr.mid5
frame $wr.mid6
frame $wr.bot
pack $wr.top -pady 4 -padx 2 
pack $wr.data -pady 2
pack $wr.mid5 $wr.mid6 -side top -pady 6 -anchor w
pack $wr.bot -side top -pady 10 -padx 10

#-->information

label $wr.top.lab -fg blue -relief raised -justify left -wraplength 5i \
 -text "Convert one NetCDF WRF-ARW output file to ARL format.\
Multiple time periods may be contained in one file. Single time\
period files must be manually concatenated."
pack $wr.top.lab

#-->meteorology input

frame $wr.data.lab
frame $wr.data.pick
pack $wr.data.lab $wr.data.pick -pady 2 -side top
button $wr.data.lab.set  -text "Set the WRF-ARW input file name" -width 55 -command {
   .arwarch.data.pick.src.list delete 0 end
   .arwarch.data.pick.dir.list delete 0 end
   set Grid_number 0
   do_file_list .arwarch.data.pick.src.list .arwarch.data.pick.dir.list 
   }
pack  $wr.data.lab.set -side left -padx 15

frame $wr.data.pick.dir
frame $wr.data.pick.src
pack $wr.data.pick.dir $wr.data.pick.src -side left -padx 5
listbox $wr.data.pick.dir.list -relief sunken -width 35 -height 1 -setgrid yes -bg LightCyan3
pack $wr.data.pick.dir.list -fill both -expand yes 
listbox $wr.data.pick.src.list -relief sunken -width 20 -height 1 -setgrid yes -bg LightCyan3
pack $wr.data.pick.src.list -fill both -expand yes

#-->output file name base

label $wr.mid5.lab1 -text "      Output path: "
entry $wr.mid5.ent1 -textvariable Fcst_path -relief sunken -width 30
pack $wr.mid5.lab1 $wr.mid5.ent1 -side left

label $wr.mid6.lab2 -text "      Output File: "
entry $wr.mid6.ent2 -textvariable Fcst_file -relief sunken -width 15
pack $wr.mid6.lab2 $wr.mid6.ent2 -side left

#-->bottom action buttons

button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help  -width 8 \
       -command "load_html [file join $html_dir S120.htm ] "

button $wr.bot.save  -bg green -text "Process Data" -width 20 -command {
       set gdir  [.arwarch.data.pick.dir.list get 0 0]
       set gbase [.arwarch.data.pick.src.list get 0 0]
       xtrct_data $gdir $gbase
       }
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 
set_date
}

#-->set current date

proc set_date {} {
global Grids_path Grid_number Work_path
global Year Month Fcst_path Fcst_file 
set Grid_number 0
set date [clock format [clock scan now] -format "%y %m %d %H"]
set Year [lindex [split $date] 0]
set Month [lindex [split $date] 1]
if {$Fcst_path == ""} {set Fcst_path "$Work_path"}
set Grids_path $Work_path
set Fcst_file "ARW$Year$Month"
}

#-->process grib data files and append to output file

proc xtrct_data {gdir gbase} {

global X_dir exec_dir tcl_platform
global Year Month Fcst_path Fcst_file 

set log [ScrollText .msg]
$log configure -cursor watch
$log insert end "Processing started ...\n"
update

#-->individual forecasts may be appended to this file: delete at start

if [file exists $Fcst_path/$Fcst_file] {file delete \
   -force $Fcst_path/$Fcst_file}

#  construct file name according to convention
   set data_file "$gdir/$gbase"

#  convert grib data to hysplit regional grid
   $log insert end "\n"
   if [file exists $data_file] {
      $log insert end "Starting to convert: $data_file \n"
      $log insert end "Program directory  : $exec_dir \n\n"
      update
   } else {
      $log insert end "Missing file: $data_file ... processing terminated \n"
      destroy .arwarch
      return}

   if [ file exists STDOUT ] {file delete STDOUT} 
   if { "$tcl_platform(platform)" == "unix" } {
      if { "$X_dir" == "" } {
      exec $exec_dir/arw2arl $data_file >STDOUT
      } else {
      exec $X_dir/xterm -fn fixed -e $exec_dir/arw2arl $data_file
      }
   } else {
      exec $exec_dir/arw2arl.exe $data_file >STDOUT
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

   if [file exists ARLDATA.BIN] {file rename ARLDATA.BIN $Fcst_path/$Fcst_file}

#  dump diagnostic message with each time period
   $log insert end "Finished: $data_file \n"

destroy .wrfarch
}
