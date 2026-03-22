proc era_arch {} {

#-----------------------------------------------------------------------------
# ARCH_ERA.TCL: Convert grib ECMWF archive data files to ARL format
# Last Revised: 09 Apr 2002
#               13 Aug 2002
#               20 Apr 2004 - modified to work with online ERA-40 archive
#               10 Jan 2005 - simplify xterm shell
#               21 Aug 2009 - trap standard output
#               10 Feb 2011 - new option to set precip accumulation time
#               09 Dec 2014 - modified ECMWF URL
#-----------------------------------------------------------------------------

global sfcp levels rain html_dir Grid_number
global Year Month Fdur Lat Lon Fcst_path Fcst_file Method
global gdir gbase sdir sbase cdir cbase 
if [winfo exists .eraarch] {destroy .eraarch}
set wr .eraarch 
toplevel $wr
wm title $wr " Converts ECMWF Reanalysis archive GRIB-1 data to ARL format "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.data
frame $wr.surf
frame $wr.cons
frame $wr.mid1
frame $wr.mid4
frame $wr.mid5
frame $wr.mid6
frame $wr.bot
pack $wr.top -pady 4 -padx 2 
pack $wr.data $wr.surf $wr.cons -pady 2
pack $wr.mid1 $wr.mid4 $wr.mid5 -side top -pady 6 -anchor w
pack $wr.mid6 $wr.bot -side top -pady 10 -padx 10

#-->information

label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "Converts multi-time-period ECMWF ERA-40 or -Interim GRIB-1 files to ARL\
format. Files are available from http://apps.ecmwf.int/datasets/. The following\
fields are required: 3D (geop, temp, uvel, vvel, wvel, rh); 2D (2m temp, 10m UV);\
Invariant (geop). These are designated by the three different input files."
pack $wr.top.lab

#-->3D meteorology input

frame $wr.data.lab
frame $wr.data.pick
pack $wr.data.lab $wr.data.pick -pady 2 -side left
button $wr.data.lab.set  -text "3D Data" -width 15 -command {
   .eraarch.data.pick.src.list delete 0 end
   .eraarch.data.pick.dir.list delete 0 end
   set Grid_number 0
   do_file_list .eraarch.data.pick.src.list .eraarch.data.pick.dir.list
   }
pack  $wr.data.lab.set -side left -padx 15

frame $wr.data.pick.dir
frame $wr.data.pick.src
pack $wr.data.pick.dir $wr.data.pick.src -side left -padx 5
listbox $wr.data.pick.dir.list -relief sunken -width 30 -height 1 -setgrid yes -bg LightCyan3
pack $wr.data.pick.dir.list -fill both -expand yes 
listbox $wr.data.pick.src.list -relief sunken -width 20 -height 1 -setgrid yes -bg LightCyan3
pack $wr.data.pick.src.list -fill both -expand yes

#-->2D meteorology input

frame $wr.surf.lab
frame $wr.surf.pick
pack $wr.surf.lab $wr.surf.pick -pady 2 -side left
button $wr.surf.lab.set  -text "2D Surface" -width 15 -command {
   .eraarch.surf.pick.src.list delete 0 end
   .eraarch.surf.pick.dir.list delete 0 end
   set Grid_number 0
   do_file_list .eraarch.surf.pick.src.list .eraarch.surf.pick.dir.list 
   }
pack  $wr.surf.lab.set -side left -padx 15

frame $wr.surf.pick.dir
frame $wr.surf.pick.src
pack $wr.surf.pick.dir $wr.surf.pick.src -side left -padx 5
listbox $wr.surf.pick.dir.list -relief sunken -width 30 -height 1 -setgrid yes -bg LightCyan3
pack $wr.surf.pick.dir.list -fill both -expand yes 
listbox $wr.surf.pick.src.list -relief sunken -width 20 -height 1 -setgrid yes -bg LightCyan3
pack $wr.surf.pick.src.list -fill both -expand yes

#-->Invariant (constant) input

frame $wr.cons.lab
frame $wr.cons.pick
pack $wr.cons.lab $wr.cons.pick -pady 2 -side left
button $wr.cons.lab.set  -text "Invariant" -width 15 -command {
   .eraarch.cons.pick.src.list delete 0 end
   .eraarch.cons.pick.dir.list delete 0 end
   set Grid_number 0
   do_file_list .eraarch.cons.pick.src.list .eraarch.cons.pick.dir.list
   }
pack  $wr.cons.lab.set -side left -padx 15

frame $wr.cons.pick.dir
frame $wr.cons.pick.src
pack $wr.cons.pick.dir $wr.cons.pick.src -side left -padx 5
listbox $wr.cons.pick.dir.list -relief sunken -width 30 -height 1 -setgrid yes -bg LightCyan3
pack $wr.cons.pick.dir.list -fill both -expand yes 
listbox $wr.cons.pick.src.list -relief sunken -width 20 -height 1 -setgrid yes -bg LightCyan3
pack $wr.cons.pick.src.list -fill both -expand yes

#-->output file name base

label $wr.mid4.lab1 -text "    Sfc Pres:"
radiobutton $wr.mid4.but1 -variable sfcp -text Yes -value 1
radiobutton $wr.mid4.but2 -variable sfcp -text No  -value 0
pack $wr.mid4.lab1 $wr.mid4.but1 $wr.mid4.but2 -side left

label $wr.mid4.lab2 -text "    Numb Levels:"
entry $wr.mid4.ent2 -textvariable levels -relief sunken -width 3
pack $wr.mid4.lab2 $wr.mid4.ent2 -side left

label $wr.mid4.lab3 -text "    Rain Sum (hr):"
entry $wr.mid4.ent3 -textvariable rain -relief sunken -width 3
pack $wr.mid4.lab3 $wr.mid4.ent3 -side left

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
       -command "load_html [file join $html_dir S126.htm ] "

button $wr.bot.save  -bg green -text "Process Data" -width 20 -command {
       set gdir  [.eraarch.data.pick.dir.list get 0 0]
       set gbase [.eraarch.data.pick.src.list get 0 0]
       set sdir  [.eraarch.surf.pick.dir.list get 0 0]
       set sbase [.eraarch.surf.pick.src.list get 0 0]
       set cdir  [.eraarch.cons.pick.dir.list get 0 0]
       set cbase [.eraarch.cons.pick.src.list get 0 0]
       xtrct_data 
       }
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 
set_date
}

#-->set current date

proc set_date {} {
global sfcp levels rain Grids_path Grid_number Work_path
global Year Month Fdur Lat Lon Fcst_path Fcst_file Method 
set Grid_number 0
set date [clock format [clock scan now] -format "%y %m %d %H"]
set Year [lindex [split $date] 0]
set Month [lindex [split $date] 1]
if {$Fcst_path == ""} {set Fcst_path "$Work_path"}
set Grids_path $Work_path
set Fcst_file "ERA$Year$Month"
set Method 3
set Lat 60.0
set Lon -80.0
set sfcp 0
set rain 12
set levels 25
if [file exists DATA.ARL] {file delete -force DATA.ARL}
}

#-->process grib data files and append to output file

proc xtrct_data {} {

global sfcp levels rain X_dir exec_dir tcl_platform
global Lat Lon Fcst_path Fcst_file Method 
global gdir gbase sdir sbase cdir cbase 

set log [ScrollText .msg]
$log configure -cursor watch
$log insert end "Processing started ...\n"
update

#-->individual forecasts are appended to this file: delete at start

if [file exists $Fcst_path/$Fcst_file] {file delete \
   -force $Fcst_path/$Fcst_file}

if {[file size $gdir/$gbase] == 0} {
   $log insert end "File not found: $gdir/$gbase\n"
   destroy .eraarch
   return}

#  decoder command line arguments
   set arg1 -i  ; append arg1 $gdir/$gbase
   set arg2 -y  ; append arg2 $Lat 
   set arg3 -x  ; append arg3 $Lon 
   set arg4 -g  ; append arg4 $Method 
   set arg5 -p  ; append arg5 $sfcp     
   set arg6 -k  ; append arg6 $levels 
   set arg9 -r  ; append arg9 $rain

   set arg7 "-:"
   if {[file size $sdir/$sbase] > 1024} {
      $log insert end "Using 2D file: $sdir/$sbase\n"
      set arg7 -s ; append arg7 $sdir/$sbase}

   set arg8 "-:"
   if {[file size $cdir/$cbase] > 1024} {
      $log insert end "Using Invariant: $cdir/$cbase\n"
      set arg8 -c ; append arg8 $cdir/$cbase}

#  convert grib data to hysplit regional grid
   $log insert end "\n"
   if [file exists $gdir/$gbase] {
      $log insert end "Starting to convert: $arg1 $arg2 $arg3 $arg4 $arg9 \n"
      $log insert end "       with options: $arg5 $arg6 $arg7 $arg8 \n"
      $log insert end "Program directory  : $exec_dir \n\n"
      update
   } else {
      $log insert end "Missing file: $gdir/$gbase ... processing terminated \n"
      destroy .eraarch
      return}

   if [ file exists STDOUT ] {file delete STDOUT} 
   if { "$tcl_platform(platform)" == "unix" } {
      if { "$X_dir" == "" } {
      exec $exec_dir/grib2arl $arg1 $arg2 $arg3 $arg4 $arg9 $arg5 $arg6 $arg7 $arg8 >STDOUT
      } else {
      exec $X_dir/xterm -fn fixed -e $exec_dir/grib2arl $arg1 $arg2 $arg3 $arg4 $arg9 \
                                                        $arg5 $arg6 $arg7 $arg8
      }
   } else {
      exec $exec_dir/grib2arl.exe $arg1 $arg2 $arg3 $arg4 $arg9 $arg5 $arg6 $arg7 $arg8 >STDOUT
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
      $log insert end "Processed: $gdir/$gbase ... created DATA.ARL \n"
   } else {
      $log insert end "Missing file: DATA.ARL ... output from grib2arl \n"
      destroy .eraarch
      return}

#  initialize summation file or append output file
   if [file exists $Fcst_path/$Fcst_file] {
      file delete $Fcst_path/$Fcst_file}
   file rename DATA.ARL $Fcst_path/$Fcst_file
   $log insert end "Renamed file: DATA.ARL to $Fcst_file \n"

#  dump diagnostic message with each time period
   $log insert end "Finished: $gdir/$gbase \n"

destroy .eraarch
}
