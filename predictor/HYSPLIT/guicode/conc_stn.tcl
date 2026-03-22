proc conc_2stn {} {
#------------------------------------------------------------------------------
# CONC_STN.TCL: interpolate binary concentration to station script
# Last Revised: 15 Jul 2002 
#               13 Aug 2002
#               10 Jan 2005 - simplify xterm shell
#               18 Mar 2005 - add level/species option
#               07 Nov 2008 - version 4.9 update
#               29 Sep 2009 - add GE option
#               26 Dec 2010 - plot supplemental file
#               11 Jul 2011 - quick plot in DATEM format
#               16 Aug 2012 - browse button for DATEM file name
#               17 Apr 2013 - force input file
#               23 Jan 2014 - add interpolation method option
#               07 Jul 2014 - correct force name entry for multiple inputs
#               03 Sep 2014 - added TCM option for binary input file
#               17 Jul 2017 - changed wording for Google Earth
#               28 Jul 2017 - changed Cfact to Cscale
#               02 Sep 2020 - default initial value Cscale = 1
#               23 Jul 2021 - use svg instead of postscript.
#               03 Feb 2023 - fix extracting station from user input.
#               01 Jun 2023 - fix creating kml for the time plot.
#------------------------------------------------------------------------------

global num_samplers Level Species Rformat Interp Force_name TCM
global html_dir Grid_name Num_disp Dformat Out_file Sup_file
global Stnlist Stnid Stnlat Stnlon Cscale GE_plot Stn_log

if [winfo exists .timeplot] {destroy .timeplot}
set wr .timeplot
toplevel $wr
wm title $wr " Time Series Data Extraction "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.mid0
frame $wr.mid0b
frame $wr.mid5
frame $wr.midR
frame $wr.midI
frame $wr.mid8
frame $wr.mid0a
frame $wr.mid1
frame $wr.mid1a
frame $wr.mid2
frame $wr.mid2b
frame $wr.mid2a
frame $wr.mid3
frame $wr.mid7
frame $wr.mid4
frame $wr.mid9
frame $wr.bot
pack $wr.top $wr.mid0 $wr.mid0b -pady 5 -padx 10
pack $wr.mid5 $wr.midR $wr.midI $wr.mid3 $wr.mid8 $wr.mid0a $wr.mid1 $wr.mid1a $wr.mid2 \
     $wr.mid2b $wr.mid2a $wr.mid7 $wr.mid4 $wr.mid9 -side top -pady 3 -padx 10
pack $wr.bot -side top -pady 5 -padx 10

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "Extract the concentration time series at a specified location\
with output written to file \\working\\con2stn.txt.  SVG plotting\
is optional but requires setting a multiplier for integer display units."
pack $wr.top.lab

#-->select grid
label $wr.mid0.lab -text "Input Data: "
pack $wr.mid0.lab -side left -padx 5
set d 0
foreach item $Grid_name {
   radiobutton $wr.mid0.$d -variable Num_disp -text $item -value $d -background grey
   pack $wr.mid0.$d -side left -padx 2
   incr d
}
entry $wr.mid0.ent -textvariable Force_name -relief sunken -width 20
label $wr.mid0.txt -text "  Force: "
pack  $wr.mid0.txt $wr.mid0.ent -side left 

label $wr.mid0b.lab1 -text "=Height Index    "
entry $wr.mid0b.ent1 -textvariable Level -width 2
pack  $wr.mid0b.ent1 $wr.mid0b.lab1 -side left
label $wr.mid0b.lab2 -text "=Species Index   "
entry $wr.mid0b.ent2 -textvariable Species -width 2
pack  $wr.mid0b.ent2 $wr.mid0b.lab2 -side left
checkbutton $wr.mid0b.box -variable TCM -text " TCM format" -background grey
pack $wr.mid0b.box -side left

#-->date format
label $wr.mid5.log -text "Output Date:"
pack $wr.mid5.log -side left
radiobutton $wr.mid5.d0 -variable Dformat -text "Julian (graphic)" -value 0 
radiobutton $wr.mid5.d1 -variable Dformat -text "MM/DD/YYYY" -value 1 
radiobutton $wr.mid5.d2 -variable Dformat -text "MM/DD/YYYY NO header" -value 2 
pack $wr.mid5.d0 $wr.mid5.d1 $wr.mid5.d2 -side left 

#-->date format
label $wr.midR.log -text "Output Format:"
pack $wr.midR.log -side left
radiobutton $wr.midR.d0 -variable Rformat -text "Record" -value 0 
radiobutton $wr.midR.d1 -variable Rformat -text "Column" -value 1 
radiobutton $wr.midR.d2 -variable Rformat -text "DATEM" -value 2 
pack $wr.midR.d0 $wr.midR.d1 $wr.midR.d2 -side left 

#-->interpolation method
label $wr.midI.log -text "Interpolation method:"
pack $wr.midI.log -side left
radiobutton $wr.midI.d0 -variable Interp -text "Nearest Neighbor" -value 0 
radiobutton $wr.midI.d1 -variable Interp -text "Bilinear Interpolation" -value 1 
pack $wr.midI.d0 $wr.midI.d1 -side left 

#-->integer concentration multiplier
label $wr.mid3.lab -text "Concentration Multiplier:"
entry $wr.mid3.ent -textvariable Cscale -relief sunken -width 14
pack $wr.mid3.lab $wr.mid3.ent -side left

#-->output file
label $wr.mid8.lab -text "Output File Base Name: "
entry $wr.mid8.ent -textvariable Out_file -relief sunken -width 20
pack $wr.mid8.lab $wr.mid8.ent -side left

#-->divider
label $wr.mid0a.lab -text "_____________________________________________________________"
pack  $wr.mid0a.lab -side left
 
#-->extraction file name
label  $wr.mid1.l1  -text "Station File:" 
entry  $wr.mid1.e1  -textvariable Stnlist -width 12
button $wr.mid1.win  -text Browse -width 8 -command {
   set temp [tk_getOpenFile -title "File Selection"]
   if {[string length $temp] > 0} {set Stnlist $temp}}
label  $wr.mid1.l2  -text "  Numb Samplers:" 
entry  $wr.mid1.e2  -textvariable num_samplers  -width 2
button $wr.mid1.but  -text "New File" -width 10  -command "samp_loc"
pack   $wr.mid1.l1 $wr.mid1.e1 $wr.mid1.win $wr.mid1.l2 $wr.mid1.e2 $wr.mid1.but -side left -padx 2

label $wr.mid1a.lab -fg blue -text " ... or enter values for one site below ..." 
pack  $wr.mid1a.lab -side top 

#-->extraction point
label $wr.mid2.l1 -text "  Station ID:"  
entry $wr.mid2.e1 -textvariable Stnid -width 10
pack  $wr.mid2.l1 $wr.mid2.e1 -side left  
label $wr.mid2.l2 -text "  Latitude:" 
entry $wr.mid2.e2 -textvariable Stnlat -width 10
pack  $wr.mid2.l2 $wr.mid2.e2 -side left 
label $wr.mid2.l3 -text "  Longitude:" 
entry $wr.mid2.e3 -textvariable Stnlon -width 10
pack  $wr.mid2.l3 $wr.mid2.e3 -side left 

#-->action buttons
button $wr.mid2b.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.mid2b.help -text Help  -width 8 \
       -command "load_html [file join $html_dir S342.htm ] "
button $wr.mid2b.save -bg yellow -text "Extract Data" -width 20 -command {run_time}
pack  $wr.mid2b.dismiss $wr.mid2b.help $wr.mid2b.save -side left -padx 10 

#-->divider
label $wr.mid2a.lab -text "_____________________________________________________________"
pack $wr.mid2a.lab -side left
 
#-->google earth options
label $wr.mid7.lev -text "Convert to KML/KMZ Output File:"
pack $wr.mid7.lev -side left
radiobutton $wr.mid7.d1 -variable GE_plot -text "No " -value 0 
radiobutton $wr.mid7.d2 -variable GE_plot -text "Yes" -value 1 
pack $wr.mid7.d1 $wr.mid7.d2 -side left

#-->display options
#label $wr.mid4.lev -text "Display Time Series:"
#pack $wr.mid4.lev -side left
#radiobutton $wr.mid4.d1 -variable Stn_plot -text "No "  -value 0 
#radiobutton $wr.mid4.d2 -variable Stn_plot -text "Yes" -value 1 
#pack $wr.mid4.d1 $wr.mid4.d2 -side left

#-->scaling options
label $wr.mid4.log -text "   Ordinate scale:"
pack $wr.mid4.log -side left
radiobutton $wr.mid4.d3 -variable Stn_log -text "Lin " -value 0 
radiobutton $wr.mid4.d4 -variable Stn_log -text "Log " -value 1 
pack $wr.mid4.d3 $wr.mid4.d4 -side left

#-->supplemental data plot file
label $wr.mid9.lab -text "Supplemental Data file Name:"
entry $wr.mid9.ent -textvariable Sup_file -relief sunken -width 25
button $wr.mid9.win  -text Browse -width 8 -command {
   set temp [tk_getOpenFile -title "File Selection"]
   if {[string length $temp] > 0} {set Sup_file $temp}}
pack $wr.mid9.lab $wr.mid9.ent $wr.mid9.win -padx 5 -side left

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help  -width 8 \
       -command "load_html [file join $html_dir S342.htm ] "
button $wr.bot.save  -bg green -text "Plot Data" -width 20 -command {plot_time}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 -pady 5
set_defaults
}

#---------------------------------------------------------------------
proc run_time {} {

global Stnlist Stnid Stnlat Stnlon Cscale GE_plot
global X_dir tcl_dir exec_dir tcl_platform result
global Grid_dir Grid_name Num_disp Rformat Interp Force_name
global Level Species Dformat Out_file TCM

if { "$Force_name" == "" } {
   set Grid_dir_n [lindex $Grid_dir $Num_disp]
   set Grid_name_n  [lindex $Grid_name $Num_disp]
   set arg1 -i  ; append arg1 $Grid_dir_n$Grid_name_n
} else {
   set arg1 -i  ; append arg1 $Force_name
}
 
set arg2 -o  ; append arg2 ${Out_file}.txt
set arg3 -c  ; append arg3 $Cscale

set arg4 -s
if { "$Stnlist" == "" } {
   set arg4 "-:"
} else {
   append arg4 $Stnlist
}
set input "" ; append input $Stnid { } $Stnlat { } $Stnlon \n

set arg7 -z  ; append arg7 $Level
set arg8 -p  ; append arg8 $Species
set arg9 -d  ; append arg9 $Dformat
set arg0 -r  ; append arg0 $Rformat
set arga -o  ; append arga $Out_file
if { "$Interp" == "1" } {
  set Interp1 "i"
  set argb -x  ; append argb $Interp1
} else {
  set argb -x  ; append argb $Interp
}
set argc -:  ; if { "$TCM" == "1" } {set argc "-t"}

if { $Rformat <= 1} {
if { $Rformat == 0 || $Dformat != 0 || $Level == 0 || $Species == 0 } {
   msg_box "Output format incompatible with time series plot"
   tkwait window .msg_win
   if {$result != 0} {return}
}
}

# turn on for diagnostic testing
# msg_box "con2stn $arg1 $arg2 $arg3 $arg7 $arg8 $arg9 $arg0 $argb $argc"  
# tkwait window .msg_win

if { "$Stnlist" == "" } {
#  station location from menu
   if { "$tcl_platform(platform)" == "unix" } {
      if { "$X_dir" == "" } {
      exec $exec_dir/con2stn $arg1 $arg2 $arg3 $arg7 $arg8 $arg9 $arg0 $argb $argc <<$input
      } else {
      # The $input value is passed to xterm but not to con2stn.  Drop xterm. (3 FEB 2023)
      #exec $X_dir/xterm -fn fixed -e $exec_dir/con2stn $arg1 $arg2 \
      #                   $arg3 $arg7 $arg8 $arg9 $arg0 $argb $argc <<$input
      exec $exec_dir/con2stn $arg1 $arg2 $arg3 $arg7 $arg8 $arg9 $arg0 $argb $argc <<$input
      } 
   } else {
      exec $exec_dir/con2stn.exe $arg1 $arg2 $arg3 $arg7 $arg8 $arg9 $arg0 $argb $argc <<$input
   }

} else {
#  station locations from input file
   if { "$tcl_platform(platform)" == "unix" } {
      if { "$X_dir" == "" } {
      exec $exec_dir/con2stn $arg1 $arg2 $arg3 $arg7 $arg8 $arg4 $arg9 $arg0 $argb $argc
      } else {
      exec $X_dir/xterm -fn fixed -e $exec_dir/con2stn $arg1 $arg2 \
                   $arg3 $arg7 $arg8 $arg4 $arg9 $arg0 $argb $argc
      }
   } else {
      exec $exec_dir/con2stn.exe $arg1 $arg2 $arg3 $arg7 $arg8 $arg4 $arg9 $arg0 $argb $argc
   }
}

if [file exists ${Out_file}.txt] {
   set log [ScrollText .f]
   $log insert end " Contents of ${Out_file}.txt ...\n"
   update
   set fileid [open "${Out_file}.txt" r]
   while {[eof $fileid] != 1} {
      gets $fileid cline
      $log insert end $cline\n
   }
   close $fileid
} else {
  msg_box "File not found: ${Out_file}.txt"
}

}


#---------------------------------------------------------------------
proc plot_time {} {

global Stnid Stnlat Stnlon
global GE_plot Stn_log
global X_dir tcl_dir exec_dir tcl_platform
global Out_file Sup_file
global result log pipe_closed

set arg5 -i  ; append arg5 ${Out_file}.txt
set arg6 -:  ; if { "$Stn_log" == "1" } {set arg6 "-y"}
set arg_ext "+g1"

set args -s
if { "$Sup_file" == "" } {
   set args "-:"
} else {
   append args $Sup_file
}

if [file exists timeplot.html] {
   file delete timeplot.html
}

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
     #exec $exec_dir/timeplot $arg_ext $arg5 $arg6 $args
     set code "$exec_dir/timeplot $arg_ext $arg5 $arg6 $args"
   } else {
     #exec $X_dir/xterm -fn fixed -e $exec_dir/timeplot $arg_ext $arg5 $arg6 $args
     set code "$exec_dir/timeplot $arg_ext $arg5 $arg6 $args"
   }
} else {
  #exec $exec_dir/timeplot.exe $arg_ext $arg5 $arg6 $args
  set code "$exec_dir/timeplot.exe $arg_ext $arg5 $arg6 $args"
}
RunWatchLog $code "timeplot"

if [file exists timeplot.html] {
   svg_box {timeplot.html}
}


if { "$GE_plot" == "1" } {
   if [file exists ${Out_file}.kml] { file delete ${Out_file}.kml }

   set arg4 -o; append arg4 $Out_file

   if { "$Sup_file" == "" } {
      # station location from menu
      set input "" ; append input $Stnid { } $Stnlat { } $Stnlon \n
      if { "$tcl_platform(platform)" == "unix" } {
         exec $exec_dir/stn2ge $arg4 $arg5 $args <<$input
      } else {
         exec $exec_dir/stn2ge.exe $arg4 $arg5 $args <<$input
      }
   } else {
      #  station locations from input file
      if { "$tcl_platform(platform)" == "unix" } {
         if { "$X_dir" == "" } {
            exec $exec_dir/stn2ge $arg4 $arg5 $args
         } else {
            exec $X_dir/xterm -fn fixed -e $exec_dir/stn2ge $arg4 $arg5 $args
         }
      } else {
         exec $exec_dir/stn2ge.exe $arg4 $arg5 $args
      }
   }

   if [file exists ${Out_file}.kml] {
      msg_box "KML file created: ${Out_file}.kml"
   } else {
      msg_box "KML file not found: ${Out_file}.kml"
   }
}

}


#-->set initial defaults
proc set_defaults {} {
global num_samplers Level Species Psout_file Dformat Out_file Sup_file Force_name
global Stnlist Stnid Stnlat Stnlon Cscale GE_plot Stn_log Rformat Interp TCM

if [ info exists Stnid ] { } else {set Stnid ""}

if { $Stnid == "" } {
   set Psout_file timeplot
   set Out_file con2stn
   set Sup_file ""
   set Stnid 99999
   set Stnlat 0.0
   set Stnlon 0.0
   if { $Cscale == "" } {set Cscale 1.0}
   set GE_plot 0
   set Stn_log 0
   set Stnlist ""
   set Level 1
   set Species 1
   set num_samplers 1
   set Dformat 0
   set Rformat 1
   set Interp 0
   set TCM 0
   if [ info exists Force_name ] { } else {set Force_name ""}
   }
}

#-----------------------------------------------------------------
proc samp_loc {} {

global num_samplers samp_pos 

if [winfo exists .samp_loc] {destroy .samp_loc}
set wr .samp_loc
toplevel $wr
wm title $wr "Sampling Location Setup"
wm  geometry $wr +100+100

frame $wr.title
frame $wr.loc
frame $wr.end
pack $wr.title $wr.loc $wr.end -padx 4 -side top

label $wr.title.lab1 -text "Set up $num_samplers Sampling Locations"
label $wr.title.lab2 -text "IntegerID Latitude Longitude"
pack  $wr.title.lab1 $wr.title.lab2

for { set d 1} { $d <=$num_samplers} {incr d} {
   frame $wr.loc.dat$d
   pack $wr.loc.dat$d -side top
   label $wr.loc.dat$d.lab -text "$d:"
   entry $wr.loc.dat$d.ent -textvariable samp_pos($d) -width 20
   pack $wr.loc.dat$d.lab $wr.loc.dat$d.ent -padx 4 -side left
}
button $wr.end.dismiss -text Quit -width 8 -command "destroy $wr"
button $wr.end.help -text OK  -width 8 -command "save_sampler $wr"
pack $wr.end.dismiss $wr.end.help -side left -pady 6 -padx 10
init_sampler 
}

#-----------------------------------------------------------------
proc init_sampler {} {

global num_samplers samp_pos

for { set d 1} { $d <=$num_samplers} {incr d} {
    set samp_pos($d) [format "%4.4d % 05.2f %  07.2f" $d 0 0]
}
}

#-----------------------------------------------------------------
proc save_sampler {wr} {

global num_samplers samp_pos Stnlist

if {$Stnlist == ""} {set Stnlist "STNLIST.TXT"}
set f [open $Stnlist w]

for { set d 1} { $d <=$num_samplers} {incr d} {
    puts $f $samp_pos($d)
}
close $f
destroy $wr
}
