proc conc_xtrct {} {
#-----------------------------------------------------------------------------
# CONC_XTRCT.TCL: concentration extraction
# Last Revised: 19 October 2009
#               16 March   2012
#               28 July    2017 - set Extract_typ as initialization variable
#-----------------------------------------------------------------------------
if [winfo exists .conxtrct] {destroy .conxtrct}
global html_dir

set wr .conxtrct
toplevel $wr
wm title $wr " Concentration Grid Extraction Utility "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.data
frame $wr.mid0
frame $wr.mid1
frame $wr.mid2
frame $wr.mid3
frame $wr.mid4
frame $wr.bot
pack $wr.top $wr.data $wr.mid0 $wr.mid1 $wr.mid2  $wr.mid3 $wr.mid4\
     -side top -pady 5 -padx 10
pack $wr.bot -side top -pady 10 -padx 10


#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
-text "Program to extract a subgrid from the contents of a HYSPLIT concentration\
data file. The new subgrid is written to the file: conxtrct.bin."
pack $wr.top.lab

arl_set

#-->input file
frame $wr.data.lab
frame $wr.data.pick
pack $wr.data.lab $wr.data.pick -pady 2 -side top
button $wr.data.lab.set  -text "Set File Name of Concentration Input File" -width 55 -command {
   .conxtrct.data.pick.src.list delete 0 end
   .conxtrct.data.pick.dir.list delete 0 end
   set Grid_number 0
   do_file_list .conxtrct.data.pick.src.list .conxtrct.data.pick.dir.list 
       set gdir  [.conxtrct.data.pick.dir.list get 0 0]
       set gbase [.conxtrct.data.pick.src.list get 0 0]
       run_coninfo $gdir $gbase

   }
pack  $wr.data.lab.set -side left -padx 15
frame $wr.data.pick.dir
frame $wr.data.pick.src
pack $wr.data.pick.dir $wr.data.pick.src -side left -padx 5
listbox $wr.data.pick.dir.list -relief sunken -width 35 -height 1 -setgrid yes -bg LightCyan3
pack $wr.data.pick.dir.list -fill both -expand yes 
listbox $wr.data.pick.src.list -relief sunken -width 20 -height 1 -setgrid yes -bg LightCyan3
pack $wr.data.pick.src.list -fill both -expand yes

#-->choose which extract to perform

label $wr.mid0.lab -text "Select type of extract:"
radiobutton $wr.mid0.d0 -text "0:Time" -variable Extract_typ \
-value "0" 
radiobutton $wr.mid0.d1 -text "1:Area" -variable Extract_typ \
-value "1" 
radiobutton $wr.mid0.d2 -text "2:Time and Area" -variable Extract_typ \
-value "2" 

pack $wr.mid0.lab $wr.mid0.d0 $wr.mid0.d1 $wr.mid0.d2 -side left

#-->select start and ending day

label $wr.mid1.lab1 -text "  Start Month:"
entry $wr.mid1.ent1 -textvariable Sdate_mn -relief sunken -width 3
pack $wr.mid1.lab1 $wr.mid1.ent1 -side left

label $wr.mid1.lab2 -text "  Start Day:"
entry $wr.mid1.ent2 -textvariable Sdate_dy -relief sunken -width 3
pack $wr.mid1.lab2 $wr.mid1.ent2 -side left

label $wr.mid1.lab3 -text "  Start Hour:"
entry $wr.mid1.ent3 -textvariable Sdate_hr -relief sunken -width 3
pack $wr.mid1.lab3 $wr.mid1.ent3 -side left

label $wr.mid2.lab1 -text "    End Month:"
entry $wr.mid2.ent1 -textvariable Edate_mn -relief sunken -width 3
pack $wr.mid2.lab1 $wr.mid2.ent1 -side left

label $wr.mid2.lab2 -text "    End Day:"
entry $wr.mid2.ent2 -textvariable Edate_dy -relief sunken -width 3
pack $wr.mid2.lab2 $wr.mid2.ent2 -side left

label $wr.mid2.lab3 -text "    End Hour:"
entry $wr.mid2.ent3 -textvariable Edate_hr -relief sunken -width 3
pack $wr.mid2.lab3 $wr.mid2.ent3 -side left

#-->select extract grid points

label $wr.mid3.lab1 -text "   Lower Left latitude:"
entry $wr.mid3.ent1 -textvariable Lat1 -relief sunken -width 8
pack $wr.mid3.lab1 $wr.mid3.ent1 -side left

label $wr.mid3.lab2 -text "   Lower Left Longitude:"
entry $wr.mid3.ent2 -textvariable Lon1 -relief sunken -width 9
pack $wr.mid3.lab2 $wr.mid3.ent2 -side left

label $wr.mid4.lab1 -text "  Upper Right latitude:"
entry $wr.mid4.ent1 -textvariable Lat2 -relief sunken -width 8
pack $wr.mid4.lab1 $wr.mid4.ent1 -side left

label $wr.mid4.lab2 -text "  Upper Right Longitude:"
entry $wr.mid4.ent2 -textvariable Lon2 -relief sunken -width 9
pack $wr.mid4.lab2 $wr.mid4.ent2 -side left

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help  -width 8 \
       -command "load_html [file join $html_dir S349.htm ] "
button $wr.bot.save  -bg green -text "Extract Grid" -width 20 -command {
       set gdir  [.conxtrct.data.pick.dir.list get 0 0]
       set gbase [.conxtrct.data.pick.src.list get 0 0]
       run_conxtrct $gdir $gbase

}

pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 
}


##################################################
#-->set directory defaults

proc arl_set {} {
global Extract_typ Lat1 Lon1 Lat2 Lon2
global Sdate_mn Sdate_dy Sdate_hr Edate_mn Edate_dy Edate_hr

if [ info exists Extract_typ ] { } else {set Extract_typ ""}

if {$Extract_typ == ""} {
   set Extract_typ "0"
   set Lat1 ""
   set Lon1 ""
   set Lat2 ""
   set Lon2 ""
   set Sdate_mn ""
   set Sdate_dy ""
   set Sdate_hr ""
   set Edate_mn ""
   set Edate_dy ""
   set Edate_hr ""
   }
}

##################################################
#-->run coninfo program

proc run_coninfo {gdir gbase} {
global exec_dir Num_pts Incr_latlon Latlon1 Latlon2 Strt_date End_date 
global Lat1 Lon1 Lat2 Lon2 Sdate_mn Sdate_dy Sdate_hr
global Edate_mn Edate_dy Edate_hr

if {[file size $gdir/$gbase] == 0} {
   msg_box "File not found: $gdir/$gbase\n"
   destroy .conxtrct
   return
   }

#-->decoder command line arguments
  set input [file join $gdir $gbase]
  set dfile coninfo.txt
  exec $exec_dir/coninfo -i$input >$dfile

  if [file exists $dfile] {
   set f [open "$dfile" r]
   gets $f Num_pts
   gets $f Incr_latlon
   gets $f Latlon1
     set Lat1  [lindex $Latlon1 4]
     set Lon1 [lindex $Latlon1 5]
   gets $f Latlon2
     set Lat2  [lindex $Latlon2 4]
     set Lon2 [lindex $Latlon2 5]
   gets $f Strt_date
     set Sdate_mn [lindex $Strt_date 3]
     set Sdate_dy [lindex $Strt_date 4]
     set Sdate_hr [lindex $Strt_date 5]
   gets $f End_date
     set Edate_mn [lindex $End_date 3]
     set Edate_dy [lindex $End_date 4]
     set Edate_hr [lindex $End_date 5]
   close $f
  }

}

##################################################
#-->run conxtrct program

proc run_conxtrct {gdir gbase} {
global exec_dir Extract_typ
global Lat1 Lon1 Lat2 Lon2 Sdate_mn Sdate_dy Sdate_hr
global Edate_mn Edate_dy Edate_hr

if {[file size $gdir/$gbase] == 0} {
   msg_box "File not found: $gdir/$gbase\n"
   destroy .conxtrct
   return
   }

if [file exists conxtrct.bin] {file delete conxtrct.bin}

#--> make sure time/date values are 2 digits
 set Sd_mn [format %2.2d $Sdate_mn]
 set Sd_dy [format %2.2d $Sdate_dy]
 set Sd_hr [format %2.2d $Sdate_hr]
 set Ed_mn [format %2.2d $Edate_mn]
 set Ed_dy [format %2.2d $Edate_dy]
 set Ed_hr [format %2.2d $Edate_hr]

#-->decoder command line arguments
  set hyph ":"
  set arg1 -i  ; append arg1 $gdir/$gbase
  set arg2 -o  ; append arg2 "conxtrct.bin"
  set arg3 -b  ; append arg3 $Lat1$hyph$Lon1$hyph$Lat2$hyph$Lon2
  set arg4 -p  ; append arg4 $Sd_mn$Sd_dy$Sd_hr$hyph$Ed_mn$Ed_dy$Ed_hr
  set arg5 -x1

if {$Extract_typ == "0"} {
  set arg3 -b  ; append arg3 "0:0:0:0"
  exec $exec_dir/concrop $arg1 $arg2 $arg4 $arg3 $arg5
} elseif {$Extract_typ == "1"} {
  exec $exec_dir/concrop $arg1 $arg2 $arg3 $arg5
} else {
  exec $exec_dir/concrop $arg1 $arg2 $arg3 $arg4 $arg5
}

if [file exists conxtrct.bin] {
  msg_box "Concentration extract created in working directory (conxtrxt.bin)"
} else {
  msg_box "File not found: conxtrct.bin"
}

}
