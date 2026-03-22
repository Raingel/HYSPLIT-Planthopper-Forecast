proc prof_disp {} {
#-----------------------------------------------------------------------------
# DISP_PROF.TCL: meteorological data profile script
# Last Revised: 11 Mar 2002
#               13 Aug 2002
#               28 Apr 2004
#               21 Sep 2004 - do not close widget after display
#               10 Jan 2005 - revised xterm link
#               28 Jul 2017 - use Wind variable for initialization
#-----------------------------------------------------------------------------
if [winfo exists .profplot] {destroy .profplot}
global html_dir
global Plat Plon Start Delta Wind

set wr .profplot
toplevel $wr
wm title $wr " Meteorological Data Data Profile "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.data
frame $wr.mid0
frame $wr.mid1
frame $wr.mid2
frame $wr.mid3
frame $wr.bot
pack $wr.top $wr.data $wr.mid0 $wr.mid1 $wr.mid2 $wr.mid3 $wr.bot \
     -side top -pady 5 -padx 10

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "Displays a text meteorological data profile (file:profile.txt)\
for an ARL formatted data set. Defaults (zeros) to grid center location\
for the first time period."
pack $wr.top.lab

#-->meteorology input
frame $wr.data.lab
frame $wr.data.pick
pack $wr.data.lab $wr.data.pick -pady 2 -side top
button $wr.data.lab.set  -text "Set File Name of ARL format Data" -width 55 -command {
   .profplot.data.pick.src.list delete 0 end
   .profplot.data.pick.dir.list delete 0 end
   set Grid_number 0
   do_file_list .profplot.data.pick.src.list .profplot.data.pick.dir.list 
   }
pack  $wr.data.lab.set -side left -padx 15
frame $wr.data.pick.dir
frame $wr.data.pick.src
pack $wr.data.pick.dir $wr.data.pick.src -side left -padx 5
listbox $wr.data.pick.dir.list -relief sunken -width 35 -height 1 -setgrid yes -bg LightCyan3
pack $wr.data.pick.dir.list -fill both -expand yes 
listbox $wr.data.pick.src.list -relief sunken -width 20 -height 1 -setgrid yes -bg LightCyan3
pack $wr.data.pick.src.list -fill both -expand yes

#-->components or wind direction
label $wr.mid0.lab -text "   Wind Display: "
radiobutton $wr.mid0.d0 -text "Vector" -variable Wind -value "0" 
radiobutton $wr.mid0.d1 -text "Polar"  -variable Wind -value "1" 
pack $wr.mid0.lab $wr.mid0.d0 $wr.mid0.d1 -side left

#-->grid point plotting increment
label $wr.mid1.lab -text "  Time offset (hrs):" 
pack $wr.mid1.lab -side left
set d 0
foreach item [list 0 2 3 6 12 24 48] {
   radiobutton $wr.mid1.but$d -variable Start -text $item -value $item
   pack $wr.mid1.but$d -side left
   incr d
   }

#-->time increment between profiles
label $wr.mid2.lab3 -text "  Time increment (hrs):" 
pack $wr.mid2.lab3 -side left
set d 0
foreach item [list 0 1 2 3 6 12 24] {
   radiobutton $wr.mid2.but$d -variable Delta -text $item -value $item
   pack $wr.mid2.but$d -side left
   incr d
   }

#-->profile location
label $wr.mid3.lab1 -text "Profile Location     Lat: "
entry $wr.mid3.ent1 -textvariable Plat -relief sunken -width 10
pack $wr.mid3.lab1 $wr.mid3.ent1 -side left
label $wr.mid3.lab2 -text "    Lon: "
entry $wr.mid3.ent2 -textvariable Plon -relief sunken -width 10
pack $wr.mid3.lab2 $wr.mid3.ent2 -side left

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help  -width 8 \
       -command "load_html [file join $html_dir S132.htm ] "
button $wr.bot.save  -bg green -text "Run PROFILE" -width 20 -command {
       set gdir  [.profplot.data.pick.dir.list get 0 0]
       set gbase [.profplot.data.pick.src.list get 0 0]
       run_prof $gdir $gbase}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 
set_prof
}

##################################################
proc set_prof {} {
global Grid_number Plat Plon Start Delta Wind

if [ info exists Wind ] { } else {set Wind ""}
if {$Wind == ""} {
   if {$Plat == ""} {set Plat 0.0}
   if {$Plon == ""} {set Plon 0.0}
   set Start 0
   set Delta 0
   set Wind 1
}
set Grid_number 0
}

###################################################
#-->run plotting program
proc run_prof {gdir gbase} {
global X_dir log tcl_platform exec_dir
global Plat Plon Start Delta Wind

if {[file size $gdir/$gbase] == 0} {
   msg_box "File not found: $gdir/$gbase\n"
   destroy .profplot
   return
   }

#-->decoder command line arguments
set arg1 -D  ; append arg1 $gdir/
set arg2 -F  ; append arg2 $gbase
set arg3 -Y  ; append arg3 $Plat
set arg4 -X  ; append arg4 $Plon
set arg5 -O  ; append arg5 $Start 
set arg6 -T  ; append arg6 $Delta
set arg7 -W  ; append arg7 $Wind 

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
   exec $exec_dir/profile $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/profile $arg1 $arg2 \
                                  $arg3 $arg4 $arg5 $arg6 $arg7
   }
} else {
   exec $exec_dir/profile.exe $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7
}

if [file exists profile.txt] {
   set log [ScrollText .f]
   $log insert end "METEOROLOGICAL PROFILE LISTING ...\n"
   set fileid [open "profile.txt" r]
   while {[eof $fileid] != 1} {
      gets $fileid cline
      $log insert end $cline\n
   }
   close $fileid
} else {
  msg_box "File not found: profile.txt"
}
}
