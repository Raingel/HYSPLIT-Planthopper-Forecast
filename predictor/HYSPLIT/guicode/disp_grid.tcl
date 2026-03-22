proc grid_disp {} {
#-----------------------------------------------------------------------------
# DISP_GRID.TCL: meteorological data grid display script
# Last Revised: 06 Feb 2002
#               13 Aug 2002
#               21 Apr 2004
#               21 Sep 2004 - do not close widget after display
#               25 Oct 2004 - global postscript file name
#               10 Jan 2005 - simplify xterm shell
#               13 Aug 2009 - browse button on map background file
#               12 Sep 2016 - fixed browse button for shapefiles.txt
#               07 Nov 2016 - do not permit shapefile copy to itself
#               28 Jul 2017 - added initialization variable test
#-----------------------------------------------------------------------------
if [winfo exists .gridplot] {destroy .gridplot}
global html_dir
global Gpnts Mlabs Map_file

set wr .gridplot
toplevel $wr
wm title $wr " Meteorological Data Grid Point Locations "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.data
frame $wr.mid1
frame $wr.mid2
frame $wr.mid3
frame $wr.bot
pack $wr.top $wr.data $wr.mid1 $wr.mid2 $wr.mid3 $wr.bot \
     -side top -pady 5 -padx 10

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "Program to show the meteorological grid domain using the\
locations of each grid point. Output is written to file showgrid.html."
pack $wr.top.lab

#-->meteorology input
frame $wr.data.lab
frame $wr.data.pick
pack $wr.data.lab $wr.data.pick -pady 2 -side top
button $wr.data.lab.set  -text "Set File Name of ARL format Data" -width 55 -command {
   .gridplot.data.pick.src.list delete 0 end
   .gridplot.data.pick.dir.list delete 0 end
   set Grid_number 0
   do_file_list .gridplot.data.pick.src.list .gridplot.data.pick.dir.list 
   }
pack  $wr.data.lab.set -side left -padx 15
frame $wr.data.pick.dir
frame $wr.data.pick.src
pack $wr.data.pick.dir $wr.data.pick.src -side left -padx 5
listbox $wr.data.pick.dir.list -relief sunken -width 35 -height 1 -setgrid yes -bg LightCyan3
pack $wr.data.pick.dir.list -fill both -expand yes 
listbox $wr.data.pick.src.list -relief sunken -width 20 -height 1 -setgrid yes -bg LightCyan3
pack $wr.data.pick.src.list -fill both -expand yes

#-->grid point plotting increment
label $wr.mid1.lab -text "  Grid point plotting interval:" 
pack $wr.mid1.lab -side left
set d 0
foreach item [list 0 1 2 4 5 10] {
   radiobutton $wr.mid1.but$d -variable Gpnts -text $item -value $item
   pack $wr.mid1.but$d -side left
   incr d
   }

#-->lat-lon label interval
label $wr.mid2.lab3 -text "  Lat-Lon interval (deg):" 
pack $wr.mid2.lab3 -side left
set d 0
foreach item [list 0.2 0.5 1 2 5 10 15 30 60] {
   radiobutton $wr.mid2.but$d -variable Mlabs -text $item -value $item
   pack $wr.mid2.but$d -side left
   incr d
   }

#-->map background input file name
label $wr.mid3.lab -text "  Map background file: "
entry $wr.mid3.ent -textvariable Map_file -relief sunken -width 35
button $wr.mid3.win  -text Browse -width 8 -command {
   set temp [tk_getOpenFile -title "File Selection"]
   if {[string length $temp] > 0} {
      set cshp [string last shapefiles $temp]
      if {$cshp > 0} {
         if {[file dirname $temp] != [pwd]} {file copy -force $temp .}
         set Map_file [string range $temp $cshp end]
      } else {
         set Map_file $temp
      }
   }
}
pack $wr.mid3.lab $wr.mid3.ent $wr.mid3.win -side left -padx 5

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help  -width 8 \
       -command "load_html [file join $html_dir S133.htm ] "
button $wr.bot.save  -bg green -text "Create Map" -width 20 -command {
       set gdir  [.gridplot.data.pick.dir.list get 0 0]
       set gbase [.gridplot.data.pick.src.list get 0 0]
       run_grid $gdir $gbase}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 
set_showg
}

##################################################
proc set_showg {} {
global Grid_number Gpnts Mlabs maps_dir Map_file Psout_file

if [ info exists Gpnts ] { } else {set Gpnts ""}

if {$Gpnts == ""} {
   if {$Map_file == ""} {set Map_file ${maps_dir}/arlmap}
   set Gpnts 2
   set Mlabs 5
}
set Grid_number 0
set Psout_file "showgrid"
}

##################################################
#-->run plotting program
proc run_grid {gdir gbase} {
global X_dir tcl_platform exec_dir
global Gpnts Mlabs Map_file Psout_file

if {[file size $gdir/$gbase] == 0} {
   msg_box "File not found: $gdir/$gbase\n"
   destroy .gridplot
   return
   }

#-->decoder command line arguments
set arg1 -D  ; append arg1 $gdir/
set arg2 -F  ; append arg2 $gbase
set arg3 -I  ; append arg3 $Gpnts
set arg4 -L  ; append arg4 $Mlabs
set arg5 -A  ; append arg5 $Map_file 

set arg_ext "+g1"

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
   exec $exec_dir/showgrid $arg_ext $arg1 $arg2 $arg3 $arg4 $arg5
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/showgrid $arg_ext $arg1 $arg2 $arg3 $arg4 $arg5
   }
} else {
   exec $exec_dir/showgrid.exe $arg_ext $arg1 $arg2 $arg3 $arg4 $arg5
}
svg_box ${Psout_file}.html
}
