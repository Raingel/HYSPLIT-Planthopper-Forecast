proc map_disp {} {
#-----------------------------------------------------------------------------
# DISP_MAP.TCL: meteorological data contour program script
# Last Revised: 06 Feb 2002
#               13 Aug 2002
#               28 Apr 2004
#               21 Sep 2004 - do not close widget after display
#               25 Oct 2004 - global postscript file name
#               10 Jan 2005 - simplify xterm shell
#               16 Oct 2008 - added option to select variable via text entry
#               22 Nov 2010 - new option to turn contour lines on or off
#               28 Jul 2017 - initialize test for varb
#               26 Jul 2021 - use svg instead of postscript
#-----------------------------------------------------------------------------
if [winfo exists .mapplot] {destroy .mapplot}
global html_dir
global Plat Plon Start Delta Kolor Map_file 
global Max_con Del_con Gis Level Varb Radius Klev Lines

set wr .mapplot
toplevel $wr
wm title $wr " Contour Map of Meteorological Data Variables "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.data
frame $wr.mid1
frame $wr.mid2
frame $wr.mid3
frame $wr.mid4
frame $wr.mid5
frame $wr.bot
pack $wr.top $wr.data $wr.mid1 $wr.mid2 $wr.mid3 $wr.mid4 $wr.mid5 \
     $wr.bot -side top -pady 5 -padx 10

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 7i \
 -text "Contour a selected field in the ARL meteorological data file for one\
or more time periods. Graphics output is written to the file contour.html."
pack $wr.top.lab

#-->meteorology input
frame $wr.data.lab
frame $wr.data.pick
pack $wr.data.lab $wr.data.pick -pady 2 -side top
button $wr.data.lab.set  -text "Set File Name of ARL format Data" -width 76 -command {
   .mapplot.data.pick.src.list delete 0 end
   .mapplot.data.pick.dir.list delete 0 end
   set Grid_number 0
   do_file_list .mapplot.data.pick.src.list .mapplot.data.pick.dir.list 
   }
pack  $wr.data.lab.set -side left -padx 15
frame $wr.data.pick.dir
frame $wr.data.pick.src
pack $wr.data.pick.dir $wr.data.pick.src -side left -padx 5
listbox $wr.data.pick.dir.list -relief sunken -width 45 -height 1 -setgrid yes -bg LightCyan3
pack $wr.data.pick.dir.list -fill both -expand yes 
listbox $wr.data.pick.src.list -relief sunken -width 30 -height 1 -setgrid yes -bg LightCyan3
pack $wr.data.pick.src.list -fill both -expand yes

#-->select variable
frame $wr.mid1.lab
frame $wr.mid1.top
frame $wr.mid1.bot
frame $wr.mid1.ent
pack $wr.mid1.lab $wr.mid1.top $wr.mid1.bot $wr.mid1.ent -side top
label $wr.mid1.lab.txt -text "    ----- Select Surface or Upper Level Variables -----"
pack $wr.mid1.lab.txt
 
#-->surface variables
label $wr.mid1.top.lab -text "Surface:   "
pack $wr.mid1.top.lab -side left
set d 0
foreach item [list SHGT PRSS TPP3 TPP6 PRT6 U10M V10M T02M VECT DIVG] {
   radiobutton $wr.mid1.top.$d -variable Varb -text $item -value $item \
               -command {set Level 1}
   pack $wr.mid1.top.$d -side left
   incr d
   }

#-->upper level variables
label $wr.mid1.bot.lab -text "Level:"
entry $wr.mid1.bot.ent -textvariable Level -relief sunken -width 2
pack $wr.mid1.bot.lab $wr.mid1.bot.ent -side left -padx 4
bind $wr.mid1.bot.ent <KeyRelease> updateLEV
set d 0
foreach item [list HGTS UWND VWND WWND TKEN TEMP RELH SPHU VECT DIVG ] {
   radiobutton $wr.mid1.bot.$d -variable Varb -text $item -value $item \
               -command {if {"$Level" == "1"} {set Level $Klev} \
                                              {set Klev $Level}}
   pack $wr.mid1.bot.$d -side left
   incr d
   }

#->manual variable selection
label $wr.mid1.ent.txt -text "Variable selected for display:  "
entry $wr.mid1.ent.lab -textvar Varb -relief sunken -width 5
pack  $wr.mid1.ent.txt $wr.mid1.ent.lab -side left

#-->initial map time offset
label $wr.mid2.lab -text "  Time offset (hrs):" 
pack $wr.mid2.lab -side left
set d 0
foreach item [list 0 2 3 6 12 24 48] {
   radiobutton $wr.mid2.but$d -variable Start -text $item -value $item
   pack $wr.mid2.but$d -side left
   incr d
   }

#-->time increment between maps
label $wr.mid3.lab -text "  Time increment (hrs):" 
pack $wr.mid3.lab -side left
set d 0
foreach item [list 0 1 2 3 6 12 24] {
   radiobutton $wr.mid3.but$d -variable Delta -text $item -value $item
   pack $wr.mid3.but$d -side left
   incr d
   }

#-->map location
label $wr.mid4.lab1 -text "Map Center ->   Lat:"
entry $wr.mid4.ent1 -textvariable Plat -relief sunken -width 6
pack $wr.mid4.lab1 $wr.mid4.ent1 -side left
label $wr.mid4.lab2 -text "  Lon:"
entry $wr.mid4.ent2 -textvariable Plon -relief sunken -width 6
pack $wr.mid4.lab2 $wr.mid4.ent2 -side left
label $wr.mid4.lab3 -text "  Radius:"
entry $wr.mid4.ent3 -textvariable Radius -relief sunken -width 6
pack $wr.mid4.lab3 $wr.mid4.ent3 -side left

#-->contour defaults
label $wr.mid5.lab1 -text "Contour ->   Max:"
entry $wr.mid5.ent1 -textvariable Max_con -relief sunken -width 6
pack $wr.mid5.lab1 $wr.mid5.ent1 -side left
label $wr.mid5.lab2 -text "   Delta:"
entry $wr.mid5.ent2 -textvariable Del_con -relief sunken -width 6
pack $wr.mid5.lab2 $wr.mid5.ent2 -side left
label $wr.mid5.lab3 -text "  "
checkbutton $wr.mid5.ent3 -variable Kolor -text "Color" -background grey
checkbutton $wr.mid5.ent4 -variable Lines -text "Lines" -background grey
checkbutton $wr.mid5.ent5 -variable Gis -text "GIS" -background grey
pack $wr.mid5.lab3 $wr.mid5.ent3 $wr.mid5.ent4 $wr.mid5.ent5 -side left

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help  -width 8 \
       -command "load_html [file join $html_dir S131.htm ] "
button $wr.bot.save  -bg green -text "Run CONTOUR" -width 20 -command {
       set gdir  [.mapplot.data.pick.dir.list get 0 0]
       set gbase [.mapplot.data.pick.src.list get 0 0]
       run_map $gdir $gbase}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 
set_cmap
}


##################################################
proc set_cmap {} {
global Grid_number Psout_file
global Plat Plon Start Delta Kolor maps_dir Map_file Klev
global Max_con Del_con Gis Level Varb Radius Lines

if [ info exists Varb ] { } else {set Varb ""}

if {$Varb == ""} {
   set Varb "TEMP"
   set Grid_number 0
   if {$Plat == ""} {set Plat 0.0}
   if {$Plon == ""} {set Plon 0.0}
   set Start 0
   set Delta 0
   set Kolor 1
   set Lines 1
   set Max_con -1.0
   set Del_con -1.0
   set Gis 0
   set Level 2
   set Klev 2
   set Radius 15.0
   }
set Map_file ${maps_dir}/arlmap
set Psout_file "contour"
}

proc updateLEV {} {
global Level Klev
set Klev $Level
}

#####################################################################
#-->run plotting program

proc run_map {gdir gbase} {
global X_dir tcl_platform exec_dir Psout_file
global Plat Plon Start Delta Kolor Map_file 
global Max_con Del_con Gis Level Varb Radius Lines

if {[file size $gdir/$gbase] == 0} {
   msg_box "File not found: $gdir/$gbase\n"
   destroy .mapplot
   return
   }

set Poly $Kolor
if {"$Lines" == "0" } {set Poly [expr $Kolor + 2]}

#-->decoder command line arguments
set arg1 -D  ; append arg1 $gdir/
set arg2 -F  ; append arg2 $gbase
set arg3 -Y  ; append arg3 $Plat
set arg4 -X  ; append arg4 $Plon
set arg5 -O  ; append arg5 $Start 
set arg6 -T  ; append arg6 $Delta 
set arg7 -C  ; append arg7 $Poly
set arg8 -G  ; append arg8 $Map_file
set arg9 -M  ; append arg9 $Max_con
set arga -I  ; append arga $Del_con
set argb -A  ; append argb $Gis
set argc -L  ; append argc $Level
set argd -V  ; append argd $Varb
set arge -R  ; append arge $Radius
set arg_ext "+g1"

if { "$tcl_platform(platform)" == "unix" } {
   if { "$X_dir" == "" } {
   exec $exec_dir/contour  $arg_ext \
        $arg1 $arg2 $arg3 \
        $arg4 $arg5 $arg6 $arg7 $arg8 $arg9 $arga $argb $argc $argd $arge
   } else {
   exec $X_dir/xterm -fn fixed -e $exec_dir/contour  $arg_ext \
        $arg1 $arg2 $arg3 \
        $arg4 $arg5 $arg6 $arg7 $arg8 $arg9 $arga $argb $argc $argd $arge
   }
} else {
   exec $exec_dir/contour.exe $arg_ext \
                              $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 \
                              $arg8 $arg9 $arga $argb $argc $argd $arge
}
svg_box ${Psout_file}.html
}
