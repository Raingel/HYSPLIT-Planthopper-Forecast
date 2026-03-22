proc cfgconc_init { } {

#-----------------------------------------------------------------------------
# CONC_CFG.TCL: setup.cfg namelist file configuration script
# Last Revised: 22 Mar 2002
#               13 Aug 2002 - html browser
#               13 Sep 2002 - default cfg file
#               15 May 2003 - enhanced particle init/dump options
#               17 Nov 2003 - additional turbulence options
#               20 Sep 2004 - load names to blank problem solved
#               13 Oct 2004 - kpuff option
#               26 May 2005 - cmass for mass output instead of concentration
#               24 May 2006 - temporal emissions file
#               20 Nov 2006 - restructured for enhanced submenu options
#               21 May 2007 - proc convert name change to chemods
#               19 Jul 2007 - more TKE options added to menu
#               05 Feb 2008 - split-merge variables to menu
#               05 Nov 2008 - version 4.9 options
#               14 Aug 2009 - enhanced window title label
#               07 Apr 2010 - STILT emulation mode
#               10 Mar 2011 - set only varying concentration layer
#               05 Jul 2011 - maxdim added to output list
#               13 Jun 2014 - delete CONC.CFG when pressing reset
#               29 Aug 2014 - added ichem=10 TCM option
#               03 Sep 2014 - skip null list elements when reading CONC.CFG
#               25 Nov 2014 - added cpack=3 option
#               16 Jan 2015 - set default kblt=0
#               14 Sep 2016 - added Save_path
#               16 Sep 2016 - autocreation of CHEMRATE.TXT file
#               26 May 2017 - dimensioned CPACK variable
#               17 Jul 2017 - fixed problem when loading file with no CPACK
#               31 Jul 2017 - insure crate always defined in CHEMRATE.TXT
#               12 Sep 2018 - added vscale & hscale to menu
#               19 Feb 2019 - new namelist variable GEMAGE
#               13 Dec 2019 - new namelist variable CMTFN
#               08 May 2020 - add a vertical scrollbar to menu 7.
#               01 Sep 2020 - added reset for hscale,vscale,kmix0 to menu 7
#                           - initialized dispersion to idsp=1 in menu 3
#               01 Jun 2021 - change wvert default to true
#-----------------------------------------------------------------------------

global maxdim p10f html_dir cnumb pakval cpack Grid_name Num_pollut

if [winfo exists .configinit] {destroy .configinit}
set wr .configinit
toplevel $wr
wm title $wr "Create Optional Concentration Namelist File: SETUP.CFG"
wm  geometry $wr +5+5

frame $wr.step 
frame $wr.metg
frame $wr.init
frame $wr.pnum
frame $wr.mrge
frame $wr.emit
frame $wr.turb
frame $wr.pack 
frame $wr.piof
frame $wr.chem
frame $wr.ensb
frame $wr.cmtr
frame $wr.wver
frame $wr.exit

pack $wr.step $wr.metg $wr.init $wr.pnum $wr.mrge $wr.emit  \
     $wr.turb $wr.pack $wr.piof $wr.chem $wr.ensb $wr.cmtr $wr.wver -side top -pady 3
pack $wr.exit -side top -pady 8

label  $wr.step.txt -text "       Set fixed or automatic TIME STEPS (1): "
button $wr.step.but  -text "Menu" -width 6 -command "tsteps"
pack   $wr.step.txt $wr.step.but -side left

label  $wr.metg.txt -text "        Define subgrid and MSL/AGL UNITS (2): "
button $wr.metg.but  -text "Menu" -width 6 -command "metgrd"
pack   $wr.metg.txt $wr.metg.but -side left

label  $wr.init.txt -text " Configure release of PARTICLES or PUFFS (3): "
button $wr.init.but  -text "Menu" -width 6 -command "modtyp"
pack   $wr.init.txt $wr.init.but -side left

label  $wr.pnum.txt -text " Set particle/puff RELEASE NUMBER limits (4): "
button $wr.pnum.but  -text "Menu" -width 6 -command "parnum"
pack   $wr.pnum.txt $wr.pnum.but -side left

label  $wr.mrge.txt -text "     Set the puff SPLIT-MERGE parameters (5): "
button $wr.mrge.but  -text "Menu" -width 6 -command "merge"
pack   $wr.mrge.txt $wr.mrge.but -side left

label  $wr.emit.txt -text "   Define EMISSION CYCLING or input file (6): "
button $wr.emit.but  -text "Menu" -width 6 -command "emission"
pack   $wr.emit.txt $wr.emit.but -side left

label  $wr.turb.txt -text "         Configure the TURBULENCE method (7): "
button $wr.turb.but  -text "Menu" -width 6 -command "turbdif"
pack   $wr.turb.txt $wr.turb.but -side left

label  $wr.pack.txt -text "       Concentration GRID PACKING method (8): "
button $wr.pack.but  -text "Menu" -width 6 -command "conpak"
pack   $wr.pack.txt $wr.pack.but -side left

label  $wr.piof.txt -text "         Input and output PARTICLE FILES (9): "
button $wr.piof.but  -text "Menu" -width 6 -command "pardump"
pack   $wr.piof.txt $wr.piof.but -side left

label  $wr.chem.txt -text "    In-Line chemical CONVERSION MODULES (10): "
button $wr.chem.but  -text "Menu" -width 6 -command "chemods"
pack   $wr.chem.txt $wr.chem.but -side left

label  $wr.ensb.txt -text "    Meteorological grid offset ENSEMBLE (11): "
button $wr.ensb.but  -text "Menu" -width 6 -command "ensemble"
pack   $wr.ensb.txt $wr.ensb.but -side left

label  $wr.cmtr.txt -text "       Output CENTER-OF-MASS trajectory (12): "
button $wr.cmtr.but  -text "Menu" -width 6 -command "cmtout"
pack   $wr.cmtr.txt $wr.cmtr.but -side left

label  $wr.wver.txt -text "             WRF vertical interpolation (13): "
button $wr.wver.but  -text "Menu" -width 6 -command "wrfvert"
pack   $wr.wver.txt $wr.wver.but -side left

#-->termination

label $wr.exit.top -text "--------------------------------------------------------"
pack  $wr.exit.top -side top
button $wr.exit.dismiss  -bg red -text "Quit" -width 12 -command "destroy $wr"
button $wr.exit.reset  -text "Reset" -width 6 -command "clear_config"
button $wr.exit.help -text "Help" -width 6 \
       -command "load_html [file join $html_dir S412.htm ] "
button $wr.exit.save  -text "Save as" -width 8 -command "save_as_def"
button $wr.exit.load  -text "Retrieve" -width 8 -command "load_as_def"
button $wr.exit.cont  -bg green1 -text "Save" -width 12 -command "save_config SETUP.CFG $wr"
pack  $wr.exit.dismiss $wr.exit.reset $wr.exit.help $wr.exit.save $wr.exit.load \
      $wr.exit.cont -side left -padx 4

if [ info exists Grid_name ]  { } else {set Grid_name ""}
if { $Grid_name == "" } {set Grid_name "Undefined"}
if [ info exists Num_pollut ] { } else {set Num_pollut ""}
if { $Num_pollut == "" } {set Num_pollut 1}

if [file exists CONC.CFG] {
   load_config CONC.CFG .dummy
   set cnumb 0
   set pakval 1
   set cpack $pakval,
   foreach item $Grid_name {
      incr cnumb
   }
} else {
   reset_config
}
chemread
}


#-----------------------------------------------------------------------------
proc tsteps {} {
global tset delt tratio html_dir

if [winfo exists .step_win] {destroy .step_win}
set wr .step_win
toplevel $wr
wm title $wr "(1) Integration Time Step"
wm  geometry $wr +100+100

set ts0 $tset
set dt0 $delt
set tr0 $tratio

frame $wr.title
frame $wr.val1
frame $wr.val2
frame $wr.end

pack  $wr.title -pady 10
pack  $wr.val1 $wr.val2
pack  $wr.end -pady 20

label $wr.title.lab -fg blue -relief raised -justify left -wraplength 6i \
-text "The integration time step is set automatically each hour according\
to the stabilty ratio criteria. Under certain conditions, such as the\
output may be required at fixed intervals less than one hour, it may be\
desirable to fix the time step to a constant value. A negative value sets\
the minimum time step (abs)."
pack  $wr.title.lab

radiobutton $wr.val1.d0 -text "Set Value                 " -variable tset -value "1" 
radiobutton $wr.val1.d1 -text "Set Ratio  " -variable tset -value "2"
pack $wr.val1.d0 $wr.val1.d1 -side left

label $wr.val2.lab1 -text "Time step (min): "
entry $wr.val2.ent1 -textvariable delt -width 4
label $wr.val2.lab2 -text "   Stability ratio: "
entry $wr.val2.ent2 -textvariable tratio -width 4
pack $wr.val2.lab1 $wr.val2.ent1 $wr.val2.lab2 $wr.val2.ent2 -side left -padx 4

button $wr.end.quit  -bg red -text Quit -width 12 -command "set tset $ts0; set delt $dt0; set tratio $tr0; destroy $wr"
button $wr.end.reset  -text Reset -width 12 -command "set tset 2; set delt 0; set tratio 0.75"
button $wr.end.help -text "Help" -width 12 -command "load_html [file join $html_dir S610.htm ] "
button $wr.end.save  -bg green1 -text Save -width 12 -command "destroy $wr"

pack $wr.end.quit $wr.end.reset $wr.end.help $wr.end.save -side left  -padx 4
}


#-----------------------------------------------------------------------------
proc metgrd {} {
global mgmin kmsl html_dir

if [winfo exists .metg_win] {destroy .metg_win}
set wr .metg_win
toplevel $wr
wm title $wr "(2) Meteorological Subgrid and Vertical Coordinates"
wm  geometry $wr +100+100

set mg0 $mgmin
set km0 $kmsl

frame $wr.title
frame $wr.met1
frame $wr.met2
frame $wr.end

pack  $wr.title -pady 10
pack  $wr.met1 $wr.met2
pack  $wr.end -pady 20

label $wr.title.lab -fg blue -relief raised -justify left -wraplength 6i \
-text "The subgrid expands automatically during the calculation to\
encompss the size of the plume. A subgrid larger than the meteorological\
grid forces the model to load the entire data grid. Source heights and
concentration grid heights can be defined as either height above ground-\
level (AGL-default) or relataive to mean-sea-level (MSL)."
pack  $wr.title.lab

label $wr.met1.lab -text "Minimum horizontal size of the meteorological subgrid: "
entry $wr.met1.ent -textvariable mgmin -relief sunken -width 4
pack $wr.met1.lab $wr.met1.ent -side left

label $wr.met2.div -text "___________________________________________"
label $wr.met2.txt -fg blue -text "Height unit for input and output"
pack  $wr.met2.div $wr.met2.txt -side top

radiobutton $wr.met2.d0 -text "Heights above ground level" -variable kmsl -value "0" 
radiobutton $wr.met2.d1 -text "Relative to mean-sea-level" -variable kmsl -value "1" 
pack $wr.met2.d0 $wr.met2.d1

button $wr.end.quit  -bg red -text Quit -width 12 -command "set mgmin $mg0; set kmsl $km0; destroy $wr"
button $wr.end.reset  -text Reset -width 12 -command "set mgmin 10; set kmsl 0"
button $wr.end.help -text "Help" -width 12 -command "load_html [file join $html_dir S621.htm ] "
button $wr.end.save  -bg green1 -text Save -width 12 -command "destroy $wr"

pack $wr.end.quit $wr.end.reset $wr.end.help $wr.end.save -side left  -padx 4
}


#-----------------------------------------------------------------------------
proc modtyp {} {
global initd html_dir conage gemage

if [winfo exists .mode_win] {destroy .mode_win}
set wr .mode_win
toplevel $wr
wm title $wr "(3) Particle-Puff Release Mode"
wm  geometry $wr +100+100

set ini0 $initd
set age0 $conage
set age1 $gemage

frame $wr.title
frame $wr.std -pady 5
frame $wr.dur -pady 5
frame $wr.cnv
frame $wr.dsp -pady 5
frame $wr.gem -pady 5
frame $wr.end

pack  $wr.title -pady 10
pack  $wr.std $wr.dur $wr.cnv $wr.gem $wr.dsp
pack  $wr.end -pady 20

label $wr.title.lab -fg blue -relief raised -justify left -wraplength 6i \
-text "The particle-puff release mode determines if emissions are assigned\
to puffs, particles, or a combination. In particle mode the dispersion is\
computed by adding a random component to the particle motion, while in the\
puff mode dispersion is computed by assuming a concentration distribution\
and its rate of growth. Fewer puffs than particles need to be released\
for comparable simulation results."
pack  $wr.title.lab

label $wr.std.lab -fg blue -text "Particle-puff release mode ... constant for run duration"
radiobutton $wr.std.d0 -variable initd -value "0"   -text "3-D particle horizontal and vertical          "
radiobutton $wr.std.d1 -variable initd -value "1"   -text "Gaussian-horizontal TH-vertical puff  (Gh-THv)"  
radiobutton $wr.std.d2 -variable initd -value "2"   -text "Top-Hat-horizontal and vertical puff (THh-THv)"  
radiobutton $wr.std.d3 -variable initd -value "3"   -text "Gaussian-horizonal particle-vertical  (Gh-Pv) "  
radiobutton $wr.std.d4 -variable initd -value "4"   -text "Top-Hat-horizontal particle-vertical (THh-Pv) " 
pack $wr.std.lab $wr.std.d0 $wr.std.d1 $wr.std.d2 $wr.std.d3 $wr.std.d4

label $wr.dur.pad -fg blue -text "Particle-puff release mode ... conversion after"
entry $wr.dur.ent -textvariable conage -relief sunken -width 3
label $wr.dur.txt -fg blue -text " hours"
pack  $wr.dur.pad $wr.dur.ent $wr.dur.txt -side left

radiobutton $wr.cnv.d5 -variable initd -value "103" -text "3D particle converts to  Gh-Pv                "
radiobutton $wr.cnv.d6 -variable initd -value "104" -text "3D particle converts to THh-Pv                "
radiobutton $wr.cnv.d7 -variable initd -value "130" -text "Gh-Pv converts to 3D particle                 "
radiobutton $wr.cnv.d8 -variable initd -value "140" -text "THh-Pv converts to 3D particle                "
pack $wr.cnv.d5  $wr.cnv.d6 $wr.cnv.d7 $wr.cnv.d8 -side top

label $wr.gem.pad -fg blue -text "Particle-puff transfer GEM ... conversion after"
entry $wr.gem.ent -textvariable gemage -relief sunken -width 3
label $wr.gem.txt -fg blue -text " hours"
pack  $wr.gem.pad $wr.gem.ent $wr.gem.txt -side left

label $wr.dsp.lab -fg blue -text "Particle dispersion algorithm"
radiobutton $wr.dsp.d1 -variable idsp -value "1"   -text "HYSPLIT                                        "
radiobutton $wr.dsp.d2 -variable idsp -value "2"   -text "STILT                                          "
pack $wr.dsp.lab $wr.dsp.d1 $wr.dsp.d2

button $wr.end.quit  -bg red -text Quit -width 12 -command "set initd $ini0; set conage $age0; set gemage $age1; destroy $wr"
button $wr.end.reset  -text Reset -width 12 -command "set initd 0; set conage 24; set gemage 48; set idsp 1"
button $wr.end.help -text "Help" -width 12 -command "load_html [file join $html_dir S622.htm ] "
button $wr.end.save  -bg green1 -text Save -width 12 -command "destroy $wr"

pack $wr.end.quit $wr.end.reset $wr.end.help $wr.end.save -side left  -padx 4
}


#-----------------------------------------------------------------------------
proc merge {} {
global kspl krnd frhs frvs frts frhmax splitf html_dir

if [winfo exists .merge_win] {destroy .merge_win}
set wr .merge_win
toplevel $wr
wm title $wr "(5) Puff Split-Merge Options"
wm  geometry $wr +100+100

set ks0 $kspl
set kr0 $krnd
set fh0 $frhs
set fv0 $frvs
set ft0 $frts
set fm0 $frhmax
set sp0 $splitf

frame $wr.title
frame $wr.val1
frame $wr.val2
frame $wr.val3
frame $wr.val4
frame $wr.val5
frame $wr.val6
frame $wr.val7
frame $wr.end

pack  $wr.title -pady 10
pack  $wr.val1 $wr.val2 $wr.val3 $wr.val4 $wr.val5 $wr.val6 $wr.val7
pack  $wr.end -pady 20

label $wr.title.lab -fg blue -relief raised -justify left -wraplength 5i \
-text "The parameters in this menu control the rate of puff splitting\
and merging. Caution should be exercised in modifying these values.\
See the associated help file for more detailed information."
pack  $wr.title.lab

label $wr.val1.lab -text "                    Split frequency (hrs): " 
entry $wr.val1.ent -textvariable kspl -width 6
pack $wr.val1.lab $wr.val1.ent -side left

label $wr.val2.lab -text "          Enhanced merging interval (hrs): "
entry $wr.val2.ent -textvariable krnd -width 6
pack $wr.val2.lab $wr.val2.ent -side left

label $wr.val3.lab -text "        Horizontal merge distance (sigma): "
entry $wr.val3.ent -textvariable frhs -width 6
pack $wr.val3.lab $wr.val3.ent -side left

label $wr.val4.lab -text "          Vertical merge distance (sigma): " 
entry $wr.val4.ent -textvariable frvs -width 6
pack $wr.val4.lab $wr.val4.ent -side left

label $wr.val5.lab -text "       Temporal merge distance (fraction): "
entry $wr.val5.ent -textvariable frts -width 6
pack $wr.val5.lab $wr.val5.ent -side left

label $wr.val6.lab -text "         Maximum horizontal merge (sigma): "
entry $wr.val6.ent -textvariable frhmax -width 6
pack $wr.val6.lab $wr.val6.ent -side left

label $wr.val7.lab -text "Horizontal split factor (puff/grid ratio): "
entry $wr.val7.ent -textvariable splitf -width 6
pack $wr.val7.lab $wr.val7.ent -side left

button $wr.end.quit  -bg red -text Quit -width 12 -command "set kspl $ks0; set krnd $kr0; set frhs $fh0; set frvs $fv0; set frts $ft0; set frhmax $fm0; set splitf $sp0;destroy $wr"
button $wr.end.reset  -text Reset -width 12 -command "set kspl 1; set krnd 6; set frhs 1.0; set frvs 0.01; set frts 0.10; set frhmax 3.0; set splitf 1.0"
button $wr.end.help -text "Help" -width 12 -command "load_html [file join $html_dir S630.htm ] "
button $wr.end.save  -bg green1 -text Save -width 12 -command "destroy $wr"

pack $wr.end.quit $wr.end.reset $wr.end.help $wr.end.save -side left  -padx 4
}


#-----------------------------------------------------------------------------
proc parnum {} {
global numpar maxpar khmax html_dir

if [winfo exists .part_win] {destroy .part_win}
set wr .part_win
toplevel $wr
wm title $wr "(4) Particle/Puff Release Number Limits"
wm  geometry $wr +100+100

set np0 $numpar
set mp0 $maxpar
set kh0 $khmax

frame $wr.title
frame $wr.val1
frame $wr.val2
frame $wr.val3
frame $wr.end

pack  $wr.title -pady 10
pack  $wr.val1 $wr.val2 $wr.val3
pack  $wr.end -pady 20

label $wr.title.lab -fg blue -relief raised -justify left -wraplength 6i \
-text "The particle/puff release rate is computed from the per cycle\
definition. The minimum rate is one per time step. Emissions are turned\
off when the maximum is reached. Particles are terminated if they exceed\
the maximum age."
pack  $wr.title.lab

label $wr.val1.lab -text "   Particles released per cycle: " 
entry $wr.val1.ent -textvariable numpar -width 6
pack $wr.val1.lab $wr.val1.ent -side left

label $wr.val2.lab -text "    Maximum number of particles: "
entry $wr.val2.ent -textvariable maxpar -width 6
pack $wr.val2.lab $wr.val2.ent -side left

label $wr.val3.lab -text "Maximum particle duration (hrs): "
entry $wr.val3.ent -textvariable khmax -width 6
pack $wr.val3.lab $wr.val3.ent -side left

button $wr.end.quit  -bg red -text Quit -width 12 -command "set numpar $np0; set maxpar $mp0; set khmax $kh0; destroy $wr"
button $wr.end.reset  -text Reset -width 12 -command "set numpar 2500; set maxpar 10000; set khmax 9999"
button $wr.end.help -text "Help" -width 12 -command "load_html [file join $html_dir S623.htm ] "
button $wr.end.save  -bg green1 -text Save -width 12 -command "destroy $wr"

pack $wr.end.quit $wr.end.reset $wr.end.help $wr.end.save -side left  -padx 4
}

#-----------------------------------------------------------------------------

proc conpak {} {
global cnumb cpack pakval cmass html_dir

if [winfo exists .pack_win] {destroy .pack_win}
set wr .pack_win
toplevel $wr
wm title $wr "(8) Concentration Grid Packing Method"
wm  geometry $wr +100+100

set cp0 $cpack
set cm0 $cmass

frame $wr.title
frame $wr.pack
frame $wr.grid
frame $wr.mass
frame $wr.end

pack  $wr.title -pady 10
pack  $wr.pack $wr.grid $wr.mass
pack  $wr.end -pady 20

label $wr.title.lab -fg blue -relief raised -justify left -wraplength 5i \
-text "Defines the structure of the concentration output file: it can be
packed or unpacked. If most points have nonzero mass, then unpacked\
storage is more efficient. Two non-rectangular grid options exist. One\
defines the grid as just one point. The other option defines a polar\
coordinate grid. Grid defintions may be different for each grid. The\
output units can be concentration (mass divided by volume) or just mass."
pack  $wr.title.lab -padx 2

label $wr.pack.txt -fg blue -text "Concentration Grid Packing Method"
radiobutton $wr.pack.d0 -variable pakval -value "0" -text "(0) Off - all points written" 
radiobutton $wr.pack.d1 -variable pakval -value "1" -text "(1) On - only nonzero values" 
radiobutton $wr.pack.d2 -variable pakval -value "2" -text "(2) Single - at center point" 
radiobutton $wr.pack.d3 -variable pakval -value "3" -text "(3) Polar grid (arc,dist)   " 
pack $wr.pack.txt $wr.pack.d0 $wr.pack.d1 $wr.pack.d2 $wr.pack.d3

label  $wr.grid.txt1 -text " CPACK="
entry  $wr.grid.ent1 -textvariable cpack -relief sunken -width 12
label  $wr.grid.txt2 -text " Set grid:"
entry  $wr.grid.ent2 -textvariable cnumb -relief sunken -width 4
button $wr.grid.but  -bg green1 -text Update -width 12 -command {
       set tmp [expr ($cnumb*2)-2]
       set wrk ""
       append wrk [string range $cpack 0 $tmp-1] $pakval "," [string range $cpack $tmp+2 end] 
       set cpack $wrk}
pack   $wr.grid.txt1 $wr.grid.ent1 $wr.grid.txt2 $wr.grid.ent2 $wr.grid.but -side left -padx 6

label $wr.mass.div -text "____________________________________"
label $wr.mass.txt -fg blue -text "Output Units - Concentration or Mass"
pack  $wr.mass.div $wr.mass.txt -side top
radiobutton $wr.mass.d3 -variable cmass -value "0" -text "Concentration (mass/m3)" 
radiobutton $wr.mass.d4 -variable cmass -value "1" -text "Mass (same as emission)" 
pack $wr.mass.d3 $wr.mass.d4

button $wr.end.quit  -bg red -text Quit -width 12 -command "set cpack $cp0; set cmass $cm0; destroy $wr"
button $wr.end.reset  -text Reset -width 12 -command "set cpack 1; set cmass 0"
button $wr.end.help -text "Help" -width 12 -command "load_html [file join $html_dir S626.htm ] "
button $wr.end.save  -bg green1 -text Save -width 12 -command "destroy $wr"

pack $wr.end.quit $wr.end.reset $wr.end.help $wr.end.save -side left  -padx 4
}


#-----------------------------------------------------------------------------
proc emission {} {
global qcycle efile html_dir

if [winfo exists .emit_win] {destroy .emit_win}
set wr .emit_win
toplevel $wr
wm title $wr "(6) Emission Cycling or Input File"
wm  geometry $wr +100+100

set qc0 $qcycle
set ef0 $efile

frame $wr.title
frame $wr.qcycle
frame $wr.efile
frame $wr.end

pack  $wr.title -pady 10
pack  $wr.qcycle
pack  $wr.efile
pack  $wr.end -pady 20

label $wr.title.lab -fg blue -relief raised -justify left -wraplength 5i \
-text "A non-zero value for the emission cycle will repeat the emission\
amount and duration as defined in the control file at this specified\
interval. Multiple emissions can also be configured from an input file\
the name of which is defined in this menu."
pack  $wr.title.lab

label $wr.qcycle.txt -fg blue -text "Set Emission Cycling Parameters"
pack  $wr.qcycle.txt -side top
label $wr.qcycle.lab -text "Repeat emission cycle (hr): " 
entry $wr.qcycle.ent -textvariable qcycle -width 6
pack $wr.qcycle.lab $wr.qcycle.ent -side left 

label $wr.efile.div -text "____________________________________"
label $wr.efile.txt -fg blue -text "Optional Point Source Emission File"
pack  $wr.efile.div $wr.efile.txt -side top

button $wr.efile.d0  -text "Clear Name" -width 12 -command {set efile " "}
button $wr.efile.d1  -text "Default Name" -width 12 -command {set efile "EMITIMES"}
entry  $wr.efile.ent -textvariable efile -width 15  
pack   $wr.efile.d0 $wr.efile.d1 $wr.efile.ent -side left -padx 2 

button $wr.end.quit  -bg red -text Quit -width 12 -command "set qcycle $qc0; set efile $ef0; destroy $wr"
button $wr.end.reset  -text Reset -width 12 -command "set qcycle 0.0; set efile { }"
button $wr.end.help -text "Help" -width 12 -command "load_html [file join $html_dir S624.htm ] "
button $wr.end.save  -bg green1 -text Save -width 12 -command "destroy $wr"

pack $wr.end.quit $wr.end.reset $wr.end.help $wr.end.save -side left -padx 4
}


#-----------------------------------------------------------------------------
proc turbdif {} {
global kpuff tkerd tkern html_dir kmixd kmix0 kzmix kdef kbls kblt
global hscale vscales vscaleu
if [winfo exists .turb_win] {destroy .turb_win}
set topwin .turb_win
toplevel $topwin
wm title $topwin "(7) Configure Turbulence Method"
wm  geometry $topwin +50+25

# create a scrollable frame (wr).
frame $topwin.outerframe
canvas $topwin.outerframe.canvas -yscrollcommand "$topwin.outerframe.yscroll set"
scrollbar $topwin.outerframe.yscroll -command "$topwin.outerframe.canvas yview"
frame $topwin.outerframe.canvas.innerframe
set wr $topwin.outerframe.canvas.innerframe

set kbl0 $kblt
set kdf0 $kdef
set kbs0 $kbls
set kzm0 $kzmix
set kmx0 $kmixd
set mix0 $kmix0
set kpf0 $kpuff
set tkd0 $tkerd
set tkn0 $tkern
set vcs0 $vscales
set vcu0 $vscaleu
set hca0 $hscale

frame $wr.title
frame $wr.turb
frame $wr.time
frame $wr.time2
frame $wr.time3
frame $wr.scale
frame $wr.zmix
frame $wr.mixd
frame $wr.kpuff 
frame $wr.tkea
frame $wr.tker
frame $wr.end

pack  $wr.title -pady 6 -padx 4
pack  $wr.turb $wr.time $wr.time2 $wr.time3 $wr.scale $wr.zmix $wr.mixd $wr.kpuff 
pack  $wr.tkea $wr.tker -pady 2
pack  $wr.end -pady 2

label $wr.title.lab -fg blue -relief raised -justify left -wraplength 6i \
-text "Sets the methods used to compute the turbulent velocities, boundary\
layer stability, mixed layer depth, and particle or puff dispersion rates.\
In general, these values should not be changed from their default values\
without detailed technical guidance."
pack  $wr.title.lab

#--> turbulence computation

frame $wr.turb.kblt
frame $wr.turb.kdef 
pack  $wr.turb.kblt $wr.turb.kdef -side left -padx 10

label $wr.turb.kblt.txt -fg blue -text "Vertical Turbulence"
radiobutton $wr.turb.kblt.d1 -variable kblt -value "1" -text "Beljaars-Holtslag  "  
radiobutton $wr.turb.kblt.d2 -variable kblt -value "2" -text "Kanthar-Clayson    " 
radiobutton $wr.turb.kblt.d3 -variable kblt -value "3" -text "Met Model TKE field"
radiobutton $wr.turb.kblt.d4 -variable kblt -value "4" -text "Measured variances " 
radiobutton $wr.turb.kblt.d5 -variable kblt -value "5" -text "Hanna              "
pack $wr.turb.kblt.txt $wr.turb.kblt.d1 $wr.turb.kblt.d2 $wr.turb.kblt.d3 $wr.turb.kblt.d4 $wr.turb.kblt.d5 -side top

label $wr.turb.kdef.txt -fg blue -text "Horizontal Turbulence"
radiobutton $wr.turb.kdef.d0 -variable kdef -value "0" -text "Proportional to vertical"  
radiobutton $wr.turb.kdef.d1 -variable kdef -value "1" -text "Velocity deformation    "
radiobutton $wr.turb.kdef.d2 -variable kdef -value "2" -text "Undefined               " 
radiobutton $wr.turb.kdef.d3 -variable kblt -value "4" -text "Measured variances      "  
pack $wr.turb.kdef.txt $wr.turb.kdef.d0 $wr.turb.kdef.d1 $wr.turb.kdef.d2 $wr.turb.kdef.d3 -side top

#-->Lagrangian Time Scales

label $wr.time.txt -fg blue -text "Lagrangian Time Scales    "
pack  $wr.time.txt -side top
label $wr.time2.txt -fg blue -text "Set Stable=-1 for time and space varying Hanna vertical Lagrangian time scale"
pack  $wr.time2.txt -side top
label $wr.time3.txt -fg blue -text "Unstable setting not used when Stable=-1"
pack  $wr.time3.txt -side top

frame $wr.scale.vmix
frame $wr.scale.hmix 
pack  $wr.scale.vmix $wr.scale.hmix -side left -padx 10

label $wr.scale.vmix.txt  -text "Stable/Unstable (s): "
pack  $wr.scale.vmix.txt  
entry $wr.scale.vmix.ent1 -textvariable vscales -relief sunken -width 8
entry $wr.scale.vmix.ent2 -textvariable vscaleu -relief sunken -width 8
pack  $wr.scale.vmix.ent1 $wr.scale.vmix.ent2 -side left -padx 4

label $wr.scale.hmix.txt -text "All stability (s):        "
pack  $wr.scale.hmix.txt 
entry $wr.scale.hmix.ent -textvariable hscale -relief sunken -width 16
pack  $wr.scale.hmix.ent -side left

#--stability and mixing

label $wr.zmix.div -text "____________________________________"
pack  $wr.zmix.div -side top

frame $wr.zmix.kbls
frame $wr.zmix.kzmix
pack  $wr.zmix.kbls $wr.zmix.kzmix -side left

label $wr.zmix.kbls.txt -fg blue -text "Boundary Layer Stability"
radiobutton $wr.zmix.kbls.d1 -variable kbls -value "1" -text "Heat and momentum fluxes  "  
radiobutton $wr.zmix.kbls.d2 -variable kbls -value "2" -text "Computed from U/T profile " 
pack $wr.zmix.kbls.txt $wr.zmix.kbls.d1 $wr.zmix.kbls.d2 -side top

label $wr.zmix.kzmix.txt -fg blue -text "Vertical Mixing Profile"
radiobutton $wr.zmix.kzmix.d0 -variable kzmix -value "0" -text "Varies with height in PBL"  
radiobutton $wr.zmix.kzmix.d1 -variable kzmix -value "1" -text "Replaced by PBL average  " 
pack $wr.zmix.kzmix.txt $wr.zmix.kzmix.d0 $wr.zmix.kzmix.d1 -side top

#-->mixed layer depth

label $wr.mixd.div -text "____________________________________"
label $wr.mixd.txt -fg blue -text "Mixed Layer Depth Computation"
pack  $wr.mixd.div $wr.mixd.txt -side top

frame $wr.mixd.kmixd
frame $wr.mixd.value
pack  $wr.mixd.kmixd $wr.mixd.value -side left -padx 10

radiobutton $wr.mixd.kmixd.d0 -text "Use meteorological model" -variable kmixd -value "0" 
radiobutton $wr.mixd.kmixd.d1 -text "From temperature profile" -variable kmixd -value "1" 
radiobutton $wr.mixd.kmixd.d2 -text "Compute from TKE profile" -variable kmixd -value "2"
radiobutton $wr.mixd.kmixd.d3 -text "modified Richardson #   " -variable kmixd -value "3"
pack $wr.mixd.kmixd.d0 $wr.mixd.kmixd.d1 $wr.mixd.kmixd.d2 $wr.mixd.kmixd.d3

frame $wr.mixd.value.top
frame $wr.mixd.value.mid
frame $wr.mixd.value.bot 
pack $wr.mixd.value.top $wr.mixd.value.mid $wr.mixd.value.bot -side top

radiobutton $wr.mixd.value.top.con -text "Set as constant (m):" -variable kmixd -value "1500"
entry $wr.mixd.value.top.ent -textvariable kmixd -relief sunken -width 5
pack  $wr.mixd.value.top.con $wr.mixd.value.top.ent -side left

label $wr.mixd.value.mid.txt -text "----------------------------"
pack  $wr.mixd.value.mid.txt

label $wr.mixd.value.bot.txt -text "   Default minimum (m):"
entry $wr.mixd.value.bot.min -textvariable kmix0 -relief sunken -width 5
pack  $wr.mixd.value.bot.txt $wr.mixd.value.bot.min -side left

label $wr.kpuff.div -text "____________________________________"
label $wr.kpuff.txt -fg blue -text "Puff Growth Computation Method"
pack  $wr.kpuff.div $wr.kpuff.txt -side top

label $wr.kpuff.lab -text "     "
radiobutton $wr.kpuff.p0 -variable kpuff -value "0" -text "Linear  " 
radiobutton $wr.kpuff.p1 -variable kpuff -value "1" -text "Empirical" 
pack $wr.kpuff.lab $wr.kpuff.p0 $wr.kpuff.p1 -side left

label $wr.tkea.div -text "____________________________________"
label $wr.tkea.txt -fg blue -text "Turbulence Aniosotropy Factors"
pack  $wr.tkea.div $wr.tkea.txt -side top

button $wr.tkea.p0 -width 10 -text "None " -command {set tkerd 0.0;  set tkern 0.0}
button $wr.tkea.p1 -width 10 -text "Force" -command {set tkerd 0.18; set tkern 0.18}
button $wr.tkea.p2 -width 10 -text "Urban" -command {set tkerd 0.18; set tkern 0.22}
pack $wr.tkea.p0 $wr.tkea.p1 $wr.tkea.p2 -side left
 
label  $wr.tker.lab1 -text "Day:"
entry  $wr.tker.ent1 -textvariable tkerd -width 5
label  $wr.tker.lab2 -text "Night:"
entry  $wr.tker.ent2 -textvariable tkern -width 5 
pack $wr.tker.lab1 $wr.tker.ent1 -side left -padx 2
pack $wr.tker.lab2 $wr.tker.ent2 -side left -padx 2

label  $wr.end.div -text "____________________________________"
pack   $wr.end.div -pady 5
button $wr.end.quit  -bg red  -text Quit -width 12 -command \
  "set kblt $kblt; set kdef $kdef; set kbls $kbls; set kzmix $kzmix; \
   set kmixd $kmixd; set kmix0 $mix0; set kpuff $kpuff; set tkerd $tkd0; set tkern $tkn0; \
set vscales $vcs0; set vscaleu $vcu0; set hscale $hca0; destroy $topwin"

button $wr.end.reset  -text Reset -width 12 -command \
  "set kpuff 0; set kmixd 0; set kmix0 150; set kzmix 0; set kdef 0; set kbls 1; set kblt 0; \
   set tkerd 0.18; set tkern 0.18; set vscales 0.0; set vscaleu 200.0; set hscale 10800.0"
button $wr.end.help -text "Help" -width 12 -command "load_html [file join $html_dir S625.htm ] "
button $wr.end.save  -bg green1 -text Save -width 12 -command "destroy $topwin"

pack $wr.end.quit $wr.end.reset $wr.end.help $wr.end.save -side left  -padx 4

# scrollable frame business
pack $topwin.outerframe.canvas.innerframe -expand yes -fill both -side top
pack $topwin.outerframe.yscroll -side right -fill y
$topwin.outerframe.canvas create window 0 0 -anchor nw -window $topwin.outerframe.canvas.innerframe
$topwin.outerframe.canvas configure -scrollregion [$topwin.outerframe.canvas bbox all]
pack $topwin.outerframe.canvas -expand yes -fill both -side top
pack $topwin.outerframe -expand yes -fill both -side top
bind $topwin.outerframe <Map> {
   set topwin .turb_win
   # assume only 80% of the screen height is available.
   set content_height [winfo height $topwin.outerframe.canvas.innerframe]
   set avail_scr_height [expr 0.80 * [winfo screenheight .]]
   if {$content_height >= $avail_scr_height} {
      set view_height $avail_scr_height
   } else {
      set view_height $content_height
   }
   $topwin.outerframe.canvas.innerframe configure -height $content_height
   $topwin.outerframe.canvas configure -width 650 -height $view_height
   $topwin.outerframe.canvas configure -scrollregion [$topwin.outerframe.canvas bbox all]
}

}


#-----------------------------------------------------------------------------
proc pardump {} {
global ninit ndump ncycl pinpf poutf html_dir
if [winfo exists .pdump_win] {destroy .pdump_win}
set wr .pdump_win
toplevel $wr
wm title $wr "(9) Input and Output of Particle Files"
wm  geometry $wr +100+100

set nini0 $ninit
set ndmp0 $ndump
set ncyc0 $ncycl
set pinp0 $pinpf
set poutf $poutf

frame $wr.title
frame $wr.par1
frame $wr.par2
frame $wr.par3
frame $wr.par4
frame $wr.par5
frame $wr.end

pack  $wr.title -pady 10
pack  $wr.par1
pack  $wr.par2 $wr.par3 $wr.par4 $wr.par5
pack  $wr.end -pady 20

label $wr.title.lab -fg blue -relief raised -justify left -wraplength 5i \
-text "Set the options to read and write the particle position endpoint\
file. The particle file can be used to initialize a simulation or the\
file can be read by one of the particle display programs."
pack  $wr.title.lab

label $wr.par1.txt -fg blue -text "Particle File Initialization Options"
radiobutton $wr.par1.d0 -variable ninit -value "0" -text "None                                     "  
radiobutton $wr.par1.d1 -variable ninit -value "1" -text "Read file once at startup if it exists   " 
radiobutton $wr.par1.d2 -variable ninit -value "2" -text "Read file each hour and add particles    " 
radiobutton $wr.par1.d3 -variable ninit -value "3" -text "Read file each hour and replace particles" 
pack $wr.par1.txt -side top
pack $wr.par1.d0 $wr.par1.d1 $wr.par1.d2 $wr.par1.d3 -side top

label $wr.par1.lab -text "   Initialization file name: "
entry $wr.par1.ent -textvariable pinpf -relief sunken -width 10
pack $wr.par1.lab $wr.par1.ent -side left 

label $wr.par2.div -text "____________________________________"
label $wr.par2.txt0 -fg blue -text "Particle File Output Options"
pack  $wr.par2.div $wr.par2.txt0 -side top

label $wr.par3.lab1 -text "    First output (hrs): "
entry $wr.par3.ent1 -textvariable ndump -relief sunken -width 4
pack  $wr.par3.lab1 $wr.par3.ent1 -side left

label $wr.par4.lab1 -text " Repeat interval (hrs): "
entry $wr.par4.ent1 -textvariable ncycl -relief sunken -width 4
pack  $wr.par4.lab1 $wr.par4.ent1 -side left 

label $wr.par5.lab1 -text "Output file name: "
entry $wr.par5.ent1 -textvariable poutf -relief sunken -width 10
pack  $wr.par5.lab1 $wr.par5.ent1 -side left

button $wr.end.quit  -bg red -text Quit -width 12 -command \
"set ninit $nini0; set ndump $ndmp0; set ncycl $ncyc0; set pinpf $pinp0; set poutf $poutf; destroy $wr"
button $wr.end.reset  -text Reset -width 12 -command \
"set ninit 1; set ndump 0; set ncycl 0; set pinpf PARINIT; set poutf PARDUMP"
button $wr.end.help -text "Help" -width 12 -command "load_html [file join $html_dir S627.htm ] "
button $wr.end.save  -bg green1 -text Save -width 12 -command "destroy $wr"

pack $wr.end.quit $wr.end.reset $wr.end.help $wr.end.save -side left  -padx 15
}


#-----------------------------------------------------------------------------
proc chemods {} {
global ichem maxdim html_dir crate
if [winfo exists .chem_win] {destroy .chem_win}
set wr .chem_win
toplevel $wr
wm title $wr "(10) In-Line Chemical Conversion Modules"
wm  geometry $wr +100+100

set ichem0 $ichem
set maxpd0 $maxdim

frame $wr.title
frame $wr.chem1
frame $wr.chem2
frame $wr.chem
frame $wr.part
frame $wr.end

pack  $wr.title -pady 8
pack  $wr.chem1 $wr.chem2 $wr.chem -side top -anchor w -padx 10
pack  $wr.part
pack  $wr.end -side top -pady 15

label $wr.title.lab -fg blue -relief raised -justify left -wraplength 6i \
-text "Several conversion modules can be called from within the code during\
the transport and dispersion calculation that may alter the model physics,\
parameterizations, or output data structure. Currently only one option may\
be selected for a simulation. See the User's Guide for more information."
pack  $wr.title.lab

radiobutton $wr.chem1.d0 -variable ichem -value "0" -text "DEFAULT - no conversions enabled"  
radiobutton $wr.chem1.d1 -variable ichem -value "1" -text "Restructure the concentration grid to the source-receptor format " 
pack $wr.chem1.d0 $wr.chem1.d1 -anchor w

radiobutton $wr.chem2.d2a -variable ichem -value "2" -text "Convert species 1 to species 2 at " 
entry $wr.chem2.ent -textvariable crate -relief sunken -width 4
label $wr.chem2.d2b -text " per hour"
pack $wr.chem2.d2a $wr.chem2.ent $wr.chem2.d2b -side left -anchor w

radiobutton $wr.chem.d3 -variable ichem -value "3" -text "Enable the PM10 dust storm emission algorithm for desert landuse " 
radiobutton $wr.chem.d5 -variable ichem -value "5" -text "Deposit particles rather than reducing the mass of each particle " 
radiobutton $wr.chem.d6 -variable ichem -value "6" -text "Divide output mass by air density (kg/m3) to sum the mixing ratio" 
radiobutton $wr.chem.d7 -variable ichem -value "7" -text "Enable deposited particles to be transported on the ocean surface" 
radiobutton $wr.chem.d8 -variable ichem -value "8" -text "STILT mode: mixing ratio and set layer-1 to PBL fraction" 
radiobutton $wr.chem.d9 -variable ichem -value "9" -text "Concentration layer-1 set as fraction of mixed layer (hundreds)" 
radiobutton $wr.chem.d10 -variable ichem -value "10" -text "Concentration grid as time-varying Transfer Coefficient Matrix" 
radiobutton $wr.chem.d11 -variable ichem -value "11" -text "Enable daughter product calculation" 

pack $wr.chem.d3 $wr.chem.d5 $wr.chem.d6 \
     $wr.chem.d7 $wr.chem.d8 $wr.chem.d9 $wr.chem.d10 $wr.chem.d11 -side top -anchor w

label $wr.part.div -text "____________________________________"
label $wr.part.txt0 -fg blue -text "Additional Chemistry Options"
pack  $wr.part.div $wr.part.txt0 -side top

label $wr.part.lab1 -text " Single particle mass dimension: "
entry $wr.part.ent1 -textvariable maxdim -relief sunken -width 4
pack  $wr.part.lab1 $wr.part.ent1 -side left

button $wr.end.quit  -bg red -text Quit -width 12 -command "set ichem $ichem0; set maxdim $maxpd0; destroy $wr"
button $wr.end.reset  -text Reset -width 12 -command {
   set ichem 0; set maxdim 1; set crate 0.10
   if [file exists CHEMRATE.TXT] {file delete CHEMRATE.TXT}
   }
button $wr.end.help -text "Help" -width 12 -command "load_html [file join $html_dir S628.htm ] "
button $wr.end.save  -bg green1 -text Save -width 12 -command {
  if {$ichem == 2} {chemwrite}
  destroy .chem_win}
pack $wr.end.quit  $wr.end.reset $wr.end.help $wr.end.save -side left  -padx 4
}

#----------------------------------------------------------------------------
proc chemread {} {
global crate
if [file exists CHEMRATE.TXT] {
   set f [open CHEMRATE.TXT r]
   gets $f temp
   close $f

   set pol1  [lindex $temp 0] 
   set pol2  [lindex $temp 1] 
   set crate [lindex $temp 2] 
   set cgmw  [lindex $temp 3]

} else {
   set crate 0.10
}
}

#----------------------------------------------------------------------------
proc chemwrite {} {
global crate
if [file exists CHEMRATE.TXT] {
   set f [open CHEMRATE.TXT r]
   gets $f temp
   close $f

   set pol1 [lindex $temp 0] 
   set pol2 [lindex $temp 1]
   set dumy [lindex $temp 2] 
   set cgmw [lindex $temp 3] 

} else {
   set pol1 1
   set pol2 2
   set cgmw 1.0
}

# use crate from menu when defined
if [ info exists crate ] { } else {set crate ""} 
if { $crate == "" } {set crate 0.10}

set f [open CHEMRATE.TXT w]
puts $f "$pol1 $pol2 $crate $cgmw"
close $f
}

#-----------------------------------------------------------------------------
proc ensemble {} {
global member1 member2 dxf dyf dzf html_dir
if [winfo exists .ens_win] {destroy .ens_win}
set wr .ens_win
toplevel $wr
wm title $wr "(11) Meteorogical Ensemble Configuration"
wm  geometry $wr +100+100

set mb1 $member1
set mb2 $member2
set dx0 $dxf
set dy0 $dyf
set dz0 $dzf

frame $wr.title
frame $wr.val1
frame $wr.val2
frame $wr.val3
frame $wr.end

pack  $wr.title -pady 5
pack  $wr.val1 $wr.val2 $wr.val3 -side top -anchor w -pady 5
pack  $wr.end -side top -pady 5

label $wr.title.lab -fg blue -relief raised -justify left -wraplength 5i \
-text "Menu to configure the meteorological ensemble by setting the horizontal\
grid point offset in grid units and the vertical offset in sigma units. One\
sigma unit scales to about 25 km. The size of a horizontal grid point depends\
upon the meteorological data set used in the computation. Vertical shift\
dz=0, >0, and <0 applies to member numbers 1-9, 10-18, and 19-27."
pack  $wr.title.lab

label $wr.val1.lab1 -text " Starting and Ending Member Numbers:"
entry $wr.val1.ent1 -textvariable member1 -relief sunken -width 4
label $wr.val1.lab2 -text "to "
entry $wr.val1.ent2 -textvariable member2 -relief sunken -width 4

label $wr.val2.lab1 -text " Horizontal Shift (grid points):  X "
entry $wr.val2.ent1 -textvariable dxf -relief sunken -width 4
label $wr.val2.lab2 -text " Y "
entry $wr.val2.ent2 -textvariable dyf -relief sunken -width 4

label $wr.val3.lab1 -text " Vertical Shift (z sigma units):  Z "
entry $wr.val3.ent1 -textvariable dzf -relief sunken -width 4

pack  $wr.val1.lab1 $wr.val1.ent1 $wr.val1.lab2 $wr.val1.ent2 -side left -padx 2
pack  $wr.val2.lab1 $wr.val2.ent1 $wr.val2.lab2 $wr.val2.ent2 -side left -padx 2
pack  $wr.val3.lab1 $wr.val3.ent1 -side left -padx 2

button $wr.end.quit  -bg red  -text Quit -width 12 -command \
       "set member1 $mb1; set member2 $mb2; set dxf $dx0; set dyf $dy0; set dzf $dz0; destroy $wr"
button $wr.end.reset  -text Reset -width 12 -command \
       "set member1 1; set member2 27; set dxf 1.0; set dyf 1.0; set dzf 0.01"
button $wr.end.help -text "Help" -width 12 -command "load_html [file join $html_dir S615.htm ] "
button $wr.end.save  -bg green1 -text Save -width 12 -command "destroy $wr"

pack $wr.end.quit  $wr.end.reset $wr.end.help $wr.end.save -side left  -padx 4
}


#-----------------------------------------------------------------------------
proc cmtout {} {
global cmtfn html_dir
if [winfo exists .cmtout_win] {destroy .cmtout_win}
set wr .cmtout_win
toplevel $wr
wm title $wr "(12) Output Center-of-Mass Trajectory"
wm  geometry $wr +100+100

set cmtfn $cmtfn

frame $wr.title
frame $wr.par1
frame $wr.par2
frame $wr.par3
frame $wr.par4
frame $wr.par5
frame $wr.end

pack  $wr.title -pady 10
pack  $wr.par1
pack  $wr.par2 $wr.par3 $wr.par4 $wr.par5
pack  $wr.end -pady 20

label $wr.title.lab -fg blue -relief raised -justify left -wraplength 5i \
-text "Set the file name to write the particle center-of-mass trajectory.\
The center-of-mass trajectory can be read by one of the trajectory display\
programs. If no center-of-mass trajectory output is wanted, use blank for\
the file name."
pack  $wr.title.lab

label $wr.par1.txt -fg blue -text "Center-of-Mass Trajectory Options"
label $wr.par1.lab -text "   Output file name: "
entry $wr.par1.ent -textvariable cmtfn -relief sunken -width 15
pack $wr.par1.lab $wr.par1.ent -side left 

button $wr.end.quit  -bg red -text Quit -width 12 -command \
"set cmtfn $cmtfn; destroy $wr"
button $wr.end.reset  -text Reset -width 12 -command \
"set cmtfn { }"
button $wr.end.help -text "Help" -width 12 -command "load_html [file join $html_dir S631.htm ] "
button $wr.end.save  -bg green1 -text Save -width 12 -command "destroy $wr"

pack $wr.end.quit $wr.end.reset $wr.end.help $wr.end.save -side left  -padx 15
}


#--------------------------------------------------------------------------

proc wrfvert {} {
global wvert html_dir
if [winfo exists .wrfvert_win] {destroy .wrfvert_win}
set wr .wrfvert_win
toplevel $wr
wm title $wr "(13) WRF Vertical Interpolation"
wm  geometry $wr +100+100

set wvert $wvert

frame $wr.title
frame $wr.par1
frame $wr.par2
frame $wr.par3
frame $wr.par4
frame $wr.par5
frame $wr.end

pack  $wr.title -pady 10
pack  $wr.par1
pack  $wr.par2 $wr.par3 $wr.par4 $wr.par5
pack  $wr.end -pady 20

label $wr.title.lab -fg blue -relief raised -justify left -wraplength 6i \
-text "Vertical interpolation algorithm for meteorological input files \
derived from WRF can be computed using the default HYSPLIT interpolation \
scheme for pressure-sigma coordinate meteorological input files or a WRF \
vertical interpolation scheme. The WRF vertical interpolation scheme \
mimics the vertical coordinate transformations within the WRF model. \
The difference between the two schemes is how they estimate the change \
in height bewteen verical levels. The HYSPLIT scheme calculates the \
change in height using the hypsometric equation while the WRF scheme \
uses the eta level, WRF dry mass, and WRF dry inverse density."
pack  $wr.title.lab

label $wr.par1.txt -fg blue -text "Vertical interpolation scheme for WRF fields"
radiobutton $wr.par1.d1 -variable wvert -value .FALSE. -text "HYSPLIT scheme                     "
radiobutton $wr.par1.d2 -variable wvert -value .TRUE.  -text "WRF scheme                         "
pack $wr.par1.txt -side top
pack $wr.par1.d1 $wr.par1.d2 -side top

button $wr.end.quit  -bg red -text Quit -width 13 -command \
"set wvert $wvert; destroy $wr"
button $wr.end.reset  -text Reset -width 13 -command \
"set wvert .TRUE."
button $wr.end.help -text "Help" -width 13 -command "load_html [file join $html_dir S632.htm ] "
button $wr.end.save  -bg green1 -text Save -width 13 -command "destroy $wr"

pack $wr.end.quit $wr.end.reset $wr.end.help $wr.end.save -side left  -padx 15
}

#--------------------------------------------------------------------------

proc clear_config {} {
if [file exists SETUP.CFG] {file delete -force SETUP.CFG}
reset_config
}

#--------------------------------------------------------------------------
proc reset_config {} {
global initd khmax numpar qcycle delt tratio ndump ncycl tkerd \
       mgmin kmsl tset maxpar cpack dxf dyf dzf ichem maxdim tkern \
       cmass ninit pinpf poutf kpuff efile p10f member1 member2 \
       kspl krnd frhs frvs frts frhmax splitf kmixd kmix0 kzmix \
       kdef kbls kblt idsp conage gemage crate cnumb pakval Grid_name \
       hscale vscales vscaleu cmtfn wvert

set member1 1
set member2 27
set initd 0
set kpuff 0 
set kmixd -1
set kmix0 150
set kzmix 0
set kdef 0
set kbls 1
set kblt 0
set idsp 1
set conage 24
set gemage 48
set tkerd 0.18
set tkern 0.18
set hscale 10800.0
set vscales 0.0
set vscaleu 200.0
set numpar 2500
set maxpar 10000
set khmax 9999
set qcycle 0.0
set efile ""
set delt 0
set tratio 0.75
set mgmin 10
set kmsl 0
set tset 2
set cmass 0
set dxf 1.0
set dyf 1.0
set dzf 0.01
set p10f 1.0
set ichem 0
set maxdim 1
set kspl 1
set krnd 6
set frhs 1.0
set frvs 0.01
set frts 0.10
set frhmax 3.0
set splitf 1.0
set ninit 1
set ndump 0
set ncycl 0
set pinpf PARINIT
set poutf PARDUMP
set crate 0.10
set cmtfn " "
set wvert .TRUE.

set cnumb 0
set pakval 1
set cpack ""
foreach item $Grid_name {
   append cpack $pakval ","
   incr cnumb
}

if [file exists CONC.CFG] {file delete CONC.CFG}
}


#--------------------------------------------------------------------------
proc load_config {Fname Wname} {
global initd khmax numpar qcycle delt tratio ndump ncycl \
       mgmin kmsl tset maxpar cpack dxf dyf dzf ichem maxdim  \
       cmass ninit pinpf poutf kpuff efile p10f tkerd tkern \
       kspl krnd frhs frvs frts frhmax splitf kmixd kmix0 \
       kzmix kdef kbls kblt idsp conage gemage hscale vscales vscaleu \
       cmtfn wvert

set delt 0

if [file exists $Fname] {
   set f [open "$Fname" r]
   gets $f cline
   while {[gets $f cline] >= 0} {
      set pline [string trim $cline]
      set cline [split $pline]

      if {[llength $cline] >= 3} {
         set local [string tolower [lindex $cline 0]]

         set nvarb 2
         while {$nvarb < [llength $cline]-1 && [string length [lindex $cline $nvarb]] == 0} {
            incr nvarb
         }

         if { "$local" == "cpack" } {
            regsub -all , [lindex $cline $nvarb] , value
#diag       msg_box "$local $value"
#diag       tkwait window .msg_win
          } else {
            regsub -all , [lindex $cline $nvarb] {} value
         }

         if [catch {expr $value}] {
            regsub -all ' $value {} value2
            set $local $value2
         } else {
            set $local [expr $value]
         }
      }
   }
   close $f

   if {$delt == 0} { 
     set tset 2
   } else {
     set tset 1
   } 
   if [info exists p10f]   { } else {set p10f  1.0} 
   if [info exists maxdim] { } else {set maxdim  1} 
   if [info exists cmtfn]  { } else {set cmtfn { }} 
   if [info exists wvert]  { } else {set wvert .TRUE.}
}
if [winfo exists $Wname] {destroy $Wname}
}


#------------------------------------------------------------------------------
proc save_config {Fname Wname} {
global initd khmax numpar qcycle delt tratio ndump ncycl \
       mgmin kmsl tset maxpar cpack dxf dyf dzf ichem maxdim  \
       cmass ninit pinpf poutf kpuff efile p10f tkerd tkern \
       kspl krnd frhs frvs frts frhmax splitf kmixd kmix0 \
       kzmix kdef kbls kblt idsp conage gemage hscale vscales vscaleu \
       cmtfn wvert

global Num_pollut 

set f [open "$Fname" w]
puts $f " &SETUP"
if { $tset == 1} {puts $f " delt = $delt,"}
if { $tset == 2} {puts $f " tratio = $tratio,"}
puts $f " initd = $initd," 
puts $f " kpuff = $kpuff,"
puts $f " khmax = $khmax,"
puts $f " kmixd = $kmixd,"
puts $f " kmix0 = $kmix0,"
puts $f " kzmix = $kzmix,"
puts $f " kdef = $kdef,"
puts $f " kbls = $kbls,"
puts $f " kblt = $kblt,"
puts $f " idsp = $idsp,"
puts $f " conage = $conage,"
puts $f " gemage = $gemage,"
puts $f " numpar = $numpar,"
puts $f " qcycle = $qcycle,"
puts $f " efile = '$efile',"
puts $f " tkerd = $tkerd,"
puts $f " tkern = $tkern,"
puts $f " hscale = $hscale,"
puts $f " vscales = $vscales,"
puts $f " vscaleu = $vscaleu,"
puts $f " ninit = $ninit,"
puts $f " ndump = $ndump,"
puts $f " ncycl = $ncycl,"
puts $f " pinpf = '$pinpf',"
puts $f " poutf = '$poutf',"
puts $f " mgmin = $mgmin,"
puts $f " kmsl = $kmsl,"
puts $f " maxpar = $maxpar,"
puts $f " cpack = $cpack"
puts $f " cmass = $cmass,"
puts $f " dxf = $dxf,"
puts $f " dyf = $dyf,"
puts $f " dzf = $dzf,"
puts $f " ichem = $ichem,"
if { $ichem == 0 } {puts $f " maxdim = $maxdim,"}
if { $ichem == 2 } {puts $f " maxdim = $Num_pollut,"}
if { $ichem == 3 } {puts $f " p10f = $p10f,"}
if { $ichem == 11 } {puts $f " maxdim = $maxdim,"}
puts $f " kspl = $kspl,"
puts $f " krnd = $krnd,"
puts $f " frhs = $frhs,"
puts $f " frvs = $frvs,"
puts $f " frts = $frts,"
puts $f " frhmax = $frhmax,"
puts $f " splitf = $splitf,"
puts $f " cmtfn = '$cmtfn',"
puts $f " wvert = $wvert,"
puts $f " /"
close $f

file copy -force $Fname CONC.CFG
if [winfo exists $Wname] {destroy $Wname}
}


#--------------------------------------------------------------------------
proc save_as_def {} {
if [winfo exists .savedir] {destroy .savedir}
global html_dir Save_path
set wr .savedir
toplevel $wr
wm title $wr "Save Configuration by Name"
wm  geometry $wr +150+150
set Name_save " "

frame $wr.win
frame $wr.typ
frame $wr.bot
pack $wr.win $wr.typ $wr.bot -side top -pady 10

label $wr.win.lft -text "Enter Path/Name to save configuration"
button $wr.win.rgt -text Browse -width 8 -command {
   set temp [tk_getSaveFile -initialdir $Save_path -title "File Selection"]
   if {[string length $temp] > 0} {
      set Name_save $temp
      set Save_path [file dirname $temp]
      }
}
pack $wr.win.lft $wr.win.rgt -side left -padx 5

entry $wr.typ.ent -textvariable Name_save -relief sunken -width 45
pack $wr.typ.ent -side left

button $wr.bot.dismiss  -bg red -text "Quit" -width 8 -command {destroy .savedir}
button $wr.bot.help -text "Help" -width 8 \
       -command "load_html [file join $html_dir S211.htm ] "
button $wr.bot.save  -bg green1 -text "Save" -width 8 -command {save_config $Name_save .savedir}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 5 
bind $wr.typ.ent <Return> {save_config $Name_save .savedir}
}


#--------------------------------------------------------------------------------
proc load_as_def {} {
if [winfo exists .loaddir] {destroy .loaddir}
global html_dir Save_path
set wr .loaddir
toplevel $wr
wm title $wr "Load Previously Saved Configuration"
wm  geometry $wr +150+150
set Name_load " "

frame $wr.win
frame $wr.txt
frame $wr.bot
pack $wr.win $wr.txt $wr.bot -side top -pady 10

label $wr.win.lft -text "Enter Path/Name to load configuration"
button $wr.win.rgt -text Browse -width 8 -command {
   set temp [tk_getOpenFile -initialdir $Save_path -title "File Selection"]
   if {[string length $temp] > 0} {
      set Name_load $temp
      set Save_path [file dirname $temp]
      }
}
pack $wr.win.lft $wr.win.rgt -side left -padx 5

entry $wr.txt.ent -textvariable Name_load -relief sunken -width 45
pack $wr.txt.ent -side left

button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help  -width 8 \
       -command "load_html [file join $html_dir S211.htm ] "
button $wr.bot.save  -bg green1 -text "OK " -width 8 -command {load_config $Name_load .loaddir}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 5 
bind $wr.txt.ent <Return> {load_config $Name_load .loaddir}
}
