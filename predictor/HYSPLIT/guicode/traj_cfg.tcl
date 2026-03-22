proc cfgtraj_init { } {

#-----------------------------------------------------------------------------
# TRAJ_CFG.TCL: setup.cfg namelist file configuration script
# Last Revised: 23 Jul 2002
#               13 Aug 2002 - html help 
#               13 Sep 2002 - vertical trajectory split
#               07 May 2004 - trajectory duration maximum
#               27 May 2005 - browse widgets 
#               01 Jun 2005 - terrain and shortwave flux
#               24 May 2006 - less text
#               14 Nov 2006 - added submenus
#               05 Mar 2007 - updated help
#               05 Nov 2008 - version 4.9 updates
#               14 Aug 2009 - enhanced window titles
#               05 Nov 2010 - patched read of TRAJ.CFG to test for apostrophe
#               01 Nov 2011 - added specific humidity and water mixing ratio
#               22 Jul 2013 - minor label change
#               13 Jun 2014 - delete TRAJ.CFG when pressing delete
#               14 Sep 2016 - added Save_path
#               01 Jun 2021 - change wvert default to true
#-----------------------------------------------------------------------------
      
global html_dir 

if [winfo exists .configinit] {destroy .configinit}
set wr .configinit
toplevel $wr
wm title $wr "Create Optional Trajectory Namelist File: SETUP.CFG"
wm  geometry $wr +50+50

frame $wr.step 
frame $wr.metg
frame $wr.nstr
frame $wr.tout
frame $wr.mixd
frame $wr.tvar
frame $wr.ensb
frame $wr.wver
frame $wr.exit

pack $wr.step $wr.nstr $wr.metg $wr.tout $wr.mixd $wr.tvar $wr.ensb $wr.wver -pady 4
pack $wr.exit -side top -pady 12

label  $wr.step.txt -text "       Set fixed or automatic TIME STEPS (1): " 
button $wr.step.but  -text "Menu" -width 6 -command "tsteps"
pack   $wr.step.txt $wr.step.but -side left

label  $wr.nstr.txt -text "        Define subgrid and MSL/AGL UNITS (2): "
button $wr.nstr.but  -text "Menu" -width 6 -command "metgrd"
pack   $wr.nstr.txt $wr.nstr.but -side left

label  $wr.metg.txt -text "           MULTIPLE trajectories in time (3): " 
button $wr.metg.but  -text "Menu" -width 6 -command "restart"
pack   $wr.metg.txt $wr.metg.but -side left

label  $wr.tout.txt -text "      Trajectory points OUTPUT FREQUENCY (4): "
button $wr.tout.but   -text "Menu" -width 6 -command "trjfile"
pack   $wr.tout.txt $wr.tout.but -side left

label  $wr.mixd.txt -text "         MIXING DEPTH computation method (5): "
button $wr.mixd.but  -text "Menu" -width 6 -command "mixdepth"
pack   $wr.mixd.txt $wr.mixd.but -side left

label  $wr.tvar.txt -text " Add METEOROLOGY output along trajectory (6): "
button $wr.tvar.but  -text "Menu" -width 6 -command "trajmet"
pack   $wr.tvar.txt $wr.tvar.but -side left

label  $wr.ensb.txt -text "     Meteorological grid offset ENSEMBLE (7): "
button $wr.ensb.but  -text "Menu" -width 6 -command "ensemble"
pack   $wr.ensb.txt $wr.ensb.but -side left

label  $wr.wver.txt -text "              WRF Vertical Interpolation (8): "
button $wr.wver.but  -text "Menu" -width 6 -command "wrfvert"
pack   $wr.wver.txt $wr.wver.but -side left

#-->termination

label  $wr.exit.top -text "--------------------------------------------------------"
pack   $wr.exit.top -side top
button $wr.exit.dismiss  -bg red -text "Quit" -width 12 -command "destroy $wr"
button $wr.exit.reset  -text "Reset" -width 6 -command "clear_config"
button $wr.exit.help -text "Help" -width 6 \
       -command "load_html [file join $html_dir S411.htm ] "
button $wr.exit.save  -text "Save as" -width 8 -command "save_as_def"
button $wr.exit.load  -text "Retrieve" -width 8 -command "load_as_def"
button $wr.exit.cont  -bg green -text "Save" -width 12 -command "save_config SETUP.CFG $wr"
pack  $wr.exit.dismiss $wr.exit.reset $wr.exit.help $wr.exit.save \
      $wr.exit.load $wr.exit.cont -side left -padx 4

if [file exists TRAJ.CFG] {
   load_config TRAJ.CFG .dummy
   } else {
   reset_config
   }
}


#-----------------------------------------------------------------------------
proc trjfile {} {
global tout khmax html_dir 

if [winfo exists .tout_win] {destroy .tout_win}
set wr .tout_win
toplevel $wr
wm title $wr "(4) Trajectory Points Output Frequency"
wm  geometry $wr +100+100

set tf0 $tout
set kh0 $khmax

frame $wr.title
frame $wr.val1
frame $wr.val2
frame $wr.end

pack  $wr.title -pady 10
pack  $wr.val1 $wr.val2
pack  $wr.end -pady 20

label $wr.title.lab -fg blue -relief raised -justify left -wraplength 6i \
-text "The default output interval (60 min) for trajectory endpoints can\
be increased or decreased. Values less than 60 may require a fixed time\
step. The maximum trajectory duration is indpendent from the multiple\
trajectory duration."
pack  $wr.title.lab

label $wr.val1.lab -text "    Endpoint write interval (min): "
entry $wr.val1.ent -textvariable tout -relief sunken -width 4
pack  $wr.val1.lab $wr.val1.ent -side left

label $wr.val2.lab -text "Maximum trajectory duration (hrs): "
entry $wr.val2.ent -textvariable khmax -width 4
pack  $wr.val2.lab $wr.val2.ent -side left

button $wr.end.quit  -bg red -text Quit -width 12 -command "set tout $tf0; set khmax $kh0; destroy $wr"
button $wr.end.reset  -text Reset -width 12 -command "set tout 0; set khmax 9999; set nver 0"
button $wr.end.help -text "Help" -width 12 -command "load_html [file join $html_dir S613.htm ] "
button $wr.end.save  -bg green -text Save -width 12 -command "destroy $wr"

pack $wr.end.quit $wr.end.reset $wr.end.help $wr.end.save -side left  -padx 4
}



#-----------------------------------------------------------------------------
proc restart {} {
global nstr mhrs nver html_dir 

if [winfo exists .mult_win] {destroy .mult_win}
set wr .mult_win
toplevel $wr
wm title $wr "(3) Multiple Trajectories in Time"
wm  geometry $wr +100+100

set ns0 $nstr
set mh0 $mhrs
set nv0 $nver

frame $wr.title
frame $wr.val1
frame $wr.val2
frame $wr.val3
frame $wr.end

pack  $wr.title -pady 10
pack  $wr.val1 $wr.val2 $wr.val3
pack  $wr.end -pady 20

label $wr.title.lab -fg blue -relief raised -justify left -wraplength 5i \
-text "New trajectories can be started at the specified time interval\
with a duration less than the run duration. Setting multiple levels\
greater than zero causes new trajectories to start at the downwind\
location rather than at the initial starting point."
pack  $wr.title.lab

label $wr.val1.lab -text "   Restart interval (hrs): "
entry $wr.val1.ent -textvariable nstr -relief sunken -width 4
pack  $wr.val1.lab $wr.val1.ent -side left

label $wr.val2.lab -text "   Restart duration (hrs): "
entry $wr.val2.ent -textvariable mhrs -relief sunken -width 4
pack  $wr.val2.lab $wr.val2.ent -side left

label $wr.val3.lab -text "Number of multiple levels: "
entry $wr.val3.ent -textvariable nver -relief sunken -width 4
pack  $wr.val3.lab $wr.val3.ent -side left

button $wr.end.quit  -bg red -text Quit -width 12 -command "set nstr $ns0; set mhrs $mh0; set nver $nv0; destroy $wr"
button $wr.end.reset  -text Reset -width 12 -command "set nstr 0; set mhrs 9999; set nver 0"
button $wr.end.help -text "Help" -width 12 -command "load_html [file join $html_dir S612.htm ] "
button $wr.end.save  -bg green -text Save -width 12 -command "destroy $wr"

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
grid forces the model to load the entire data grid.  Source heights and
concentration grid heights can be defined as either AGL (default) or MSL."
pack  $wr.title.lab

label $wr.met1.lab -text "Minimum size of the meteo subgrid: "
entry $wr.met1.ent -textvariable mgmin -relief sunken -width 4
pack $wr.met1.lab $wr.met1.ent -side left

label $wr.met2.div -text "____________________________________"
label $wr.met2.txt -fg blue -text "Height unit for input and output"
pack  $wr.met2.div $wr.met2.txt -side top

radiobutton $wr.met2.d0 -text "Heights above ground level " -variable kmsl -value "0" 
radiobutton $wr.met2.d1 -text "Relative to mean-sea-level " -variable kmsl -value "1" 
radiobutton $wr.met2.d2 -text "Fraction of the mixed layer" -variable kmsl -value "2"
pack $wr.met2.d0 $wr.met2.d1 $wr.met2.d2

button $wr.end.quit  -bg red -text Quit -width 12 -command "set mgmin $mg0; set kmsl $km0; destroy $wr"
button $wr.end.reset  -text Reset -width 12 -command "set mgmin 10; set kmsl 0"
button $wr.end.help -text "Help" -width 12 -command "load_html [file join $html_dir S611.htm ] "
button $wr.end.save  -bg green -text Save -width 12 -command "destroy $wr"

pack $wr.end.quit $wr.end.reset $wr.end.help $wr.end.save -side left  -padx 4
}


#-----------------------------------------------------------------------------
proc tsteps {} {
global tset delt tratio html_dir 

if [winfo exists .step_win] {destroy .step_win}
set wr .step_win
toplevel $wr
wm title $wr "(1) Integration Time Steps"
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
button $wr.end.save  -bg green -text Save -width 12 -command "destroy $wr"

pack $wr.end.quit $wr.end.reset $wr.end.help $wr.end.save -side left  -padx 4
}


#-----------------------------------------------------------------------------
proc mixdepth {} {
global kmixd html_dir 

if [winfo exists .mixd_win] {destroy .mixd_win}
set wr .mixd_win
toplevel $wr
wm title $wr "(5) Mixing Depth Computation Method"
wm  geometry $wr +100+100

set kmix0 $kmixd

frame $wr.title
frame $wr.mix2
frame $wr.end

pack  $wr.title -pady 10
pack  $wr.mix2
pack  $wr.end -pady 20

label $wr.title.lab -fg blue -relief raised -justify left -wraplength 6i \
-text "The default mixing depth value is provided as an input field by some\
meteorological models. If not available, it can be computed or set."
pack  $wr.title.lab

radiobutton $wr.mix2.d0 -text "Use meteorological model      " -variable kmixd -value "0" 
radiobutton $wr.mix2.d1 -text "From temperature profile      " -variable kmixd -value "1" 
radiobutton $wr.mix2.d2 -text "Compute from TKE profile      " -variable kmixd -value "2"
radiobutton $wr.mix2.d3 -text "modified Richardson number    " -variable kmixd -value "3"
pack $wr.mix2.d0 $wr.mix2.d1 $wr.mix2.d2 $wr.mix2.d3

radiobutton $wr.mix2.d4 -text "Set a constant value (m)" -variable kmixd -value "1500"
entry $wr.mix2.ent -textvariable kmixd -relief sunken -width 5
pack  $wr.mix2.d4 $wr.mix2.ent -side left

button $wr.end.quit  -bg red -text Quit  -width 12 -command "set kmixd $kmix0; destroy $wr"
button $wr.end.reset  -text Reset -width 12 -command "set kmixd 0"
button $wr.end.help  -text Help  -width 12 -command "load_html [file join $html_dir S616.htm ] "
button $wr.end.save  -bg green -text Save  -width 12 -command "destroy $wr"

pack $wr.end.quit $wr.end.reset $wr.end.help $wr.end.save -side left  -padx 4
}


#-----------------------------------------------------------------------------
proc trajmet {} {
global tm_tpot tm_tamb tm_rain tm_mixd tm_relh tm_dswf tm_terr tm_sphu tm_mixr html_dir 

if [winfo exists .tmet_win] {destroy .tmet_win}
set wr .tmet_win
toplevel $wr
wm title $wr "(6) Meteorological Variables Along the Trajectory"
wm  geometry $wr +100+100

set tp0 $tm_tpot 
set ta0 $tm_tamb 
set ra0 $tm_rain 
set md0 $tm_mixd 
set rh0 $tm_relh
set qh0 $tm_sphu
set wh0 $tm_mixr 
set sw0 $tm_dswf 
set tr0 $tm_terr

frame $wr.title
frame $wr.tvar
frame $wr.end

pack  $wr.title -pady 8
pack  $wr.tvar -pady 8
pack  $wr.end -pady 8

label $wr.title.lab -fg blue -relief raised -justify left -wraplength 5i \
-text "One or more variables may be selected for output in the endpoints\
file, however only the last variable will be displayed along the trajectory."
pack  $wr.title.lab

label $wr.tvar.lab -text "Meteorology Output Along the Trajectory"
checkbutton $wr.tvar.d0 -text " Potential Temperature " -variable tm_tpot 
checkbutton $wr.tvar.d1 -text " Ambient Temperature   " -variable tm_tamb 
checkbutton $wr.tvar.d2 -text " Precipitatiion        " -variable tm_rain 
checkbutton $wr.tvar.d3 -text " Mixing Depth          " -variable tm_mixd 
checkbutton $wr.tvar.d4 -text " Relative Humidity     " -variable tm_relh
checkbutton $wr.tvar.d5 -text " Specific Humidity     " -variable tm_sphu
checkbutton $wr.tvar.d6 -text " Water Vapor Mix Ratio " -variable tm_mixr
checkbutton $wr.tvar.d7 -text " Solar Radiation       " -variable tm_dswf 
checkbutton $wr.tvar.d8 -text " Terrain Height        " -variable tm_terr 
pack $wr.tvar.lab 
pack $wr.tvar.d0 $wr.tvar.d1 $wr.tvar.d2 $wr.tvar.d3 $wr.tvar.d4 \
     $wr.tvar.d5 $wr.tvar.d6 $wr.tvar.d7 $wr.tvar.d8 -pady 2

button $wr.end.quit  -bg red -text Quit -width 12 -command  \
  "set tm_tpot $tp0; set tm_tamb $ta0; set tm_rain $ra0; set tm_mixd $md0; set tm_relh $rh0; \
   set tm_sphu $qh0; set tm_mixr $wh0; set tm_dswf $sw0; set tm_terr $tr0; destroy $wr"
button $wr.end.reset  -text Reset -width 12 -command \
  "set tm_tpot 0; set tm_tamb 0; set tm_rain 0; set tm_mixd 0; set tm_relh 0; \
   set tm_sphu 0; set tm_mixr 0; set tm_dswf 0; set tm_terr 0"
button $wr.end.help -text "Help" -width 12 -command "load_html [file join $html_dir S614.htm ] "
button $wr.end.save  -bg green -text Save -width 12 -command "destroy $wr"

pack $wr.end.quit  $wr.end.reset $wr.end.help $wr.end.save -side left  -padx 4
}


#-----------------------------------------------------------------------------
proc ensemble {} {
global member1 member2 dxf dyf dzf html_dir 
if [winfo exists .ens_win] {destroy .ens_win}
set wr .ens_win
toplevel $wr
wm title $wr "(7) Meteorogical Ensemble Configuration"
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

button $wr.end.quit  -bg red -text Quit -width 12 -command \
       "set member1 $mb1; set member2 $mb2; set dxf $dx0; set dyf $dy0; set dzf $dz0; destroy $wr"
button $wr.end.reset  -text Reset -width 12 -command \
       "set member1 1; set member2 27; set dxf 1.0; set dyf 1.0; set dzf 0.01"
button $wr.end.help -text "Help" -width 12 -command "load_html [file join $html_dir S615.htm ] "
button $wr.end.save  -bg green -text Save -width 12 -command "destroy $wr"

pack $wr.end.quit $wr.end.reset $wr.end.help $wr.end.save -side left -padx 4
}

#--------------------------------------------------------------------------

proc wrfvert {} {
global wvert html_dir
if [winfo exists .wrfvert_win] {destroy .wrfvert_win}
set wr .wrfvert_win
toplevel $wr
wm title $wr "(8) WRF Vertical Interpolation"
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
"set wvert { }"
button $wr.end.help -text "Help" -width 13 -command "load_html [file join $html_dir S633.htm ] "
button $wr.end.save  -bg green1 -text Save -width 13 -command "destroy $wr"

pack $wr.end.quit $wr.end.reset $wr.end.help $wr.end.save -side left  -padx 15
}


#--------------------------------------------------------------------------
proc clear_config {} {
if [file exists SETUP.CFG] {file delete SETUP.CFG}
reset_config
}

#--------------------------------------------------------------------------
proc reset_config {} {
global tset delt tratio mgmin kmixd kmsl mhrs nstr nver tout khmax wvert                
global tm_tpot tm_tamb tm_rain tm_mixd tm_relh tm_sphu tm_mixr tm_dswf tm_terr 
global dxf dyf dzf

set tset 2
set delt 0
set tratio 0.75
set kmixd 0
set mgmin 10
set khmax 9999
set kmsl 0
set nstr 0
set mhrs 9999
set nver 0
set tout 60
set tm_tpot 0
set tm_tamb 0
set tm_rain 0
set tm_mixd 0
set tm_relh 0
set tm_sphu 0
set tm_mixr 0
set tm_dswf 0
set tm_terr 0
set dxf 1.0
set dyf 1.0
set dzf 0.01
set wvert .TRUE.

if [file exists TRAJ.CFG] {file delete TRAJ.CFG}
}


#--------------------------------------------------------------------------
proc load_config {Fname Wname} {
global tset delt tratio mgmin kmsl nstr mhrs nver tout khmax kmixd wvert              
global tm_tpot tm_tamb tm_rain tm_mixd tm_relh tm_sphu tm_mixr tm_dswf tm_terr 
global dxf dyf dzf

set delt 0

if [file exists $Fname] {
   set f [open "$Fname" r]
   gets $f cline
   while {[gets $f cline] >= 0} {
      set pline [string trim $cline]
      set cline [split $pline]
      if {[llength $cline] == 3} {
         set local [lindex $cline 0]
         regsub -all , [lindex $cline 2] { } value

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
   if [info exists wvert]  { } else {set wvert .TRUE.}
}
if [winfo exists $Wname] {destroy $Wname}
}


#------------------------------------------------------------------------------
proc save_config {Fname Wname} {
global tset delt tratio mgmin kmixd kmsl nstr mhrs nver tout khmax wvert              
global tm_tpot tm_tamb tm_rain tm_mixd tm_relh tm_sphu tm_mixr tm_dswf tm_terr
global dxf dyf dzf

set f [open "$Fname" w]
puts $f " &SETUP"
if { $tset == 1} {puts $f " delt = $delt,"}
if { $tset == 2} {puts $f " tratio = $tratio,"}
puts $f " mgmin = $mgmin,"
puts $f " khmax = $khmax,"
puts $f " kmixd = $kmixd,"
puts $f " kmsl = $kmsl,"
puts $f " nstr = $nstr,"
puts $f " mhrs = $mhrs,"
puts $f " nver = $nver,"
puts $f " tout = $tout,"
puts $f " tm_tpot = $tm_tpot,"
puts $f " tm_tamb = $tm_tamb,"
puts $f " tm_rain = $tm_rain,"
puts $f " tm_mixd = $tm_mixd,"
puts $f " tm_relh = $tm_relh,"
puts $f " tm_sphu = $tm_sphu,"
puts $f " tm_mixr = $tm_mixr,"
puts $f " tm_dswf = $tm_dswf,"
puts $f " tm_terr = $tm_terr,"
puts $f " dxf = $dxf,"
puts $f " dyf = $dyf,"
puts $f " dzf = $dzf,"
puts $f " wvert = $wvert,"
puts $f " /"
close $f
file copy -force $Fname TRAJ.CFG
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
button $wr.win.rgt  -text Browse -width 8 -command {
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
button $wr.bot.save  -bg green -text "Save" -width 8 -command {save_config $Name_save .savedir}
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
button $wr.win.rgt  -text Browse -width 8 -command {
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
button $wr.bot.save  -bg green -text "OK " -width 8 -command {load_config $Name_load .loaddir}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 5 
bind $wr.txt.ent <Return> {load_config $Name_load .loaddir}
}
