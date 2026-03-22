proc gem_cfg { } {

#-----------------------------------------------------------------------------
# GEM_CFG.TCL: setup.cfg namelist file configuration script
# Last Revised: 13 Nov 2008 - initial version
#               17 Apr 2013 - revised vertical integration options
#-----------------------------------------------------------------------------
      
global html_dir 
global kinit ckon gemconc gemzint kzbot kztop kzavg wfreq ihour

if [winfo exists .gemconfig] {destroy .gemconfig}
set wr .gemconfig
toplevel $wr
wm title $wr "Create GEMPARM.CFG Namelist File"
wm  geometry $wr +50+50

frame $wr.top
frame $wr.ini
frame $wr.con
frame $wr.ver
pack $wr.top $wr.ini $wr.con $wr.ver -side top -pady 6 -padx 4
frame $wr.lev
frame $wr.int
pack $wr.lev $wr.int -side top -pady 0 -padx 4
frame $wr.exit
pack $wr.exit -side top -pady 6 -padx 4

label $wr.top.txt -fg blue -relief raised -justify left -wraplength 6i \
-text "Sets the parameter values in the GEMPARM.CFG namelist file required\
by the GEM subroutines to run the special global simulation. Standard\
namelist options in SETUP.CFG should specify Lagrangian to Eulerian\
transition time as well as a particle type greater than 100."
pack  $wr.top.txt -padx 4 -pady 4

#-->initialization

label $wr.ini.txt -fg blue -text "Concentration Initialization Method"
pack $wr.ini.txt -side top
radiobutton $wr.ini.d0 -text "All Zero  " -variable kinit -value "0" 
radiobutton $wr.ini.d3 -text "Last Run  " -variable kinit -value "3" 
radiobutton $wr.ini.d5 -text "Enter ->" -variable kinit -value "5" 
entry $wr.ini.ent -textvariable ckon -relief sunken -width 6
pack $wr.ini.d0 $wr.ini.d3 $wr.ini.d5 $wr.ini.ent -side left

#-->standard concentration output

label $wr.con.lab -fg blue -text "Concentration Output Time Intervals"
pack $wr.con.lab -side top
label $wr.con.txt1 -text "File name:"
entry $wr.con.ent1 -textvariable gemconc -relief sunken -width 12
label $wr.con.txt2 -text "   Freq(hrs):"
entry $wr.con.ent2 -textvariable wfreq -relief sunken -width 3
label $wr.con.txt3 -text "   Initial hr:"
entry $wr.con.ent3 -textvariable ihour -relief sunken -width 3
pack $wr.con.txt1 $wr.con.ent1 $wr.con.txt2 $wr.con.ent2 \
     $wr.con.txt3 $wr.con.ent3 -side left

#-->vertical averaging

label $wr.ver.txt -fg blue -text "Concentration Output Vertical Structure"
pack $wr.ver.txt -side top
radiobutton $wr.ver.d0 -text "Layers " -variable kzavg -value "0" 
radiobutton $wr.ver.d1 -text "Average " -variable kzavg -value "1" 
label $wr.ver.txt2 -text "   Bottom:"
entry $wr.ver.ent2 -textvariable kzbot -relief sunken -width 3
label $wr.ver.txt3 -text "   Top:"
entry $wr.ver.ent3 -textvariable kztop -relief sunken -width 3
pack $wr.ver.d0 $wr.ver.d1 $wr.ver.txt2 $wr.ver.ent2 $wr.ver.txt3 $wr.ver.ent3 -side left

#-->specify file and layers

label $wr.lev.txt -fg blue -text "Vertically Integrated Output Options"
pack $wr.lev.txt -side top
label $wr.lev.txt1 -text "File name:"
entry $wr.lev.ent1 -textvariable gemzint -relief sunken -width 12
label $wr.lev.txt2 -text "   "
radiobutton $wr.lev.d2 -text "Integral " -variable kzavg -value "2" 
radiobutton $wr.lev.d3 -text "Integral+" -variable kzavg -value "3" 
radiobutton $wr.lev.d4 -text "Integral+" -variable kzavg -value "4"
pack $wr.lev.txt1 $wr.lev.ent1 $wr.lev.txt2 $wr.lev.d2 $wr.lev.d3 $wr.lev.d4 -side left
label $wr.int.txt3 -text "                                       Layers       Average"
pack $wr.int.txt3

#-->termination

label  $wr.exit.top -text "_________________________________________________"
pack   $wr.exit.top -side top
button $wr.exit.dismiss  -bg red -text "Quit" -width 12 -command "destroy $wr"
button $wr.exit.reset  -text "Reset" -width 6 -command "clear_config"
button $wr.exit.help -text "Help" -width 6 \
       -command "load_html [file join $html_dir S359.htm ] "
button $wr.exit.cont  -bg green -text "Save" -width 16 -command "save_config GEMPARM.CFG $wr"
pack  $wr.exit.dismiss $wr.exit.reset $wr.exit.help $wr.exit.cont -side left -padx 4 -pady 6

if [file exists GEMPARM.CFG] {
   load_config GEMPARM.CFG .dummy
   } else {
   reset_config
   }
}


#--------------------------------------------------------------------------
proc clear_config {} {
if [file exists GEMPARM.CFG] {file delete GEMPARM.CFG}
reset_config
}

#--------------------------------------------------------------------------
proc reset_config {} {
global kinit ckon gemconc gemzint kzbot kztop kzavg wfreq ihour

set kinit 0
set ckon 0.0
set gemconc gemconc.bin
set gemzint gemzint.bin
set kzbot 1
set kztop 1
set kzavg 0
set wfreq 3
set ihour 0
}


#--------------------------------------------------------------------------
proc load_config {Fname Wname} {
global kinit ckon gemconc gemzint kzbot kztop kzavg wfreq ihour

if [file exists $Fname] {
   set f [open "$Fname" r]
   gets $f cline
   while {[gets $f cline] >= 0} {
      set pline [string trim $cline]
      set cline [split $pline]
      if {[llength $cline] == 3} {
         set local [lindex $cline 0]
         regsub -all , [lindex $cline 2] {} value
         if [catch {expr $value}] {
            regsub -all ' $value {} value2
            set $local $value2
         } else {
            set $local [expr $value]
         }
      }
   }
   close $f
}
if [winfo exists $Wname] {destroy $Wname}
}


#------------------------------------------------------------------------------
proc save_config {Fname Wname} {
global kinit ckon gemconc gemzint kzbot kztop kzavg wfreq ihour
set f [open "$Fname" w]
puts $f " &GEMPARM"
puts $f " kinit = $kinit,"
puts $f " ckon = $ckon,"
puts $f " gemconc = '$gemconc',"
puts $f " gemzint = '$gemzint',"
puts $f " kzbot = $kzbot,"
puts $f " kztop = $kztop,"
puts $f " kzavg = $kzavg,"
puts $f " wfreq = $wfreq,"
puts $f " ihour = $ihour,"
puts $f " /"
close $f
if [winfo exists $Wname] {destroy $Wname}
}
