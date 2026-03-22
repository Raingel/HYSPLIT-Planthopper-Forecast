proc conc_phys { } {

#-----------------------------------------------------------------------------
# CONC_PHYS.TCL: Create a physics ensemble by running multiple dispersion
# calculations, varying critical namelist parameters with each simulation 
# Last Revised: 12 Nov 2008 - initial version
#               12 Jan 2011 - options update
#               18 Apr 2013 - options update (vscale{u|s}
#               12 Sep 2018 - revised defaults for kmix0 and vscales
#               07 Mar 2020 - new ensemble members with STILT upgrade
#-----------------------------------------------------------------------------

global html_dir Fnum Dvar
if [winfo exists .concphys] {destroy .concphys}
set wr .concphys
toplevel $wr
wm title $wr " Create Physics Ensemble using Multiple Simulations "
wm  geometry $wr +200+300

frame $wr.top
frame $wr.mid1
frame $wr.mid2
frame $wr.bot
pack $wr.top $wr.mid1 $wr.mid2 $wr.bot -side top -pady 5 -padx 5

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "Execute a script to run multiple iterations of the concentration\
calculation using different namelist parameter values for each simulation.\
Variations are written to the file ensemble.txt."
pack $wr.top.lab

#-->simultion
label $wr.mid1.lab -text "Completed Simulation Number:"
entry $wr.mid1.ent -textvariable Fnum -width 4
pack $wr.mid1.lab $wr.mid1.ent -side left -padx 5

label $wr.mid2.lab -text "Namelist Variable Variation:"
entry $wr.mid2.ent -textvariable Dvar -width 15
pack $wr.mid2.lab $wr.mid2.ent -side left -padx 5
 
#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help  -width 8 \
       -command "load_html [file join $html_dir S356.htm ] "
button $wr.bot.save  -bg green1 -text "Execute Script" -width 20 -command {make_phys}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 

set Fnum 0
set Dvar " "
}

#======================================================
proc make_phys {} {

global result X_dir tcl_dir exec_dir tcl_platform Dvar Fnum Grid_name
global initd khmax numpar qcycle delt tratio ndump ncycl \
       mgmin kmsl tset maxpar cpack dxf dyf dzf ichem maxdim  \
       cmass ninit pinpf poutf kpuff efile p10f tkerd tkern \
       kspl krnd frhs frvs frts frhmax splitf kmixd kmix0 \
       kzmix kdef kbls kblt idsp conage hscale vscales vscaleu

if [info exists khmax] { } else {
   source $tcl_dir/conc_cfg.tcl
   reset_config
}

set g [open ensemble.txt w]

set Fnum 0
while {$Fnum < 15} {
   incr Fnum       
  
   set f [open SETUP.CFG w]
   puts $f " &SETUP"

#  variables below are set in the ensemble
#  when not set, then they assume their default values regardless of the GUI settings

   if {$Fnum == 1}  {set Dvar "idsp=2"; puts $f " $Dvar,"}
   if {$Fnum == 2}  {set Dvar "kmixd=1"; puts $f " $Dvar,"}
   if {$Fnum == 3}  {set Dvar "kmixd=3"; puts $f " $Dvar,"}
   if {$Fnum == 4}  {set Dvar "kmix0=50"; puts $f " $Dvar,"}
   if {$Fnum == 5}  {set Dvar "kzmix=1"; puts $f " $Dvar,"}
   if {$Fnum == 6}  {set Dvar "kdef=1"; puts $f " $Dvar,"}
   if {$Fnum == 7}  {set Dvar "kbls=2"; puts $f " $Dvar,"}
   if {$Fnum == 8}  {set Dvar "kblt=1"; puts $f " $Dvar,"}
   if {$Fnum == 9}  {set Dvar "kblt=3"; puts $f " $Dvar,"}
   if {$Fnum == 10} {set Dvar "kblt=5"; puts $f " $Dvar,"}
   if {$Fnum == 11} {set Dvar "vscales=200.0"; puts $f " $Dvar,"}
   if {$Fnum == 12} {set Dvar "vscales=-1.0"; puts $f " $Dvar,"}
   if {$Fnum == 13} {set Dvar "kblt=1"; puts $f " $Dvar,"; puts $f " vscales=-1.0,"; set Dvar "Run  8+12"}
   if {$Fnum == 14} {set Dvar "kblt=3"; puts $f " $Dvar,"; puts $f " vscales=-1.0,"; set Dvar "Run  9+12"}
   if {$Fnum == 15} {set Dvar "kblt=5"; puts $f " $Dvar,"; puts $f " vscales=-1.0,"; set Dvar "Run 10+12"}

#  the remaining namelist values are set according to their GUI values 
   puts $f " initd = $initd," 
   puts $f " khmax = $khmax,"
   puts $f " kpuff = $kpuff,"
   puts $f " conage = $conage,"
   puts $f " numpar = $numpar,"
   puts $f " qcycle = $qcycle,"
   puts $f " efile = '$efile',"
   puts $f " ninit = $ninit,"
   puts $f " ndump = $ndump,"
   puts $f " ncycl = $ncycl,"
   puts $f " pinpf = '$pinpf',"
   puts $f " poutf = '$poutf',"
   puts $f " mgmin = $mgmin,"
   puts $f " kmsl = $kmsl,"
   puts $f " maxpar = $maxpar,"
   puts $f " cpack = $cpack,"
   puts $f " cmass = $cmass,"
   puts $f " dxf = $dxf,"
   puts $f " dyf = $dyf,"
   puts $f " dzf = $dzf,"
   puts $f " ichem = $ichem,"
   if { $ichem == 2 } {puts $f " maxdim = $Num_pollut,"}
   if { $ichem == 3 } {puts $f " p10f = $p10f,"}
   if {$tset == 1} {puts $f " delt = $delt,"}
   if {$tset == 2} {puts $f " tratio = $tratio,"}
   puts $f " kspl = $kspl,"
   puts $f " krnd = $krnd,"
   puts $f " frhs = $frhs,"
   puts $f " frvs = $frvs,"
   puts $f " frts = $frts,"
   puts $f " frhmax = $frhmax,"
   puts $f " splitf = $splitf,"
   puts $f " hscale = $hscale,"
   puts $f " tkerd = $tkerd,"
   puts $f " tkern = $tkern,"
   puts $f " vscaleu = $vscaleu,"
   puts $f " /"
   close $f

   if { "$tcl_platform(platform)" == "unix" } {
      if { "$X_dir" == "" } {
         exec $exec_dir/hycs_std
      } else {
         exec $X_dir/xterm -fn fixed -e $exec_dir/hycs_std
      }
   } else {
     exec $exec_dir/hycs_std.exe
     update
     msg_box " Calculations completed: $Fnum "
   }

#  set myfile MESSAGE
#  append myfile .[format "%3.3u" $Fnum]
#  file rename MESSAGE $myfile

#  set myfile SETUP
#  append myfile .[format "%3.3u" $Fnum]
#  file rename SETUP.CFG $myfile

   set myfile $Grid_name
   append myfile .[format "%3.3u" $Fnum]
   file rename -force $Grid_name $myfile
   puts $g "$myfile : $Dvar"
}
close $g

msg_box " Calculations completed! "
tkwait window .msg_win
if {$result == 0} {destroy .concphys}
}
