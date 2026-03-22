proc conc_daug {} {
#-----------------------------------------------------------------------------
# CONC_DAUG.TCL: concentration nuclide daughter product preprocessor
# Last Revised: 11 January 2016
#               25 January 2016 - added parent radionuclide selection list
#-----------------------------------------------------------------------------
global html_dir parent Grid_name

if [winfo exists .confile] {destroy .confile}
set wr .confile
toplevel $wr
wm title $wr " Nuclide Daughter product "
wm  geometry $wr +50+15

if {$Grid_name == ""} {
   msg_box "Simulation parameters undefined -- execute setup first!"
   return
}

file copy -force default_conc CONTROL
if [file exists MESSAGE] {file delete MESSAGE}
if [file exists CONTROL] { } else { return }

frame $wr.top
frame $wr.lab1
frame $wr.laba
frame $wr.lab2
frame $wr.lab3
frame $wr.exit
pack $wr.top $wr.lab1 $wr.laba $wr.lab2 $wr.lab3 $wr.exit \
     -side top -pady 5 -padx 10

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 5i \
-text "Program to determine the daughter nuclides produced by a parent nuclide \
along with the half-life and branching fractions.\
The summary output is written to the text file: DAUGHTER.TXT.\
In addition, the daughter product information is added to the CONTROL file"
pack $wr.top.lab

#################################################
label  $wr.lab1.lab -width 55 -bg Gray65 -anchor w \
       -text "Step 1: Define the nuclide parent name (e.g. Cs-137)"
button $wr.lab1.but  -text "Apply" -width 10 -command daughter
pack   $wr.lab1.lab $wr.lab1.but -side left -padx 5 

entry  $wr.laba.ent -textvariable parent -relief sunken -width 10 
button $wr.laba.but -bg yellow -text "Select from List" -width 20 -command set_parent
pack   $wr.laba.ent  $wr.laba.but -side left -padx 5 

# - setup

label  $wr.lab2.lab -width 55 -bg Gray65 -anchor w \
       -justify left -wraplength 6i -text "Step 2: Configure setup (set Maxdim=#daughters) "
button $wr.lab2.but  -text "Setup Run" -width 10 -command Config_conc 
pack   $wr.lab2.lab $wr.lab2.but -side left -padx 5

# - run

label $wr.lab3.lab -width 55 -bg Gray65 -anchor w \
      -text "Step 3: Run HYSPLIT with daughter products "
button $wr.lab3.but  -bg green -text "Run" -width 10 -command run_hys 
pack  $wr.lab3.lab $wr.lab3.but -side left -padx 5

#entry $wr.labb.ent -textvariable parent -relief sunken -width 60
#pack  $wr.labb.ent

# -termination

button $wr.exit.dismiss  -bg red -text Quit -width 32 -command "destroy $wr"
button $wr.exit.help -text "Help" -width 32 \
       -command "load_html [file join $html_dir S365.htm ] "
pack  $wr.exit.dismiss $wr.exit.help -side left -padx 8 
}

#--------------------------------------------------------------------------

proc daughter {} {
global parent
global X_dir tcl_platform exec_dir

    set arg1 -n ; append arg1 $parent

if [ file exists DAUGHTER.TXT ] {file delete -force DAUGHTER.TXT}
if [ file exists CONTROL.DAUGHTER ] {file delete -force CONTROL.DAUGHTER}

    if { "$tcl_platform(platform)" == "unix" } {
       if { "$X_dir" == "" } {
       exec $exec_dir/nuctree $arg1 
       } else {
       exec $X_dir/xterm -fn fixed -e $exec_dir/nuctree $arg1 
       }
    } else {
       exec $exec_dir/nuctree $arg1 
    }

   if [file exists CONTROL.DAUGHTER] {
       
   if [file exists DAUGHTER.TXT] {
      set log [ScrollText .f]
      set fid [open "DAUGHTER.TXT" r]
      while {[eof $fid] != 1} {
      gets $fid cline
      $log insert end $cline\n
   }
      close $fid
      file delete -force DAUGHTER.TXTT
   }

} else {
msg_box " Error: incorrect nuclide parent name. \
Nuclide names can be found in ../auxiliary/ICRP-07.NDX  "
tkwait window .msg_win
}
}

#----------------------------------------------------------------

proc run_hys {} {
global X_dir exec_dir exec_dir result log tcl_platform tcl_dir

if [file exists SETUP.CFG] {runcfg}
if [ file exists CONTROL.DAUGHTER ] {file copy -force CONTROL.DAUGHTER CONTROL}
file copy -force CONTROL default_conc

if { "$tcl_platform(platform)" == "unix" } {
   set code "${exec_dir}/hycs_std"
   set xops "|$code |& cat"
   } else {
   set code ${exec_dir}/hycs_std.exe
   set xops "|$code"
   }

if [file exists $code] {
   set log [ScrollText .f]
   $log configure -cursor watch
   $log insert end "Model started ...\n"
   update

   if [catch {open $xops} result] {
      $log insert end  $result
      } else {
      fileevent $result readable Log
      }

   } else {
   msg_box "Concentration executable for hycs_std  not available!"
   }
}

#------------------------------

proc Config_conc {} {
global tcl_dir
source $tcl_dir/conc_cfg.tcl
cfgconc_init
}

#---------------------------------------------------------------------
proc runcfg {} {
global tcl_dir
source $tcl_dir/run_cfg.tcl
run_cfg
}

#---------------------------------------------------------------------
proc set_parent {} {

if [winfo exists .setparent] {destroy .setparent}
set wx .setparent
toplevel $wx
wm title $wx " Select Parent Radionuclide "
wm  geometry $wx +50+15

frame $wx.top -width 20 
frame $wx.buttons
scrollbar $wx.scroll -command "$wx.list yview"
listbox $wx.list -yscroll "$wx.scroll set" -relief sunken -width 20

button $wx.cancel -bg red -text "Cancel" -width 10 -command {destroy .setparent}
button $wx.ok -bg green -text "Select" -width 10 -command radout

pack $wx.top -side top -fill x
pack $wx.buttons -side top -fill x
pack $wx.scroll -in $wx.top -side right -fill y
pack $wx.list -in $wx.top -fill x
pack $wx.cancel -in $wx.buttons -side left -fill x
pack $wx.ok -in $wx.buttons -side right

if [file exists ../auxiliary/ICRP-07.NDX] {

   set f [open ../auxiliary/ICRP-07.NDX]
   gets $f line
   while {[gets $f line] >=0} {
      set value [lindex [split $line] 0]
      $wx.list insert end $value
   }
   close $f

} else { 
   $wx.list insert end "ERROR: Parent file (ICRP-07.NDX) not found!"
   $wx.list insert end "Requires the file in auxiliary directory." 
}

$wx.list selection set 0
bind $wx.list <Double-Button-1> "radout"

#---------------------------------------------------------------------
proc radout {} {
global parent
set parent [selection get]
destroy .setparent
}

}
