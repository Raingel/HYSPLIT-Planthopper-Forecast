proc run_cfg {} {

#------------------------------------------------------------------------------
# RUN_CFG.TCL:  checks for SETUP.CFG prior to running model
# Last Revised: 21 Sep 2004 - initial version
#------------------------------------------------------------------------------

global holdopen

if [winfo exists .runcfg] {destroy .runcfg}
set wr .runcfg
toplevel $wr
wm title $wr " Advanced Configuration Namelist File Found! "
wm  geometry $wr +100+100

frame $wr.top
frame $wr.bot
pack $wr.top $wr.bot -side top -pady 6

#-->description
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 7i -text \
"SETUP.CFG namelist file found! Created from the Advanced-Configuration Menu."
pack $wr.top.lab

#-->bottom action buttons
button $wr.bot.del -bg yellow -text "Delete file then Run" -width 25 -command {
  file delete SETUP.CFG
  set holdopen 1}
button $wr.bot.end  -bg red -text "Cancel Run" -width 20 -command {
  file delete CONTROL
  set holdopen 1}
button $wr.bot.run  -bg green -text "Run using SETUP file" -width 25 -command {
  set holdopen 1}
pack $wr.bot.end $wr.bot.del $wr.bot.run -side left -padx 10
tkwait variable holdopen
unset holdopen
destroy $wr
}
