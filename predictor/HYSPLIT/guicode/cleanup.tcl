proc cleanup { } {

#-----------------------------------------------------------------------------
# CLEANUP.TCL: Deletes ensemble files in the working directory
# Last Revised: 14 Jan 2011 - initial version
#               25 Jul 2013 - added files
#               08 Nov 2013 - remove PARDUMP
#               02 Jul 2014 - added WARNING
#-----------------------------------------------------------------------------

global html_dir
if [winfo exists .clean] {destroy .clean}
set wr .clean
toplevel $wr
wm title $wr " Cleanup Ensemble Files from Working Directory "
wm  geometry $wr +200+300

frame $wr.top
frame $wr.bot
pack $wr.top $wr.bot -side top -pady 5 -padx 5

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text " Cleanup Ensemble Files from Working Directory "
pack $wr.top.lab

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.save  -bg green -text "Execute Script" -width 20 -command {clean_work}
pack  $wr.bot.dismiss $wr.bot.save -side left -padx 10 
}

#======================================================
proc clean_work {} {

global Grid_name

set Fnum 0
while {$Fnum < 999} {
   incr Fnum       

   set file1 [lindex $Grid_name 0]
   append file1 .[format "%3.3u" $Fnum]
   if [ file exists $file1] {file delete $file1}

   set file2  MESSAGE
   append file2 .[format "%3.3u" $Fnum]
   if [ file exists $file2] {file delete $file2}

   set file3  CONC
   append file3 .[format "%3.3u" $Fnum]
   if [ file exists $file3] {file delete $file3}

   set file4  VMSDIST
   append file4 .[format "%3.3u" $Fnum]
   if [ file exists $file4] {file delete $file4}

   set file5  PARDUMP
   append file5 .[format "%3.3u" $Fnum]
   if [ file exists $file5] {file delete $file5}

   set file6  WARNING
   append file6 .[format "%3.3u" $Fnum]
   if [ file exists $file6] {file delete $file6}
}

if [ file exists prob05] {file delete prob05} 
if [ file exists prob10] {file delete prob10}
if [ file exists prob25] {file delete prob25}
if [ file exists prob50] {file delete prob50}
if [ file exists prob75] {file delete prob75}
if [ file exists prob90] {file delete prob90}
if [ file exists prob95] {file delete prob95}
if [ file exists cmax00] {file delete cmax00}
if [ file exists cmax01] {file delete cmax01}
if [ file exists cmax10] {file delete cmax10}
if [ file exists cmean]  {file delete cmean}
if [ file exists cnumb]  {file delete cnumb}
if [ file exists cvarn]  {file delete cvarn}

destroy .clean
}
