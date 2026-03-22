proc conc_file {} {
#-----------------------------------------------------------------------------
# CONC_FILE.TCL: concentration file display
# Last Revised: 01 December 2008
#               16 March 2012
#-----------------------------------------------------------------------------
if [winfo exists .confile] {destroy .confile}
global html_dir

set wr .confile
toplevel $wr
wm title $wr " Simple Concentration File Display "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.data
frame $wr.bot
pack $wr.top $wr.data $wr.bot \
     -side top -pady 5 -padx 10

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
-text "Program to summarize the contents of a HYSPLIT concentration\
data file. The summary output is written to the text file: conread.txt."
pack $wr.top.lab

#-->input file
frame $wr.data.lab
frame $wr.data.pick
pack $wr.data.lab $wr.data.pick -pady 2 -side top
button $wr.data.lab.set  -text "Set File Name of Concentration Output" -width 55 -command {
   .confile.data.pick.src.list delete 0 end
   .confile.data.pick.dir.list delete 0 end
   set Grid_number 0
   do_file_list .confile.data.pick.src.list .confile.data.pick.dir.list 
   }
pack  $wr.data.lab.set -side left -padx 15
frame $wr.data.pick.dir
frame $wr.data.pick.src
pack $wr.data.pick.dir $wr.data.pick.src -side left -padx 5
listbox $wr.data.pick.dir.list -relief sunken -width 35 -height 1 -setgrid yes -bg LightCyan3
pack $wr.data.pick.dir.list -fill both -expand yes 
listbox $wr.data.pick.src.list -relief sunken -width 20 -height 1 -setgrid yes -bg LightCyan3
pack $wr.data.pick.src.list -fill both -expand yes

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help  -width 8 \
       -command "load_html [file join $html_dir S363.htm ] "
button $wr.bot.save  -bg green -text "Show File Contents" -width 20 -command {
       set gdir  [.confile.data.pick.dir.list get 0 0]
       set gbase [.confile.data.pick.src.list get 0 0]
       run_conread $gdir $gbase}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 
}

##################################################
#-->run conread program

proc run_conread {gdir gbase} {
global exec_dir

if {[file size $gdir/$gbase] == 0} {
   msg_box "File not found: $gdir/$gbase\n"
   destroy .confile
   return
   }

if [file exists conread.txt] {file delete conread.txt}

#-->decoder command line arguments
  set input [file join $gdir $gbase]
  append input "\n"
  append input "0 \n"
  append input "\n"
  exec $exec_dir/conread <<$input >conread.txt

if [file exists conread.txt] {
   set log [ScrollText .f]
   $log insert end "CONCENTRATION DATA FILE LISTING ...\n"
   set fileid [open "conread.txt" r]
   while {[eof $fileid] != 1} {
      gets $fileid cline
      $log insert end $cline\n
   }
   close $fileid
} else {
  msg_box "File not found: conread.txt"
}
}
