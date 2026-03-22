proc file_disp {} {
#-----------------------------------------------------------------------------
# DISP_FILE.TCL: meteorological data info display script
# Last Revised: 10 April 2006
#               16 March 2012
#-----------------------------------------------------------------------------
if [winfo exists .metfile] {destroy .metfile}
global html_dir
global Met_file

set wr .metfile
toplevel $wr
wm title $wr " Meteorological Data Information "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.data
frame $wr.bot
pack $wr.top $wr.data $wr.bot \
     -side top -pady 5 -padx 10

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
-text "Program to summarize the contents of a HYSPLIT compatible\
meteorological data file. The summary output is written to a text\
file: chkfile.txt. Displaying results for very large data sets may\
take a few minutes. All records are listed!"
pack $wr.top.lab

#-->meteorology input
frame $wr.data.lab
frame $wr.data.pick
pack $wr.data.lab $wr.data.pick -pady 2 -side top
button $wr.data.lab.set  -text "Set File Name of ARL format Data" -width 55 -command {
   .metfile.data.pick.src.list delete 0 end
   .metfile.data.pick.dir.list delete 0 end
   set Grid_number 0
   do_file_list .metfile.data.pick.src.list .metfile.data.pick.dir.list 
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
       -command "load_html [file join $html_dir S130.htm ] "
button $wr.bot.save  -bg green -text "Run File Program" -width 20 -command {
       set gdir  [.metfile.data.pick.dir.list get 0 0]
       set gbase [.metfile.data.pick.src.list get 0 0]
       run_chkfile $gdir $gbase}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 
}

##################################################
#-->run chk_file program
proc run_chkfile {gdir gbase} {
global exec_dir

if {[file size $gdir/$gbase] == 0} {
   msg_box "File not found: $gdir/$gbase\n"
   destroy .metfile
   return
   }

#-->decoder command line arguments
  append input "$gdir/ \n"
  append input "$gbase \n"
  exec $exec_dir/chk_file <<$input >chkfile.txt

if [file exists chkfile.txt] {
   set log [ScrollText .f]
   $log insert end "METEOROLOGICAL DATA LISTING ...\n"
   set fileid [open "chkfile.txt" r]
   while {[eof $fileid] != 1} {
      gets $fileid cline
      $log insert end $cline\n
   }
   close $fileid
} else {
  msg_box "File not found: chkfile.txt"
}
if [file exists temp.txt] {file delete temp.txt}
}
