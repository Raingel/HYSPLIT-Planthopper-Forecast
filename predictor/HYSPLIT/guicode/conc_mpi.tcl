proc conc_mpi { } {

#-----------------------------------------------------------------------------
# CONC_MPI.TCL: Run multiple dispersion through MPI                
# Last Revised: 15 jan 2009 - initial version
#               07 Sep 2020 - corrected variable arglist to Arglist
#-----------------------------------------------------------------------------

global html_dir 
global Nproc Model Start
global Temp_path Work_path

if [winfo exists .mpiconc] {destroy .mpiconc}
set wr .mpiconc
toplevel $wr
wm title $wr " Submit one or more MPI Simulations "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.mid
frame $wr.low
frame $wr.bot
pack $wr.top $wr.mid  $wr.low $wr.bot -side top -pady 5 -padx 5

#-->information
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 7i \
 -text "Execute a script to submit one or more MPI simulations, where\
the number of processors, model executable, prep-code flag, and\
working directory can be changed with each submission. The prep-code\
sets pre-defined pre-processor options set by the calling menu."
pack $wr.top.lab

#-->starting date
label $wr.mid.lab1 -text "Processors:" 
entry $wr.mid.ent1 -textvariable Nproc -width 2
pack  $wr.mid.lab1 $wr.mid.ent1 -side left -padx 2
label $wr.mid.lab2 -text "   Executable:" 
entry $wr.mid.ent2 -textvariable Model -width 10
pack $wr.mid.lab2 $wr.mid.ent2 -side left -padx 2
label $wr.mid.lab3 -text "   PrepCode:" 
entry $wr.mid.ent3 -textvariable Start -width 2
pack $wr.mid.lab3 $wr.mid.ent3 -side left -padx 2

#-->file name convention
label $wr.low.lab -text "Working Directory:"
entry $wr.low.ent -textvariable Temp_path -width 40
pack $wr.low.lab $wr.low.ent -side left -padx 5
 
#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help  -width 8 \
       -command "load_html [file join $html_dir S358.htm ] "
button $wr.bot.save  -bg green -text "Execute Script" -width 20 -command {Run_mpi}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 
set Temp_path $Work_path
}

#======================================================

proc Run_mpi { } {

global member1 exec_dir X_dir exec_dir 
global Nproc Model Start
global Temp_path Work_path

if { "$Work_path" != "$Temp_path" } {
if {! [file exists $Temp_path]} {
   exec mkdir $Temp_path
   if [file exists ASCDATA.CFG]  {file copy ASCDATA.CFG  $Temp_path}
   if [file exists default_conc] {file copy default_conc $Temp_path}
   if [file exists default_traj] {file copy default_traj $Temp_path}
   if [file exists default_exec] {file copy default_exec $Temp_path}
   if [file exists default_ftp]  {file copy default_ftp  $Temp_path}
}
}

# update in case values changed since directory was created
if [file exists SETUP.CFG] {file copy -force SETUP.CFG $Temp_path}
if [file exists default_conc] {file copy -force default_conc ${Temp_path}/CONTROL}
cd $Temp_path

#------------------------------------------------------
# pre-processor section applies only for MPI simulations

# M = matrix configuration 
# E = ensemble configuration 
# S = standard MPI run
#   = single processor run

set Arglist " "

if {"$Start" == "M"} {
   if { "$X_dir" == "" } {
      exec $exec_dir/latlon
   } else {
      exec $X_dir/xterm -fn fixed -e $exec_dir/latlon
   }
   if [file exists CONTROL] { } else {return}
}

if {"$Start" == "E"} {set Arglist $member1}

#------------------------------------------------------
# submit simulation

if {"$Start" == "E" || "$Start" == "M" || "$Start" == "S"} {

#  MPI variation
   if [file exists ${exec_dir}/${Model}] {
      if { "$X_dir" == "" } {
         exec $exec_dir/run_mpi.sh $Nproc $Model $Arglist &
      } else {
         exec $X_dir/xterm -fn fixed -e $exec_dir/run_mpi.sh $Nproc $Model $Arglist &
      }
   } else {
      msg_box "MPI executable $Model not found!"
   }

} else {
# single processor variation
   if [file exists ${exec_dir}/${Model}] {
      if { "$X_dir" == "" } {
         exec $exec_dir/${Model} &
      } else {
         exec $X_dir/xterm -fn fixed -e $exec_dir/${Model} & 
      }
   } else {
      msg_box "Executable ${Model} not found!"
   }
}

cd $Work_path
}
