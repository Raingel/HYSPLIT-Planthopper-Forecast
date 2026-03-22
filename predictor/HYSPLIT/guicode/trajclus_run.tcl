proc traj_clus_analysis {type} {

#-----------------------------------------------------------------------------
# TRAJ_CLUS.TCL: Trajectory cluster analysis main script
# Last Revised: 22 Jul 2005
#               08 Sep 2005 Input/cluster/results/archive
#               19 Aug 2006 cluster working directory
#               20 Aug 2007 cluster mean tdump filename 
#               07 Nov 2007 GIS
#               24 Nov 2008 View_plot initialized
#               08 Oct 2009 Account for relative directory structure
#               22 Dec 2009 Map_proj_cluster
#               30 Jun 2010 Created file_append to replace cmd.exe type
#               05 Nov 2010 Cleaned up script, reduced window size
#               19 Jul 2017 adjusted pady
#               08 Sep 2017 variable 'rslt' different from 'result'
#               26 Jul 2021 use svg instead of postscript
#-----------------------------------------------------------------------------

  global Infile html_dir Work_path wr
 
  if [winfo exists .trajclus] {destroy .trajclus}
  set wr .trajclus
  toplevel $wr
  wm title $wr "Trajectory Cluster Analysis (Jan. 2010)"
  wm  geometry $wr +5+5

  frame $wr.step1
  frame $wr.runID
  frame $wr.hours
  frame $wr.int
  frame $wr.skip
  frame $wr.enddir
  frame $wr.wrkdir
  frame $wr.arcdir
  frame $wr.mid3
 
  frame $wr.step2 
  frame $wr.rename
  frame $wr.make
  frame $wr.clus
  frame $wr.plot
  frame $wr.view
  frame $wr.ncout

  frame $wr.step3
  frame $wr.nc
  frame $wr.list
  frame $wr.disp

  frame $wr.sept
  frame $wr.exit

  pack $wr.step1             -side top -anchor w -pady 1
  pack $wr.step1 $wr.runID   -side top -anchor w -pady 1 
  pack $wr.hours $wr.int $wr.skip $wr.enddir -side top -anchor w -pady 1 
  pack $wr.wrkdir            -side top -anchor w -pady 1 
  pack $wr.arcdir            -side top -anchor w -pady 1
  pack $wr.mid3              -side top -anchor w -pady 1
 
  pack $wr.step2             -side top -anchor w -pady 1
  pack $wr.rename $wr.make  $wr.clus $wr.plot -side top -anchor w -pady 1
  pack $wr.view              -side top -anchor w -pady 1
  pack $wr.ncout             -side top -anchor w -pady 1

  pack $wr.step3             -side top -anchor w -pady 1
  pack $wr.nc                -side top -anchor w -pady 1 
  pack $wr.list $wr.disp     -side top -anchor w -pady 1

  pack $wr.sept              -side top -anchor w -pady 1
  pack $wr.exit              -side top -pady 1

#---------------------------------------------------------------------------------

  label $wr.step1.lab -width 62 -bg Gray65 -anchor w -text "Step 1: Inputs"
  pack $wr.step1.lab -side top -padx 2 -pady 4

  label $wr.runID.lab -text "               Run_ID: " -justify left  
  entry $wr.runID.ent -textvariable Run_ID -relief sunken -width 25  
  label $wr.runID.lab2 -text " " -justify left
  pack $wr.runID.lab $wr.runID.ent $wr.runID.lab2 -side left

  label $wr.hours.lab -text "     Hours to cluster: " 
  entry $wr.hours.ent -textvariable Hours_clus -relief sunken -width 4
  label $wr.hours.lab2 -text "                      "
  pack $wr.hours.lab $wr.hours.ent $wr.hours.lab2 -side left

  label $wr.int.lab -text "  Time interval (hrs): " 
  entry $wr.int.ent -textvariable Time_int -relief sunken -width 4
  label $wr.int.lab2 -text "                      "
  pack $wr.int.lab $wr.int.ent $wr.int.lab2 -side left

  label $wr.skip.lab -text "      Trajectory skip: "
  entry $wr.skip.ent -textvariable Traj_skip -relief sunken -width 4
  label $wr.skip.lab2 -text "                      "
  pack $wr.skip.lab $wr.skip.ent $wr.skip.lab2 -side left

  label $wr.enddir.lab -text "     Endpoints folder: " -justify left   
  entry $wr.enddir.ent -textvariable Endpts_dir -relief sunken -width 27
  label $wr.enddir.lab2 -text " " -justify left
  button $wr.enddir.note  -text Browse -width 8 -command {
   set temp [tk_chooseDirectory -title "Endpoints Directory Selection"]
   if {[string length $temp] > 0} {set Endpts_dir $temp}}
  pack $wr.enddir.lab $wr.enddir.ent $wr.enddir.lab2 $wr.enddir.note -side left 

  label $wr.wrkdir.lab -text "       Working folder: " -justify left   
  entry $wr.wrkdir.ent -textvariable Cluswork_dir -relief sunken -width 27
  label $wr.wrkdir.lab2 -text " " -justify left
  button $wr.wrkdir.note  -text Browse -width 8 -command {
   set temp [tk_chooseDirectory -title "Cluster Working Directory Selection"]
   if {[string length $temp] > 0} {set Cluswork_dir $temp}}
  pack $wr.wrkdir.lab $wr.wrkdir.ent $wr.wrkdir.lab2 $wr.wrkdir.note -side left

  label $wr.arcdir.lab -text "       Archive folder: " -justify left   
  entry $wr.arcdir.ent -textvariable Archive_dir -relief sunken -width 27
  label $wr.arcdir.lab2 -text " " -justify left
  button $wr.arcdir.note  -text Browse -width 8 -command {
   set temp [tk_chooseDirectory -title "Archive Directory Selection"]
   if {[string length $temp] > 0} {set Archive_dir $temp}}
  pack $wr.arcdir.lab $wr.arcdir.ent $wr.arcdir.lab2 $wr.arcdir.note -side left

#-->map projection options (in cluster.exe to convert to grid for calcs)

  label $wr.mid3.lab -text "   Projection (for 'Run cluster'):"
  radiobutton $wr.mid3.d0 -text "Auto"     -variable Map_proj_cluster -value "0" 
  radiobutton $wr.mid3.d1 -text "Polar"    -variable Map_proj_cluster -value "1" 
  radiobutton $wr.mid3.d2 -text "Lambert"  -variable Map_proj_cluster -value "2" 
  radiobutton $wr.mid3.d3 -text "Mercator" -variable Map_proj_cluster -value "3" 
  pack $wr.mid3.lab $wr.mid3.d0 $wr.mid3.d1 $wr.mid3.d2 $wr.mid3.d3 -side left

#-------------------------------------------------------------------------------

  label $wr.step2.lab -width 62 -bg Gray65 -anchor w -text "Step 2: Run Cluster Program"
  pack $wr.step2.lab -side top -padx 2 -pady 2

  label $wr.make.lab -text "        "
  button $wr.make.cont  -text "Make INFILE" -width 15 -command "Make_INFILE"
  button $wr.make.note  -text "Note" -width 6 -bg "CadetBlue3" \
           -command "msg_box {All files named with the Base Name in the endpts folder will be used.}"
  label  $wr.make.lab2 -text "Base Name:"
  entry  $wr.make.ent -textvariable Infile -relief sunken -width 8
  pack $wr.make.lab $wr.make.cont $wr.make.lab2 $wr.make.ent $wr.make.note -side left -padx 4

  label $wr.clus.lab -text "        "
  button $wr.clus.cont  -text "Run cluster analysis" -width 36 -command "Run_cluster"
  pack $wr.clus.lab $wr.clus.cont -side left -padx 4

  label $wr.plot.lab -text "        "
  button $wr.plot.cont  -text "Display total spatial variance" -width 36 -command "Plot_outcomes"
  pack $wr.plot.lab $wr.plot.cont -side left -padx 4
 
  label $wr.view.lab -text "         View possible final number of clusters -" -justify left   
  pack $wr.view.lab -side left -padx 4

  # (Ccrit is set in Clus_list_outcomes below)
  label $wr.ncout.lab -text "         Criterion (%):"
  radiobutton $wr.ncout.p0 -text "20" -variable Cflg -value "1" -width 3 
  radiobutton $wr.ncout.p1 -text "30" -variable Cflg -value "0" -width 3
  button $wr.ncout.cont  -text "Run" -width 10 -command "List_outcomes"
  pack $wr.ncout.lab $wr.ncout.p0 $wr.ncout.p1 $wr.ncout.cont -side left -padx 4

#----------------------------------------------------------------------------------
  label $wr.step3.lab -width 62 -bg Gray65 -anchor w \
     -text "Step 3: Get Results (repeat for different number of clusters)"
  pack $wr.step3.lab -padx 2 -pady 2

  label $wr.nc.lab0 -text "                     Number of Clusters: "
  entry $wr.nc.ent0 -textvariable Nclus -relief sunken -width 4
  button $wr.nc.cont  -text "Note" -width 6 -bg "CadetBlue3" -command \
     "msg_box {Generally, choose number before 'different' clusters are combined; \
      ie before large increase in %TSV as shown in plot}"
  pack $wr.nc.lab0 $wr.nc.ent0 $wr.nc.cont -side left -padx 4

  label  $wr.list.lab -text "   Assign trajectories to clusters: "
  checkbutton $wr.list.psv -variable View_cluslist -text "View" -width 6 -background LightGrey
  button $wr.list.cont  -text "Run" -width 18 -command "List_results"
  pack   $wr.list.lab $wr.list.psv $wr.list.cont -side left -padx 4

  label  $wr.disp.labl -text "      "
  button $wr.disp.mean  -text "Display Means"     -width 18 -command "Plot_means 1"
  button $wr.disp.clus  -text "Display Clusters"  -width 18 -command "Plot_each_cluster 1"
  button $wr.disp.notu  -text "Display Not Used"  -width 18 -command "Traj_not_used"
  pack   $wr.disp.labl $wr.disp.mean $wr.disp.clus $wr.disp.notu -side left -padx 4

#---------------------------------------------------------------------------------
  label $wr.sept.lab \
    -text " _________________________________________________________________________"
  pack $wr.sept.lab -side left

#-->termination ------------------------------------------------------------------
  button $wr.exit.arch             -text Archive -width 18 -command "Archive"
  button $wr.exit.dismiss  -bg red -text Quit -width 18 -command "Done"
  button $wr.exit.help             -text Help -width 18 \
         -command "load_html [file join $html_dir S255.htm ] "
  pack $wr.exit.arch $wr.exit.dismiss $wr.exit.help -side left -padx 4 -pady 1

  set_default $type

}
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------

proc Rename_tdump {} {

#  Took the following out of the HELP document when took this option out of the GUI.
 
#<p><u>Rename web tdumps</u>  Trajectories created on the web with
#autotraj have filenames of format tdump.[Julian Day].  These can be used as is OR
#run this rename script to rename the files to
#the format of the HYSPLIT PC- <i>Run Daily</i> with the year-month-day-hour in the 
#filename.  </p>

   global Endpts_dir

   goto_cluswork

   foreach f [glob $Endpts_dir/tdump.*] {
      set dot [string last . $f]
      set klen [string length $f]
      set JDAY [string range $f [expr $dot+1] [expr $klen-1] ]

      set g [open $Endpts_dir/DATES.1451 r] 
      gets $g line
      set jday [lindex [split $line] 0]
      while {$JDAY != $jday} {
         gets $g line
         set jday [lindex [split $line] 0]
      }
      if {"$JDAY" == "$jday"} {
         set DATE [lindex [split $line] 1]
         file rename $Endpts_dir/tdump.$JDAY $Endpts_dir/tdump$DATE
      } else {
         msg_box "ERROR renaming file $f"
         return
      }
      close $g
   }
   msg_box "     Rename complete     " 
}

#---------------------------------

proc Make_INFILE {} {

  global Endpts_dir Infile

  goto_cluswork

  if [file exists $Endpts_dir] {

     if [file exists INFILE] {file delete -force INFILE}

     set In_file INFILE

     set g [open $In_file w]
     foreach f [glob $Endpts_dir/*${Infile}*] {puts $g $f}


     close $g
     msg_box "                      ${In_file} created                     "

  } else {
  
     msg_box "                   $Endpts_dir not found                 "
  }  

}

#---------------------------------

proc Run_cluster {} {

  global Hours_clus Time_int Traj_skip Map_proj_cluster
  global X_dir exec_dir result log tcl_platform
# 9-8-17 add rslt variable here and use below
  global rslt
# 9-8-17 a user discovered that for 'big' runs (search 40000) when get
# Hysplit4 System Message that says
#  This run may take a long time and use much memory.
#  If you exit, cluster may continue running 
#   and has buttons Quit, Continue
# If you hit Quit or Continue before the running of cluster.exe is done,
#   it will hang and you need to exit gui and start again.
# What should be done is to wait until cluster.ext is done,
#     "Pass X out of X"
#     Complete Cluster ... "
#   then hit either Quit or Continue
# User suggested fix made below changing variable "result" to "rslt"
#   in the "catch" line
# Can do a "grep catch * | grep result" to find other instances of this 
#  e.g. hymenu.tcl.  However in hymenu.tcl for Traj_run and Conc_run, 
#  the msg window then goes on to show the "Calculation started..."
#  and only has button Exit, there is no Quit/Continue.
# Hence at this time only will change result to rslt here in trajrun_clus.tcl
# Also add proc Log_rslt here (proc Log is in hymenu.tcl)

  goto_cluswork

# save CCONTROL   		

  set Out_dir ./

  if [file exists CCONTROL] {file delete CCONTROL}
  
  set f [open "CCONTROL" w]

  puts $f "$Hours_clus"
  puts $f "$Time_int"
  puts $f "$Traj_skip"
  puts $f "$Out_dir"
  puts $f "$Map_proj_cluster"
  destroy .trajinit
  close $f

# count number of trajectories     
                    ##### cluster.exe runs before close msg box #####
  if [file exists INFILE] { } else { 
      msg_box "     INFILE not found     "
      return 
  }
  set g [open "INFILE" r] 
  set i 0
  foreach line [split [read $g] \n] {
    incr i
  }
  close $g
  # $i is number of tdump files listed in INFILE
  set MaxEndpts [expr ${i}*${Hours_clus}/${Time_int}]
  if {$MaxEndpts >= 40000} {
     msg_box "   This run may take a long time and use much memory.\n
If you exit, cluster may continue running"
  }
  
# run cluster program

  if [file exists CMESSAGE] {file delete CMESSAGE}
  if [file exists CLUSTER] {file delete CLUSTER}
  if [file exists DELPCT] {file delete DELPCT}
  if [file exists CLUSTERno] {file delete CLUSTERno}

  if [file exists CCONTROL] { } else { return }
  
  if { "$tcl_platform(platform)" == "unix" } {
     set code "${exec_dir}/cluster"
     set xops "|$code |& cat"
  } else {
     set code ${exec_dir}/cluster.exe
     set xops "|$code"
  }

  if [file exists $code] {

     set log [ScrollText .f]
     $log configure -cursor watch
     $log insert end "Model started ... it is slower at the beginning\n"
     update

   # 9-8-17 change 'result' to 'rslt'
     if [catch {open $xops} rslt] {
        $log insert end $rslt
     } else {
        fileevent $rslt readable Log_rslt
     }
   # 9-8-17 end change 'result' to 'rslt'

  } else {
     msg_box " Executable $code not available! "
  }

}

#---------------------------------

proc List_outcomes {} {

  # run clusend to list ALL possible outcomes given cut criterion

  global Cflg Ccrit
  global X_dir exec_dir tcl_platform
  global Ccrit
  #global log
  global DELPCT_file
  global Textoutcomes_file

  goto_cluswork

  if { $Cflg == 0 } {
     set Ccrit 30
  } else {
     set Ccrit 20
  }

  set Clusend_in ${DELPCT_file}
  set Clusend_out ${Textoutcomes_file}
  set Minc 2		
  set Maxc 20		
  set Mint 15		

  if [file exists ${Clusend_out}] {file delete ${Clusend_out}}

  set arg1 -i  ; append arg1 ${Clusend_in}
  set arg2 -o  ; append arg2 ${Clusend_out}
  set arg3 -n  ; append arg3 $Minc
  set arg4 -a  ; append arg4 $Maxc
  set arg5 -t  ; append arg5 $Mint
  set arg6 -p  ; append arg6 $Ccrit


  if { "$tcl_platform(platform)" == "unix" } {
     if { "$X_dir" == "" } {
        exec $exec_dir/clusend $arg1 $arg2 $arg3 $arg4 $arg5 $arg6
     } else {
        exec $X_dir/xterm -fn fixed \
                 -e $exec_dir/clusend $arg1 $arg2 $arg3 $arg4 $arg5 $arg6
     }
  } else {
     exec $exec_dir/clusend.exe $arg1 $arg2 $arg3 $arg4 $arg5 $arg6
  }
 
  set log [ScrollText .f]
  $log insert end "POSSIBLE NUMBER OF CLUSTERS AND PERCENT CHANGE...\n"
  if [file exists ${Textoutcomes_file}] {
     set fileid [open "${Textoutcomes_file}" r]
     while {[eof $fileid] != 1} {
        gets $fileid cline
        $log insert end $cline\n
     }
     close $fileid
  }

# read the first number of clusters in CLUSEND 		????? DOES THIS WORK ?????
# 						Yes, but doesn't automatically
#                                               put that Nclus in the Entry box			
  if [file exists CLUSEND] {
    set f [open CLUSEND r]
    gets $f tmp
    set Nclus [lindex $tmp 0]
    close $f
  }

  #update traj_clus_analysis  ##get error message must be idletasks
  ## this still doesn't work, nor plain update
   update idletasks
}

#---------------------------------

proc Plot_outcomes {} {

  # run clusplot to make plot to inspect for ALL possible outcomes

  global X_dir exec_dir tcl_platform
  global DELPCT_file Plotoutcomes_file Run_ID

  goto_cluswork

  set PlotDelpct_in ${DELPCT_file}
  set PlotDelpct_out ${Plotoutcomes_file}

  if [file exists ${PlotDelpct_out}] {file delete ${PlotDelpct_out}}

  set arg1 -i  ; append arg1 ${PlotDelpct_in}
  set arg2 -o  ; append arg2 ${PlotDelpct_out}
  set arg3 -l  ; append arg3 ${Run_ID}
  set arg_ext "+g1"

  if { "$tcl_platform(platform)" == "unix" } {
     if { "$X_dir" == "" } {
        exec $exec_dir/clusplot $arg_ext $arg1 $arg2 $arg3
     } else {
        exec $X_dir/xterm -fn fixed -e $exec_dir/clusplot $arg_ext $arg1 $arg2 $arg3
     }
  } else {
     exec $exec_dir/clusplot.exe $arg_ext $arg1 $arg2 $arg3
  }

  if [file exists ${PlotDelpct_out}] {
     svg_box ${PlotDelpct_out}
  } else {
     msg_box "   ${PlotDelpct_out} not found"
  }       

}

#---------------------------------

proc List_results {} {

  # run cluslist to list results for given number of clusters

  global X_dir exec_dir tcl_platform
  global View_cluslist
  global Nclus
  #global log
  global Master_file ClusListing_file Cluswork_dir


  goto_cluswork

  if {$Nclus == 0} {
     msg_box "ERROR - Must set number of clusters"

  } else {

    set Cluslist_in ${Master_file}
    set Cluslist_out ${ClusListing_file}_${Nclus}

    if [file exists ${Cluslist_out}] {file delete ${Cluslist_out}}

    set arg1 -i  ; append arg1 ${Cluslist_in}
    set arg2 -o  ; append arg2 ${Cluslist_out}
    set arg3 -n  ; append arg3 $Nclus

    if { "$tcl_platform(platform)" == "unix" } {
       if { "$X_dir" == "" } {
          exec $exec_dir/cluslist $arg1 $arg2 $arg3 
       } else {
          exec $X_dir/xterm -fn fixed -e $exec_dir/cluslist $arg1 $arg2 $arg3 
       }
    } else {
       exec $exec_dir/cluslist.exe $arg1 $arg2 $arg3 
    } 

    # add trajs not clustered, cluster #0, if any to CLUSLIST_${Nclus} file
    if [file exists ${Cluslist_out}] {
       if [file exists CLUSTERno] {
          file copy -force CLUSTERno concatfile
          if { "$tcl_platform(platform)" == "unix" } {
            exec cat ${Cluslist_out} >> concatfile
          } else {
            file_append $Cluslist_out concatfile
          }       
       file rename -force concatfile ${Cluslist_out} 
       }     
    }
         
    if {$View_cluslist == 1} {
      set log [ScrollText .f]
      $log insert end " CL# #TRJ/CL YR  MO DA HR file#  filename  \n"
      if [file exists ${Cluslist_out}] {
          set fileid [open "${Cluslist_out}" r]
          while {[eof $fileid] != 1} {
             gets $fileid cline
             $log insert end $cline\n
          }
          close $fileid
      
      }

    } else {
          msg_box "                    ${Cluslist_out} created                   "
    }

  }

}

#---------------------------------

proc Plot_means {ClusNo} {

  ## This is run as "Plot_means 0" to include trajs not used (cluster 0);
  ##   otherwise as "Plot_means 1"
  ## ClusNo0 variable is not needed.

  global X_dir exec_dir Trajpts_file tcl_platform
  global Psout_file 
  global Nclus 
  global tcl_dir
  global Psout_file 
  global ClusListing_file ListTrajs_file ListMeans_file
  global Map_lat Map_lon
  global home_dir

  if {! [file exists $tcl_dir] } {set tcl_dir ${home_dir}/guicode}

  goto_cluswork
   
  if {$Nclus == 0} {
     msg_box "ERROR - Must set number of clusters"

  } else {

     ###################
     ### run clusmem ###
     ###################
     # create file listing tdump filenames for each cluster
     # output files are named TRAJ.INP.Cx_CN, x=cluster number, CN=number of clusters

     set clusmem_in ${ClusListing_file}_${Nclus}
     set clusmem_out ${ListTrajs_file}

     for { set i $ClusNo } { $i <= $Nclus } {incr i} {
       if [file exists ${clusmem_out}$i] {file delete ${clusmem_out}${i}_${Nclus} }
     }
  
     if { "$tcl_platform(platform)" == "unix" } {
        if { "$X_dir" == "" } {
           exec $exec_dir/clusmem -i${clusmem_in} -o${clusmem_out}
        } else {
          exec $X_dir/xterm -fn fixed -e $exec_dir/clusmem -i${clusmem_in} -o${clusmem_out}
        }
     } else {
        exec $exec_dir/clusmem.exe -i${clusmem_in} -o${clusmem_out} 
     } 

     ####################
     ### run trajmean ###
     ####################
     # write cluster-mean trajs in tdump-format 
     # output files named Cn_Nmean.tdump

     for { set i $ClusNo } { $i <= $Nclus } {incr i} {
       if [file exists C${i}_${Nclus}mean.tdump] {
           file delete C${i}_${Nclus}mean.tdump
       }

       if {$ClusNo == 0} {
          if [file exists ${ListTrajs_file}0_${Nclus}] {
          } else {
            msg_box "All trajectories used, so cannot include those not used"
          # houseclean - delete TRAJ.INP.C files
            for { set i $ClusNo } { $i <= $Nclus } {incr i} {
              if [file exists ${ListTrajs_file}${i}_${Nclus}] {
                 file delete ${ListTrajs_file}${i}_${Nclus}
              }
            } 
            return
          }
       }

       if { "$tcl_platform(platform)" == "unix" } {
         if { "$X_dir" == "" } {
             exec $exec_dir/trajmean \
                -i+${ListTrajs_file}${i}_${Nclus} -oC${i}_${Nclus}mean.tdump
          } else {
            exec $X_dir/xterm -fn fixed -e $exec_dir/trajmean \
                -i+${ListTrajs_file}${i}_${Nclus} -oC${i}_${Nclus}mean.tdump
          }
       } else {
          exec $exec_dir/trajmean.exe \
                -i+${ListTrajs_file}${i}_${Nclus} -oC${i}_${Nclus}mean.tdump 
       } 

     }

     #####################################################
     # create file listing cluster-mean tdump filenames  (need to write directory???)
     #####################################################
     set f [open "${ListMeans_file}_${Nclus}" w]
     for { set i $ClusNo } { $i <= $Nclus } {incr i} {
       puts $f "C${i}_${Nclus}mean.tdump"
     }
     close $f

    ####################
    ### run merglist ###
    ####################
    # put cluster-mean tdumps in one file for plot (output file Cmean.tdump)
 
    if { "$tcl_platform(platform)" == "unix" } {
       if { "$X_dir" == "" } {
          exec $exec_dir/merglist -i+${ListMeans_file}_${Nclus} -oCmean${ClusNo}_${Nclus}
       } else {
         exec $X_dir/xterm -fn fixed -e $exec_dir/merglist \
                                  -i+${ListMeans_file}_${Nclus} -oCmean${ClusNo}_${Nclus}
       }
    } else {
       exec $exec_dir/merglist.exe -i+${ListMeans_file}_${Nclus} -oCmean${ClusNo}_${Nclus} 
    } 

    ### houseclean - delete TRAJ.INP.C files
    for { set i $ClusNo } { $i <= $Nclus } {incr i} {
      if [file exists ${ListTrajs_file}${i}_${Nclus}] {
         file delete ${ListTrajs_file}${i}_${Nclus}
      }
    } 
    file delete ${ListMeans_file}_${Nclus}

   #############################################################
   ### set defaults for means plot in Cluster Display window ###
   #############################################################

   # read traj source lat lon
     if [file exists C1_${Nclus}mean.tdump] {
        set f [open C1_${Nclus}mean.tdump r]
        gets $f tmp
        gets $f tmp
        gets $f tmp
        gets $f tmp
        set Map_lat [lindex $tmp 4]
        set Map_lon [lindex $tmp 5]
        close $f
     }

     set Psout_file clusmean${ClusNo}_${Nclus}
     set Trajpts_file Cmean${ClusNo}_${Nclus}.tdump
     set Mean 0

     ##################################
     ### run Cluster Display window ###
     ##################################     
     source [file join  $tcl_dir trajclus_disp.tcl ]
     clus_plot $Mean $ClusNo

  }

}

#---------------------------------

proc Plot_each_cluster {ClusNo} {

  global X_dir exec_dir Trajpts_file tcl_platform
  global Psout_file 
  global Nclus
  global tcl_dir
  global Map_lat Map_lon
  global View_clusters
  global home_dir

  if {! [file exists $tcl_dir] } {set tcl_dir ${home_dir}/guicode}

  goto_cluswork

  if {$Nclus == 0} {
     msg_box "ERROR - Must set number of clusters"

  } else {

   #############################################################
   ### set defaults for means plot in Cluster Display window ###
   #############################################################

   # read traj source lat lon
     if [file exists Cmean_${Nclus}.tdump] {
        set f [open Cmean_${Nclus}.tdump r]
        gets $f tmp
        gets $f tmp
        gets $f tmp
        gets $f tmp
        set Map_lat [lindex $tmp 4]
        set Map_lon [lindex $tmp 5]
        close $f
     }

     set Psout_file clusters_${Nclus}
     set Mean 1

     ##################################
     ### run Cluster Display window ###
     ##################################     
     source [file join  $tcl_dir trajclus_disp.tcl ]
     clus_plot $Mean $ClusNo
  }
   
}
#---------------------------------

proc Traj_not_used {} {

  global Nclus

  goto_cluswork
  
# display trajs in cluster #0 -- created previously in "Display Clusters"
  if [file exists Clusplot0_$Nclus.html] {
     svg_box Clusplot0_$Nclus.html
  } 

# display clusters means, including trajs not clustered
  Plot_means 0

}

#---------------------------------
proc Archive {} {

  global result Archive_dir Run_ID

  global Master_file DELPCT_file Textoutcomes_file Plot_outcomes_file
  global ClusListing_file

  # set Archive_dir [file join ${Archive_pdir} ${Run_ID}]

  goto_cluswork

# disable to prevent recursion with relative paths
  if {! [file exists $Archive_dir]} {
      msg_box "Archive DIR not found in [pwd]; create ${Archive_dir}?"
      tkwait window .msg_win
      if {$result != 0} {
         return
      } else {
         file mkdir $Archive_dir
      }
  }

# houseclean
  if [file exists TCLUS] {file delete TCLUS}
  if [file exists TNOCLUS] {file delete TNOCLUS}

###################################
# move files to Archive directory #
###################################

# independent of number of clusters
  foreach item [list CCONTROL    \
                     CLUSEND     \
                     CLUSTER     \
                     CLUSTERno   \
                     CMESSAGE    \
                     DELPCT      \
                     clusplot.ps \
                     logocon.gif \
                     noaa_google.gif \
                     INFILE ]       {
    if [file exists ${item}] {
      file rename -force $item ${Archive_dir}
    }
  }

# for all number of clusters chosen
  foreach f [glob -nocomplain CLUSLIST*] {
     file rename -force $f ${Archive_dir}
  }
  foreach f [glob -nocomplain C*_*mean.tdump] {
     file rename -force $f ${Archive_dir}
  }
  foreach f [glob -nocomplain C*_*.tdump] {
     file rename -force $f ${Archive_dir}
  }
  foreach f [glob -nocomplain clusmean*_*.ps] {
     file rename -force $f ${Archive_dir}
  }
  foreach f [glob -nocomplain clusplot*_*.ps] {
     file rename -force $f ${Archive_dir}
  }
  foreach f [glob -nocomplain clusters_*.ps] {
     file rename -force $f ${Archive_dir}
  } 
  foreach f [glob -nocomplain clusmean*_*.gif] {
     file rename -force $f ${Archive_dir}
  } 
  foreach f [glob -nocomplain F??-clusters_*.gif] {
     file rename -force $f ${Archive_dir}
  } 
  foreach f [glob -nocomplain GIS*] {
     file rename -force $f ${Archive_dir}
  }
  foreach f [glob -nocomplain *.sh*] {
     file rename -force $f ${Archive_dir}
  }
  foreach f [glob -nocomplain *.dbf] {
     file rename -force $f ${Archive_dir}
  }
  foreach f [glob -nocomplain *km*] {
     file rename -force $f ${Archive_dir}
  }

  msg_box "           Files moved to $Archive_dir           "

}

#---------------------------------

proc Done {} {

  global ListTrajs_file Nclus
  global Work_path
  global wr

# houseclean
  if [file exists TCLUS]    {file delete TCLUS}
  if [file exists TNOCLUS]  {file delete TNOCLUS}
  if [file exists ${ListTrajs_file}0_${Nclus}] {file delete ${ListTrajs_file}0_${Nclus} }
 
# return to hysplit working directory and close all cluster windows
  cd $Work_path
  destroy $wr

}

#---------------------------------
proc goto_cluswork {} {

  global env exec_dir Cluswork_dir Work_path
  global result

# cd to cluster working directory 
  if {! [file exists $Cluswork_dir]} {
      msg_box "Working DIR not found in [pwd]; create ${Cluswork_dir}?"
      tkwait window .msg_win
      if {$result != 0} {
         return
      } else {
         file mkdir $Cluswork_dir
      }
  }
  cd $Cluswork_dir

# check if path to executable has been broken
  if {! [file exists $exec_dir] } { 
     cd $Work_path
     cd $exec_dir
     set exec_dir [pwd]
     cd $Cluswork_dir
  }

}

#---------------------------------
proc set_default {type} {

  global Hours_clus Time_int Traj_skip Nclus Cflg ClusNo
  global View_plot View_means View_clusters View_cluslist
  global clus_dir Endpts_dir Archive_dir Cluswork_dir
  global Master_file DELPCT_file
  global Textoutcomes_file Plotoutcomes_file
  global ClusListing_file ListTrajs_file ListMeans_file tdumpMeans_file
  global LABELS.CFG_file
  global Run_ID Infile Map_proj_cluster

                         # ClusNo=1 ==> do not include cluster #0
# set ClusNo 1
  
  set Archive_dir        ${clus_dir}/archive
  set Cluswork_dir       ${clus_dir}/working

  set Master_file        CLUSTER
  set DELPCT_file        DELPCT
  set Textoutcomes_file  CLUSEND
  set Plotoutcomes_file  clusplot.html
  set ClusListing_file   CLUSLIST
  set ListTrajs_file     TRAJ.INP.C
  set ListMeans_file     TRAJ.INP.mean
  set LABELS.CFG_file    LABELS.CFG
  set Infile             tdump

  set Map_proj_cluster 0  
  set Time_int      1
  set Traj_skip     1
  set Cflg          0
  set View_plot     0
  set View_means    1
  set View_clusters 0
  set View_cluslist 1 

  if { "$type" == "E" } {
    set Run_ID      Example
    set Hours_clus  12
    set Nclus       7                   
    set Endpts_dir  ${clus_dir}/example/endpts/Example
  } else {
    set Run_ID      Standard
    set Hours_clus  36
    set Nclus       1
    set Endpts_dir  ${clus_dir}/endpts
  }

}

#----------------------------------

proc file_append {file1 file2} {

   set file1id [open $file1 r]
   set file2id [open $file2 a]
   while {[eof $file1id] != 1} {
      gets $file1id  cline
      if { "$cline" == "" } { } else {puts $file2id $cline}
   }
   close $file1id
   close $file2id
}

#----------------------------------

proc Log_rslt {} {
global rslt log
if [eof $rslt] {
   catch {close $rslt}
   $log configure -cursor arrow
   bell
} else {
   gets $rslt line
   $log insert end $line\n
   $log see end
}
}

