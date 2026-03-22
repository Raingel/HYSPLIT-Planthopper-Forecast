proc clus_plot {Mean ClusNo} {

#-----------------------------------------------------------------------------
# CLUS_DISP.TCL: trajectory cluster analysis display script
# Last Revised: 11 Mar 2002
#               13 Aug 2002
#               07 Oct 2002
#               17 Jun 2003 - option to view ps
#               20 Nov 2003 - meteo vertical display
#               05 Dec 2003 - fix map center and rings option
#               14 Apr 2004 - changed endpoints variable name
#               21 Sep 2004 - don't destroy widget after display
#               24 Oct 2004 - global postscript file name
#               10 Jan 2005 - revised xterm link
#               27 May 2005 - browse option
#               22 Jul 2005 - modified from traj_disp.tcl
#                                 took out tdump filename input
#                                 added "none" option to vertical cross-section
#                                 added Time Label Reference
#                                 added Plot Trajectory Duration
#                                 added checkbox to make GIF
#               28 Jul 2006 - cluster working directory
#               07 Nov 2007 - GIS
#               08 Jul 2009 - catps2ps.exe
#               13 Aug 2009 - browse button for map background
#               08 Oct 2009 - account for relative directory structure
#               30 Jun 2010 - Created file_append to replace cmd.exe type
#               12 Sep 2016 - corrected browse for shapefiles.txt
#               07 Nov 2016 - do not permit shapefile copy to itself
#               19 Jul 2017 - adjusted pady
#               28 Jul 2017 - changed initialization variable to LabRef
#               13 Dec 2019 - replace trajplot or trajplot.exe with trajplot.py
#               16 Jan 2020 - add option to use FORTRAN or Python trajplot.
#               21 Jul 2021 - use svg instead of postscript. compact Python tab.
#               31 May 2023 - correct GIS filenames by replacing _ps with _html.
#               07 Nov 2023 - change street maps to OPEN_STREET and OPEN_TOPO.
#-----------------------------------------------------------------------------

global html_dir Cluswork_dir Trajpts_file  
global qpnt Kagl Zoom Color Label Psout_file 
global Map_file Map_proj Igis Igif View_plot
global Ring Map Ring_num Ring_dis Map_lat Map_lon
global LabRef Lhrs
global Work_path
global PyDebug PySource_time_zone PyStreet_map PyOutput_format
global PyPreferred

if [winfo exists .clusdisp] {destroy .clusdisp}
set wr .clusdisp
toplevel $wr
wm title $wr " Trajectory Cluster Display   "
wm  geometry $wr +25+25

frame $wr.mid0
frame $wr.mid1
frame $wr.mid2
frame $wr.mid3
frame $wr.setm
frame $wr.mid4
frame $wr.mid4a
frame $wr.mid4b
frame $wr.mid5
frame $wr.mid6
frame $wr.bot

ttk::notebook $wr.lang
frame $wr.lang.py
frame $wr.lang.f
$wr.lang add $wr.lang.f -text "Fortran"
$wr.lang add $wr.lang.py -text "Python"

pack  $wr.mid0 $wr.mid1 $wr.mid2 $wr.mid3 $wr.setm $wr.mid4 $wr.mid4a $wr.mid4b \
  $wr.mid5 $wr.mid6 $wr.lang $wr.bot -side top -pady 2 -padx 10
pack configure $wr.lang -expand true -fill x

##-->input file (let filename be forced for clusters)
label $wr.mid0.lab -text "Input Endpoints:" -state disabled
entry $wr.mid0.ent -textvariable Trajpts_file -relief sunken -width 15 \
    -state disabled -bg cornsilk3 -fg Gray90
button $wr.mid0.win  -text Browse -width 8 -command {
   set temp [tk_getOpenFile -title "File Selection"]
   if {[string length $temp] > 0} {set Trajpts_file $temp}} -state disabled
pack $wr.mid0.lab $wr.mid0.ent $wr.mid0.win -side left -padx 5

#-->output file
label $wr.mid1.lab -text "Output File: "
entry $wr.mid1.ent -textvariable Psout_file -relief sunken -width 15
checkbutton $wr.mid1.gis -variable Igis -text "GIS" -background grey 
checkbutton $wr.mid1.psv -variable View_plot -text "View" -background grey
#checkbutton $wr.mid1.gif -variable Igif -text "GIF" -background grey
#pack $wr.mid1.lab $wr.mid1.ent $wr.mid1.gis $wr.mid1.psv $wr.mid1.gif -side left -padx 5
pack $wr.mid1.lab $wr.mid1.ent $wr.mid1.gis $wr.mid1.psv -side left -padx 5

#-->map background file
label $wr.mid2.lab -text "Map Background:"
entry $wr.mid2.ent -textvariable Map_file -relief sunken -width 35
button $wr.mid2.win  -text Browse -width 8 -command {
   set temp [tk_getOpenFile -title "File Selection"]
   if {[string length $temp] > 0} {
      set cshp [string last shapefiles $temp]
      if {$cshp > 0} {
         if {[file dirname $temp] != [pwd]} {file copy -force $temp .}
         set Map_file [string range $temp $cshp end]
      } else {
         set Map_file $temp
      }
   }
}
pack $wr.mid2.lab $wr.mid2.ent $wr.mid2.win -side left -padx 4

#-->map projection options
label $wr.mid3.lab -text "   Projection:"
radiobutton $wr.mid3.d0 -text "Auto"     -variable Map_proj -value "0" 
radiobutton $wr.mid3.d1 -text "Polar"    -variable Map_proj -value "1" 
radiobutton $wr.mid3.d2 -text "Lambert"  -variable Map_proj -value "2" 
radiobutton $wr.mid3.d3 -text "Mercator" -variable Map_proj -value "3" 
pack $wr.mid3.lab $wr.mid3.d0 $wr.mid3.d1 $wr.mid3.d2 $wr.mid3.d3 -side left

#-->optional map location set   
frame $wr.setm.rings
frame $wr.setm.centr
pack $wr.setm.rings $wr.setm.centr -side left -padx 10

label $wr.setm.rings.lab  -text " Rings: Number  Dist(km)"
checkbutton $wr.setm.rings.ent0 -variable Ring -text "Set" -background grey
entry $wr.setm.rings.ent1 -textvariable Ring_num -width 6
entry $wr.setm.rings.ent2 -textvariable Ring_dis -width 6
pack $wr.setm.rings.lab 
pack $wr.setm.rings.ent0 $wr.setm.rings.ent1 $wr.setm.rings.ent2 -side left -padx 5 

label $wr.setm.centr.lab  -text "Center:  Lat    Long"
checkbutton $wr.setm.centr.ent0 -variable Map -text "Set" -background grey
entry $wr.setm.centr.ent1 -textvariable Map_lat -width 6
entry $wr.setm.centr.ent2 -textvariable Map_lon -width 6 
pack $wr.setm.centr.lab 
pack $wr.setm.centr.ent0 $wr.setm.centr.ent1 $wr.setm.centr.ent2 -side left -padx 5

#-->select label interval
label $wr.mid4.lab -text "Label Source     Time Label Interval (hrs):" 
pack $wr.mid4.lab -side top

radiobutton $wr.mid4.on  -text "On"  -variable qpnt -value "1" 
radiobutton $wr.mid4.off -text "Off" -variable qpnt -value "0"
pack $wr.mid4.on $wr.mid4.off -side left 

label $wr.mid4.lab2 -text "  "
pack $wr.mid4.lab2 -side left

set d 0
foreach item [list 0 1 3 6 12 24] {
   radiobutton $wr.mid4.$d -variable Label -text $item -value $item
   pack $wr.mid4.$d -side left
   incr d
   }

#-->time label reference
label $wr.mid4a.lab -text "Time Label Reference:"
radiobutton $wr.mid4a.start -text "Trajectory Start"  -variable LabRef -value "1" 
radiobutton $wr.mid4a.synop -text "Synoptic" -variable LabRef -value "0" \
         -bg LightGray -disabledforeground DarkGray -state disabled
pack $wr.mid4a.lab $wr.mid4a.synop $wr.mid4a.start -side left

#-->hours to plot
label $wr.mid4b.lab -text "Plot Trajectory Duration (hrs):"
entry $wr.mid4b.ent -textvariable Lhrs -width 4
pack $wr.mid4b.lab $wr.mid4b.ent -side left

#-->select height display options
label $wr.mid5.lab -text "Vertical Coordinate:"
pack $wr.mid5.lab -side top
radiobutton $wr.mid5.d0 -text "Pressure" -variable Kagl -value "0"
radiobutton $wr.mid5.d1 -text "Meters-agl" -variable Kagl -value "1"
radiobutton $wr.mid5.d2 -text "Theta" -variable Kagl -value "2" 
radiobutton $wr.mid5.d3 -text "Meteo-varb" -variable Kagl -value "3"
radiobutton $wr.mid5.d4 -text "None" -variable Kagl -value "4" 
pack $wr.mid5.d0 $wr.mid5.d1 $wr.mid5.d2 $wr.mid5.d3 $wr.mid5.d4 -side left

#-->zoom factor slider bar      
label $wr.mid6.lab -text "Least Zoom -------------------> Most Zoom"
scale $wr.mid6.d0 -orient horizontal -length 400 -from 0 -to 100 \
  -tickinterval 10 -variable Zoom -resolution 5 
pack $wr.mid6.lab $wr.mid6.d0 -side top -pady 4

#-->Python options
label $wr.lang.f.lab -text "Output will be in SVG if GIF not selected."
checkbutton $wr.lang.f.gif -variable Igif -text "GIF" -background grey
pack $wr.lang.f.lab $wr.lang.f.gif -side top -pady 5 -fill y

frame $wr.lang.py.row1
label $wr.lang.py.row1.out_format_lab -text "Output Format:"
ttk::combobox $wr.lang.py.row1.out_format_cb -textvariable PyOutput_format \
    -values [list pdf ps jpg png svg tif] -width 5 -state readonly
$wr.lang.py.row1.out_format_cb set pdf

label $wr.lang.py.row1.street_map_lab -text "Street Map:"
ttk::combobox $wr.lang.py.row1.street_map_cb -textvariable PyStreet_map \
    -values [list "NOT_USED" OPEN_STREET OPEN_TOPO] -width 15 -state readonly
$wr.lang.py.row1.street_map_cb set "NOT_USED"
pack $wr.lang.py.row1.out_format_lab $wr.lang.py.row1.out_format_cb \
    $wr.lang.py.row1.street_map_lab $wr.lang.py.row1.street_map_cb \
    -side left -padx 5 -fill x

frame $wr.lang.py.misc
#checkbutton $wr.lang.py.misc.source_tz -variable PySource_time_zone -text "Source time zone" -background grey
checkbutton $wr.lang.py.misc.debug -variable PyDebug -text "Debug messages" -background grey
#pack $wr.lang.py.misc.source_tz $wr.lang.py.misc.debug -side left -padx 5
pack $wr.lang.py.misc.debug -side left -padx 5

pack $wr.lang.py.row1 $wr.lang.py.misc -side top -pady 5 -padx 10

if { $PyPreferred } {
    $wr.lang select $wr.lang.py
} else {
    $wr.lang select $wr.lang.f
}

#-->bottom action buttons (separate procs for cluster means and trajs/cluster)
button $wr.bot.dismiss  -bg red -text Quit -width 10 -command "destroy $wr"
button $wr.bot.help -text Help -width 8 \
       -command "load_html [file join $html_dir S256.htm ] "
if {$Mean == 0 } {
   button $wr.bot.save  -bg green -text "Execute Display " -width 20 \
                                                    -command "run_means $ClusNo .clusdisp.lang"
   # set_default $Mean
  } else {
   button $wr.bot.save  -bg green -text "Execute Display " -width 20 \
                                                    -command "run_clusters $ClusNo .clusdisp.lang"
   # set_default $Mean
 }
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 -pady 4
set_default $Mean
read_Cdur
}

################################################################

proc run_means {ClusNo lang} {

# run trajplot and view

  global X_dir exec_dir tcl_dir Cluswork_dir Work_path Trajpts_file tcl_platform 
  global qpnt Kagl Zoom Color Label Psout_file 
  global Map_file Map_proj Igis Igif
  global Ring Map Ring_num Ring_dis Map_lat Map_lon
  global LabRef Lhrs
  global LABELS.CFG_file ListMeans_file
  global Nclus Run_ID
  global View_plot
  global home_dir
  global PyPreferred
  global PyDebug PySource_time_zone PyStreet_map PyOutput_format

  set PyPreferred [expr {[$lang select] == "${lang}.py"}]

  if { $PyPreferred } {
      set out_file ${Psout_file}.${PyOutput_format}
  } else {
      set out_file ${Psout_file}.html
  }

  if {! [file exists $tcl_dir] } {set tcl_dir ${home_dir}/guicode}

  if [file exists $out_file] {file delete $out_file}

  set Timel $Label
#  if {$qpnt == 0} {
#     if {$Label == 0} {set Timel -48} else {set Timel -$Label}
#  }
 
  # to plot symbols with respect to traj start date/time 
  #    (note variable Time-lower case L)
  if {$LabRef == 1} {
    set Timel -${Label}
  } 

# account for cluster#0 mean to be plotted
  if {$ClusNo == 0} {
    set Nmeans [expr ${Nclus}+1]
  } else {
    set Nmeans $Nclus
  }

  # itemize colors so can use 6 colors, otherwise defaults to 3 colors
  if {$Nmeans == 1}        { set arg5  -k1:1
  } elseif {$Nmeans == 2}  { set arg5  -k2:12
  } elseif {$Nmeans == 3}  { set arg5  -k3:123
  } elseif {$Nmeans == 4}  { set arg5  -k4:1234
  } elseif {$Nmeans == 5}  { set arg5  -k5:12345
  } elseif {$Nmeans == 6}  { set arg5  -k6:123456
  } elseif {$Nmeans == 7}  { set arg5  -k7:1234567
  } elseif {$Nmeans == 8}  { set arg5  -k8:12345671
  } elseif {$Nmeans == 9}  { set arg5  -k9:123456712
  } elseif {$Nmeans == 10} { set arg5 -k10:1234567123
  } elseif {$Nmeans == 11} { set arg5 -k11:12345671234
  } elseif {$Nmeans == 12} { set arg5 -k12:123456712345
  } elseif {$Nmeans == 13} { set arg5 -k13:1234567123456
  } elseif {$Nmeans == 14} { set arg5 -k14:12345671234567
  } elseif {$Nmeans == 15} { set arg5 -k15:123456712345671
  } elseif {$Nmeans == 16} { set arg5 -k16:1234567123456712
  } elseif {$Nmeans == 17} { set arg5 -k17:12345671234567123
  } elseif {$Nmeans == 18} { set arg5 -k18:123456712345671234
  } elseif {$Nmeans == 19} { set arg5 -k19:1234567123456712345
  } elseif {$Nmeans == 20} { set arg5 -k20:12345671234567123456
  } else                   { set arg5 -k1 
  } 

  set arg1 -i  ; append arg1 $Trajpts_file
  set arg2 -o  ; append arg2 $out_file
  set arg3 -j  ; append arg3 $Map_file
  set arg4 -m  ; append arg4 $Map_proj
  set arg6 -l  ; append arg6 ${Timel}
  set arg7 -z  ; append arg7 $Zoom
  set arg8 -v  ; append arg8 $Kagl
  set arg9 -a  ; append arg9 $Igis 

  set arga "-:"; set argb "-:"
  if { $Ring == 1 } {set arga -g  ; append arga $Ring_num:$Ring_dis}
  if { $Map  == 1 } {set argb -h  ; append argb $Map_lat:$Map_lon}

  set argc -e ; append argc $Lhrs

  set f [open "${LABELS.CFG_file}" w] 
  if { $ClusNo == 0 } {
  puts $f "'TITLE&','Cluster means, including mean for trajectories not clustered, $Run_ID&'"
  } else {
  puts $f "'TITLE&','Cluster means - $Run_ID&'"
  }
  close $f

  if { $PyPreferred } {
      set arg_ext " "
      if { $View_plot == 1 } { append arg_ext " --interactive" }
      if { $PyDebug } { append arg_ext " --debug" }
      #if { $PySource_time_zone } { append arg_ext " --source-time-zone" }

      switch -nocase $PyStreet_map {
          OPEN_STREET { append arg_ext " --street-map=0" }
          OPEN_TOPO { append arg_ext " --street-map=1" }
          default { }
      }

      run_python_program "$exec_dir/trajplot.py" "$arg_ext \
                        $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 \
                        $arg7 $arg8 $arg9 $arga $argb $argc"
  } else {
      set arg_ext "+g1"
      if { "$tcl_platform(platform)" == "unix" } {
          if { "$X_dir" == "" } {
              exec $exec_dir/trajplot $arg_ext \
                        $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 \
                        $arg7 $arg8 $arg9 $arga $argb $argc
          } else {
              exec $X_dir/xterm -fn fixed -e $exec_dir/trajplot $arg_ext \
                        $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 \
                        $arg7 $arg8 $arg9 $arga $argb $argc
          }
      } else {
              exec $exec_dir/trajplot.exe $arg_ext \
                        $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 \
                        $arg7 $arg8 $arg9 $arga $argb $argc
      }
  }

  if { $Igis == 1 } {
  # set cluster working directory to regular working directory so when run
  # Trajectory - Utility Programs - GIS to Shapefile, will write to cluster directory
     set Work_path $Cluswork_dir
  # rename file
     file rename -force GIS_traj_html_01.txt GIS_clusmeans${Nclus}-${ClusNo}_01.txt
     file rename -force GIS_traj_html_01.att GIS_clusmeans${Nclus}-${ClusNo}_01.att
  }

  ### houseclean ### delete, LABELS.CFG, TRAJ.INP.mean, TRAJ.INP.C
  file delete ${LABELS.CFG_file}
  file delete ${ListMeans_file}_${Nclus}

  ####################
  ### create GIF   ###
  ####################
  if { !$PyPreferred && $Igif == 1 } {
     source [file join $tcl_dir svg2gif.tcl]
     svg2gif ${Psout_file}
  }

  ####################
  ### view output  ###
  ####################
  if { !$PyPreferred && $View_plot == 1 } {
     svg_box $out_file
  } else {
     msg_box "         $out_file created         "
  }
}

################################################################

proc run_clusters {ClusNo lang} {

# run plotting program for trajectories in each cluster

  global X_dir exec_dir tcl_dir Work_path Cluswork_dir Trajpts_file tcl_platform
  global qpnt Kagl Zoom Color Label Psout_file 
  global Map_file Map_proj Igis Igif
  global Ring Map Ring_num Ring_dis Map_lat Map_lon
  global Nclus 
  global ListTrajs_file ClusListing_file
  global LabRef Lhrs
  global LABELS.CFG_file Run_ID
  global View_plot
  global home_dir
  global PyPreferred
  global PyDebug PySource_time_zone PyStreet_map PyOutput_format

  set PyPreferred [expr {[$lang select] == "${lang}.py"}]

  if { $PyPreferred } {
      set out_file ${Psout_file}.${PyOutput_format}
  } else {
      set out_file ${Psout_file}.html
  }

  if {! [file exists $tcl_dir] } {set tcl_dir ${home_dir}/guicode}

# force cluster #0 plot but do not concatenate it to others
  set ClusNo0 0

  ###################
  ### run clusmem ###
  ###################
  # create file listing tdump filenames for each cluster
  # output file is named TRAJ.INP.Cx_CN, x=cluster number, CN=number of clusters

  set clusmem_in ${ClusListing_file}_${Nclus}
  set clusmem_out ${ListTrajs_file}

 # if [file exists [glob ${clusmem_out}*_*]] {
 #     file delete [glob ${clusmem_out}*_*]
 # }

  for { set i $ClusNo0 } { $i <= $Nclus } {incr i} {
    if [file exists ${clusmem_out}$i] {file delete ${clusmem_out}$i}
  }

  if { "$tcl_platform(platform)" == "unix" } {
     if { "$X_dir" == "" } {
       exec $exec_dir/clusmem -i${clusmem_in} -o${clusmem_out}
     } else {
       exec $X_dir/xterm -fn fixed -e $exec_dir/clusmem \
                                             -i${clusmem_in} -o${clusmem_out} 
     }
  } else {
     exec $exec_dir/clusmem.exe -i${clusmem_in} -o${clusmem_out} 
  }

  #########################################################################
  ### set trajplot.py parameters that are independent of cluster number ###
  #########################################################################
  set Timel $Label
  if {$qpnt == 0} {
     if {$Label == 0} {set Timel -48} else {set Timel -$Label}
  }

 #set arg1 -i  ; append arg1 $Trajpts_file
 #set arg2 -o  ; append arg2 $out_file
  set arg3 -j  ; append arg3 $Map_file
  set arg4 -m  ; append arg4 $Map_proj
  set arg5 -k  ; append arg5 $Color
  set arg6 -l  ; append arg6 $Timel
  set arg7 -z  ; append arg7 $Zoom
  set arg8 -v  ; append arg8 $Kagl
  set arg9 -a  ; append arg9 $Igis 

  set arga "-:"; set argb "-:"
  if { $Ring == 1 } {set arga -g  ; append arga $Ring_num:$Ring_dis}
  if { $Map  == 1 } {set argb -h  ; append argb $Map_lat:$Map_lon}

  set argc -e ; append argc $Lhrs

  #####################################################################
  ### loop through all clusters to make one output file (C$i.tdump) ###
  #####################################################################

  if [file exists ${Psout_file}.html] {file delete ${Psout_file}.html}
  for { set i $ClusNo0 } { $i <= $Nclus } {incr i} {

#  diagnostic information window - click to continue
#  msg_box "cluster:${i} $ClusNo0 $Nclus"
#  tkwait window .msg_win

    ####################
    ### run merglist ###
    ####################
      # if [file exists ${ListTrajs_file}0_${Nclus}] {
      # } else {
      #    if {$i == 0} {
      #    msg_box "All trajectories used, so cannot include those not used"
      #    continue
      #    }
      # }

    if [file exists ${ListTrajs_file}${i}_${Nclus}] {
       if { "$tcl_platform(platform)" == "unix" } {
          if { "$X_dir" == "" } {
            exec $exec_dir/merglist -i+${ListTrajs_file}${i}_${Nclus} -oC${i}_${Nclus}
          } else {
            exec $X_dir/xterm -fn fixed -e $exec_dir/merglist \
                                    -i+${ListTrajs_file}${i}_${Nclus} -oC${i}_${Nclus}
          }
       } else {
          exec $exec_dir/merglist.exe -i+${ListTrajs_file}${i}_${Nclus} -oC${i}_${Nclus}
       }
    }

    #######################
    ### run trajplot.py ###
    #######################

    # input and output files
    set arg1 -i  ; append arg1 C${i}_${Nclus}.tdump
    set arg2 -o  ; append arg2 clusplot${i}_${Nclus}
    if [file exists clusplot${i}_${Nclus}.html] {file delete clusplot${i}_${Nclus}.html}

    # plot title
    set f [open "${LABELS.CFG_file}" w] 
    if { $i == 0 } {
       puts $f "'TITLE&','Trajectories not clustered - $Run_ID&'"
    } else {
       puts $f "'TITLE&','Cluster $i of $Nclus - $Run_ID&'"
    }
    close $f

  if [file exists C${i}_${Nclus}.tdump] {
    if { $PyPreferred } {
      set arg_ext " "
      if { $View_plot == 1 } { append arg_ext " --interactive" }
      if { $PyDebug } { append arg_ext " --debug" }
      #if { $PySource_time_zone } { append arg_ext " --source-time-zone" }
      # Looping requires the main output format be Postscript (or ps).
      # Specify an additional output format that the user selected.
      append arg_ext " --more-formats=$PyOutput_format"

      switch -nocase $PyStreet_map {
          OPEN_STREET { append arg_ext " --street-map=0" }
          OPEN_TOPO { append arg_ext " --street-map=1" }
          default { }
      }

      run_python_program "$exec_dir/trajplot.py" "$arg_ext \
                        $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 \
                        $arg7 $arg8 $arg9 $arga $argb $argc"
    } else {
      set arg_ext "+g1"
      if { "$tcl_platform(platform)" == "unix" } {
          if { "$X_dir" == "" } {
              exec $exec_dir/trajplot $arg_ext \
                        $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 \
                        $arg7 $arg8 $arg9 $arga $argb $argc
          } else {
              exec $X_dir/xterm -fn fixed -e $exec_dir/trajplot $arg_ext \
                        $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 \
                        $arg7 $arg8 $arg9 $arga $argb $argc
          }
      } else {
              exec $exec_dir/trajplot.exe $arg_ext \
                        $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 \
                        $arg7 $arg8 $arg9 $arga $argb $argc
      }
    }
  
    # save this cluster's GIS files
    if { $Igis == 1 } {
       # set cluster working directory to regular working directory so when run
       # Trajectory - Utility Programs - GIS to Shapefile, will write to cluster directory
         set Work_path $Cluswork_dir
       if { $i < 10 } {
         file rename -force GIS_traj_html_01.att GIS_cluster${Nclus}_0${i}.att
         file rename -force GIS_traj_html_01.txt GIS_cluster${Nclus}_0${i}.txt
       } else {
         file rename -force GIS_traj_html_01.att GIS_cluster${Nclus}_${i}.att
         file rename -force GIS_traj_html_01.txt GIS_cluster${Nclus}_${i}.txt
       }
    }
  }

  # concatenate plots				
  if {$i > 0} {
     if [file exists clusplot${i}_${Nclus}.html] {
        if { $i == $ClusNo } {
           file copy -force clusplot${i}_${Nclus}.html ${Psout_file}.html
        } else {
           if { "$tcl_platform(platform)" == "unix" } {
              exec cat clusplot${i}_${Nclus}.html >> ${Psout_file}.html
           } else {
              plot_append clusplot${i}_${Nclus}.html ${Psout_file}.html
           }
        }
     }
   }

  }

  ###########################################
  # convert to pseudo non-concatenated file
  ###########################################
  if [file exists ${Psout_file}.html] {
  file copy -force ${Psout_file}.html tempfile.html
  set arg1 -itempfile.html
  set arg2 -o  ; append arg2 ${Psout_file}.html
  set arg3 -t  ; append arg3 1

  if { "$tcl_platform(platform)" == "unix" } {
     if { "$X_dir" == "" } {
       exec $exec_dir/cat2svg $arg1 $arg2 $arg3
  	
     } else {
       exec $X_dir/xterm -fn fixed -e $exec_dir/cat2svg $arg1 $arg2 $arg3
     }
   } else {
   exec $exec_dir/cat2svg.exe $arg1 $arg2 $arg3
   }

   file delete tempfile.html

       ### simple dump (save for reference) ##################
         #set g [open dump.txt w]
         #puts $g $arg1
         #puts $g $arg2
         #puts $g $arg3
         #puts $g $exec_dir
         #close $g
       ### end dump ##########################################
   

# set cluster working directory to regular working directory so when run
# Trajectory - Utility Programs - GIS to Shapefile, will write to cluster directory
  if { $Igis == 1 } {
     set Work_path $Cluswork_dir
  }

### houseclean, delete LABELS.CFG, TRAJ.INP.C ***
  file delete ${LABELS.CFG_file}   		
  for { set i 0 } { $i <= $Nclus } {incr i} {
    if [file exists ${ListTrajs_file}${i}_${Nclus}] {
       file delete ${ListTrajs_file}${i}_${Nclus}
    }
  } 

  ####################
  ### view output  ###
  ####################

  if { !$PyPreferred && $View_plot == 1 } {
     svg_box $out_file
  } else {
    msg_box "         $out_file created         "
  }

  ####################
  ### create GIF, need to check 'frames' button convert window   
  ####################
  if { !$PyPreferred && $Igif == 1 } {
     source [file join $tcl_dir svg2gif.tcl]
     svg2gif ${Psout_file}
  }
  }

}

################################################################
#-->set display defaults

proc set_default {Mean} {

  global qpnt Kagl Zoom Color Label Psout_file 
  global home_dir maps_dir Map_file Map_proj Igis Igif
  global Ring Map Ring_num Ring_dis Map_lat Map_lon
  global Start_loc1
  global LabRef
  global View_plot
  global PyDebug PySource_time_zone PyStreet_map PyOutput_format

  set Igif 0
  set View_plot 1

  if [ info exists LabRef ] { } else {set LabRef ""}
  if { $LabRef == "" } {
     if {! [file exists $maps_dir] } {set maps_dir ${home_dir}/graphics}
     set Map_file ${maps_dir}/arlmap 
     set Map_proj 0
     set Label 12
     set Zoom 80
     set Color 1
     set Kagl 4
     set qpnt 1
     set Igis 0
     set Ring 0
     set Map 0
     set Ring_num 0 
     set Ring_dis 1000
     set LabRef 1
     set PyDebug false
     set PySource_time_zone false
     set PyStreet_map NOT_USED
     set PyOutput_format pdf
   }

   if {$Mean == 0} {
      # cluster-means
      set Label 12
      set Color 1
   } else {
      # trajs in each cluster
      set Label 0
      set Color 0
   }

}

#--------------------------------

proc read_Cdur {} {

  global Lhrs

  if [file exists CLUSTER] {
    set f [open CLUSTER r]
    gets $f tmp
    set Lhrs [lindex $tmp 0]
    close $f
  }

}

#----------------------------------

proc plot_append {file1 file2} {

   set file1id [open $file1 r]
   set file2id [open $file2 a]
   while {[eof $file1id] != 1} {
      gets $file1id  cline
      if { "$cline" == "" } { } else {puts $file2id $cline}
   }
   close $file1id
   close $file2id
}


