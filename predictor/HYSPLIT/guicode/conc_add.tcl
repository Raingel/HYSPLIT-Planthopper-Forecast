proc conc_add {} {

#-----------------------------------------------------------------------------
# CONC_ADD.TCL: add together binary concentration files  
# Last Revised: 23 Sep 2009 - initial version
#               14 Dec 2009 - added intersection option
#               18 Jun 2013 - enhanced help information
#               29 Jul 2013 - glob nocomplain
#               14 Sep 2016 - corrected browse for shapefiles.txt
#               07 Nov 2016 - do not permit shapefile copy to itself
#               24 Jan 2017 - added -t for time label max-min
#               13 Dec 2019 - replace concplot and concplot.exe with concplot.py
#               16 Jan 2020 - add option to use FORTRAN or Python trajplot.
#               12 May 2020 - add vertical scrollbar.
#               02 Sep 2020 - rename Fscale to Cscale for consistency
#               26 Jul 2021 - use svg instead of postscript
#               04 Apr 2022 - add -81 option to concplot to create plot(s)
#                             even when all values are zero.
#               07 Nov 2023 - change street maps to OPEN_STREET and OPEN_TOPO.
#-----------------------------------------------------------------------------

global html_dir
global Cinp_file Cbase_file Cout_file Cscale Process Mask Tsum
global Psout_file Frame Igis View
global Map_file Color Map_proj Zoom 
global PyDebug PySource_time_zone PyStreet_map PyOutput_format
global PyPreferred

if [winfo exists .concadd] {destroy .concadd}
set topwin .concadd
toplevel $topwin
wm title $topwin " Merge Multiple Binary Concentration Files "
wm  geometry $topwin +100+25

# create a scrollable frame (wr).
frame $topwin.outerframe
canvas $topwin.outerframe.canvas -yscrollcommand "$topwin.outerframe.yscroll set"
scrollbar $topwin.outerframe.yscroll -command "$topwin.outerframe.canvas yview"
frame $topwin.outerframe.canvas.innerframe
set wr $topwin.outerframe.canvas.innerframe

frame $wr.top
frame $wr.mid0
frame $wr.mid2
frame $wr.mid8
frame $wr.mid3
frame $wr.mid1
frame $wr.mid4
frame $wr.mid5
frame $wr.mid6
frame $wr.mid7
frame $wr.bot

ttk::notebook $wr.lang
frame $wr.lang.py
frame $wr.lang.f
$wr.lang add $wr.lang.f -text "Fortran"
$wr.lang add $wr.lang.py -text "Python"

pack $wr.top -pady 4
pack $wr.mid0 $wr.mid2 $wr.mid8 $wr.mid3 -pady 2
pack $wr.mid1 -pady 5
pack $wr.mid4 $wr.mid5 $wr.mid6 -pady 2  
pack $wr.mid7 $wr.lang $wr.bot -pady 6
pack configure $wr.lang -expand true -fill x -padx 10

#-->description

label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "Merges two or more binary concentration files, where\
 the input file is merged with the base file and written to the output file.\
 The input file does not need not be identical in time with the base file,\
 but it must have the same number of pollutants and levels. The horizontal\
 grids need to be identical or even multiples of each other. Merge options\
 are add, maximum, mask, or intersect. The input file can be used as a mask\
 file such that grid points exceeding the mask value become zero in the base\
 file. The intersect option is a special version of masking such that values\
 are added only when when both grids exceed the mask. When the input is\
 defined by a file of filenames (INFILE) using the input file name as a\
 wildcard, then the files are processed sequentially: file n is merged into\
 n+1 according the merge rules."

pack $wr.top.lab

#-->input file or file of file names using input file name as wildcard

label  $wr.mid0.lab1 -text " Input Name:"
entry  $wr.mid0.ent1 -textvariable Cinp_file -relief sunken -width 12
label  $wr.mid0.lab2 -text "  Base name:"
entry  $wr.mid0.ent2 -textvariable Cbase_file -relief sunken -width 12
button $wr.mid0.mak -text "Create filenames" -width 18 -bg yellow -command Make_cadd
pack   $wr.mid0.lab1 $wr.mid0.ent1 $wr.mid0.lab2 $wr.mid0.ent2 $wr.mid0.mak  -side left -padx 4

#-->merge processing

label $wr.mid2.lab -text "Merge Processing:" 
radiobutton $wr.mid2.d0 -text "Sum " -variable Process -value "0" 
radiobutton $wr.mid2.d1 -text "Max " -variable Process -value "1" 
radiobutton $wr.mid2.d2 -text "Mask" -variable Process -value "2" 
radiobutton $wr.mid2.d3 -text "Intersect" -variable Process -value "3" 
label $wr.mid2.txt -text " Mask:"
entry $wr.mid2.ent -textvariable Mask -relief sunken -width 10
pack $wr.mid2.lab -side left -padx 10
pack $wr.mid2.d0 $wr.mid2.d1 $wr.mid2.d2 $wr.mid2.d3 -side left
pack $wr.mid2.txt $wr.mid2.ent -side left -padx 5

#-->time processing
checkbutton $wr.mid8.box -variable Tsum -text "Set sampling time labels as min-max" -background grey85
pack $wr.mid8.box -side left -padx 5

#-->output file name

label $wr.mid3.lab1 -text "Output name:"
entry $wr.mid3.ent1 -textvariable Cout_file -relief sunken -width 12
label $wr.mid3.lab2 -text "   Multiplier:"
entry $wr.mid3.ent2 -textvariable Cscale -relief sunken -width 12
button $wr.mid3.mak  -text "Process Files" -width 15 -bg green -command Run_cadd
pack  $wr.mid3.lab1 $wr.mid3.ent1 $wr.mid3.lab2 $wr.mid3.ent2 $wr.mid3.mak -side left -padx 4 

#-->section partition

label $wr.mid1.lab -text "_________________________________________________________"
pack  $wr.mid1.lab 

#-->output postscript file

label $wr.mid4.lab -text "Graphics File: "
entry $wr.mid4.ent -textvariable Psout_file -relief sunken -width 12
checkbutton $wr.mid4.box -variable Frame -text "Frames" -background grey
checkbutton $wr.mid4.gis -variable Igis -text "GIS" -background grey
checkbutton $wr.mid4.psv -variable View -text "View" -background grey
checkbutton $wr.mid4.kol -variable Color -text Color -background grey
pack $wr.mid4.lab $wr.mid4.ent $wr.mid4.box $wr.mid4.gis $wr.mid4.psv \
     $wr.mid4.kol -side left -padx 5 

#-->map background file

label $wr.mid5.lab -text "Map Background: "
entry $wr.mid5.ent -textvariable Map_file -relief sunken -width 35
button $wr.mid5.win  -text Browse -width 8 -command {
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
pack $wr.mid5.lab $wr.mid5.ent $wr.mid5.win -side left -padx 5

#-->map projection options

label $wr.mid6.lab -text "   Projection: "
radiobutton $wr.mid6.d0 -text "Auto"     -variable Map_proj -value "0" 
radiobutton $wr.mid6.d1 -text "Polar"    -variable Map_proj -value "1" 
radiobutton $wr.mid6.d2 -text "Lambert"  -variable Map_proj -value "2" 
radiobutton $wr.mid6.d3 -text "Mercator" -variable Map_proj -value "3" 
pack $wr.mid6.lab -side left -padx 10
pack $wr.mid6.d0 $wr.mid6.d1 $wr.mid6.d2 $wr.mid6.d3 -side left

#-->zoom factor slider bar

label $wr.mid7.lab -text "Least Zoom -------------------> Most Zoom"
scale $wr.mid7.d0 -orient horizontal -length 400 -from 0 -to 100 \
  -tickinterval 10 -variable Zoom -resolution 5 
pack $wr.mid7.lab $wr.mid7.d0 -side top -pady 3

#-->Python options
label $wr.lang.f.lab -text "Output will be in SVG"
pack $wr.lang.f.lab -expand true

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
checkbutton $wr.lang.py.misc.source_tz -variable PySource_time_zone -text "Source time zone" -background grey
checkbutton $wr.lang.py.misc.debug -variable PyDebug -text "Debug messages" -background grey
pack $wr.lang.py.misc.source_tz $wr.lang.py.misc.debug -side left -padx 5

pack $wr.lang.py.row1 $wr.lang.py.misc -side top -pady 5 -padx 10

if { $PyPreferred } {
    $wr.lang select $wr.lang.py
} else {
    $wr.lang select $wr.lang.f
}

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 12 -command "destroy $topwin"
button $wr.bot.help -text Help -width 8 \
       -command "load_html [file join $html_dir S344.htm ] "
button $wr.bot.save  -bg green -text " Plot Results " -width 12 -command {Plot_cadd ".concadd.outerframe.canvas.innerframe.lang"}
pack $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 

# scrollable frame business
pack $topwin.outerframe.canvas.innerframe -expand yes -fill both -side top
pack $topwin.outerframe.yscroll -side right -fill y
$topwin.outerframe.canvas create window 0 0 -anchor nw -window $topwin.outerframe.canvas.innerframe
$topwin.outerframe.canvas configure -scrollregion [$topwin.outerframe.canvas bbox all]
pack $topwin.outerframe.canvas -expand yes -fill both -side top
pack $topwin.outerframe -expand yes -fill both -side top
bind $topwin.outerframe <Map> {
   set topwin .concadd
   # assume only 80% of the screen height is available.
   set content_height [winfo height $topwin.outerframe.canvas.innerframe]
   set avail_scr_height [expr 0.80 * [winfo screenheight .]]
   if {$content_height >= $avail_scr_height} {
      set view_height $avail_scr_height
   } else {
      set view_height $content_height
   }
   $topwin.outerframe.canvas.innerframe configure -height $content_height
   $topwin.outerframe.canvas configure -width 690 -height $view_height
   $topwin.outerframe.canvas configure -scrollregion [$topwin.outerframe.canvas bbox all]
}

Init_cadd
}

#------------------------------------------------------------------------------
# run plotting program

proc Run_cadd {} {

global Cout_file

if [file exists $Cout_file] {file delete $Cout_file}
if [file exists INFILE] {

#  multiple input files
   Loop_cadd

} else {
#  single input file
   Merge_cadd

}

}

#------------------------------------------------------------------------------
# process one set of files INFILE does not exist

proc Merge_cadd {} {

global Cinp_file Cbase_file Cout_file Cscale Process Mask Tsum
global X_dir tcl_platform exec_dir

set arg1 -i  ; append arg1 $Cinp_file
set arg2 -b  ; append arg2 $Cbase_file
set arg3 -o  ; append arg3 $Cout_file
set arg4 -c  ; append arg4 $Cscale
set arg5 -p  ; append arg5 $Process
set arg6 -z  ; append arg6 $Mask

if { $Tsum == 0 } {
   set arg7 "-:" 
} else {
   set arg7 -t 
} 

if {[file exists $Cinp_file] && [file exists $Cbase_file]} {

   if { "$tcl_platform(platform)" == "unix" } {
      if { "$X_dir" == "" } {
      exec $exec_dir/concadd $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7
      } else {
      exec $X_dir/xterm -fn fixed -e $exec_dir/concadd $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7
      }
   } else {
      exec $exec_dir/concadd.exe $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7
   }

} else {
   msg_box "Input files $Cinp_file or $Cbase_file not found!"
}
}


#------------------------------------------------------------------------------
# process multiple files through INFILE

proc Loop_cadd {} {

global Cinp_file Cbase_file Cout_file Cscale Process Mask Tsum
global X_dir tcl_platform exec_dir

set arg5 -p  ; append arg5 $Process

if { $Tsum == 0 } {
   set arg7 "-:" 
} else {
   set arg7 -t 
} 

if [file exists $Cinp_file] {

   set f [open $Cinp_file r]
   set nline temp_sum1
#  load file "n"
   gets $f pline  

#  load file "n+1"
   while {[gets $f cline] >= 0} {

      set arg1 -i ; append arg1 $pline
      set arg2 -b ; append arg2 $cline
      set arg3 -o ; append arg3 $nline

      if { "$tcl_platform(platform)" == "unix" } {
         if { "$X_dir" == "" } {
            exec $exec_dir/concadd $arg1 $arg2 $arg3 $arg5 $arg7
         } else {
            exec $X_dir/xterm -fn fixed -e $exec_dir/concadd $arg1 $arg2 $arg3 $arg5 $arg7
         }
      } else {
         exec $exec_dir/concadd.exe $arg1 $arg2 $arg3 $arg5 $arg7
      }

#     file "n" now contains the merged contents
      set pline $nline
      if { "$pline" == "temp_sum1" } {
         set nline temp_sum2
      } else {
         set nline temp_sum1
      }
  
   }
   close $f


#  with multiple files only apply conversion factor when processing complete
#  and use program conlight rather than concadd

   if { "$Cscale" == "1.0" } {
      if [file exists $pline] {file copy -force $pline $Cout_file}

   } else {
      set arg1 -i ; append arg1 $pline
      set arg2 -m ; append arg2 $Cscale
      set arg3 -o ; append arg3 $Cout_file

      if { "$tcl_platform(platform)" == "unix" } {
         if { "$X_dir" == "" } {
            exec $exec_dir/conlight $arg1 $arg2 $arg3
         } else {
            exec $X_dir/xterm -fn fixed -e $exec_dir/conlight $arg1 $arg2 $arg3
         }
      } else {
         exec $exec_dir/conlight.exe $arg1 $arg2 $arg3
      }
   }

} else { 
   msg_box "Input file $Cinp_file not found!"
}

## set Cinp_file ""
if [file exists temp_sum1] {file delete temp_sum1}
if [file exists temp_sum2] {file delete temp_sum2}
}

#------------------------------------------------------------------------------
# plot results

proc Plot_cadd {lang} {

global X_dir tcl_platform exec_dir 
global Cout_file Psout_file Frame Igis View
global Map_file Color Map_proj Zoom 
global Process Mask
global PyPreferred
global PyDebug PySource_time_zone PyStreet_map PyOutput_format

set PyPreferred [expr {[$lang select] == "${lang}.py"}]

if { $PyPreferred } {
    set out_file ${Psout_file}.${PyOutput_format}
} else {
    set out_file ${Psout_file}.html
}

if [file exists $Cout_file] {

   if [file exists $out_file] {file delete $out_file}

   set f [open "LABELS.CFG" w]
   if {$Process == 0} {puts $f "'TITLE&','Concentration Summation&'"}
   if {$Process == 1} {puts $f "'TITLE&','Concentration Maximums&'"}
   if {$Process == 2} {puts $f "'TITLE&','Concentration Masked >= $Mask&'"}
   if {$Process == 3} {puts $f "'TITLE&','Concentration Intersection >= $Mask&'"}
   close $f

   set arg1 -i  ; append arg1 $Cout_file
   set arg2 -o  ; append arg2 $out_file
   set arg3 -j  ; append arg3 $Map_file
   set arg4 -m  ; append arg4 $Map_proj
   set arg5 -k  ; append arg5 $Color
   set arg6 -z  ; append arg6 $Zoom
   set arg7 -a  ; append arg7 $Igis

   if { $PyPreferred } {
       set arg_ext " "
       set arg_ext2 "-81"
       if { $View == 1 } { append arg_ext " --interactive" }
       if { $PyDebug } { append arg_ext " --debug" }
       if { $PySource_time_zone } { append arg_ext " --source-time-zone" }

       switch -nocase $PyStreet_map {
           OPEN_STREET { append arg_ext " --street-map=0" }
           OPEN_TOPO { append arg_ext " --street-map=1" }
           default { }
       }

       run_python_program "$exec_dir/concplot.py" "$arg_ext $arg_ext2 \
                         $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7"
   } else {
       set arg_ext "+g1"
       set arg_ext2 "-81"
       if { "$tcl_platform(platform)" == "unix" } {
           if { "$X_dir" == "" } {
               exec $exec_dir/concplot $arg_ext $arg_ext2 $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 
           } else {
               exec $X_dir/xterm -fn fixed -e $exec_dir/concplot $arg_ext $arg_ext2 $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7
           }          
       } else {
               exec $exec_dir/concplot.exe $arg_ext $arg_ext2 $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7
       }
   }

## if [file exists INFILE] {file delete INFILE}
   if [file exists LABELS.CFG] {file delete -force LABELS.CFG}
   if { !$PyPreferred && $View == 1 } {svg_box ${Psout_file}.html} 

} else {
  msg_box "Process file $Cout_file not found!"
}
}


#------------------------------------------------------------------------------
# set initial defaults

proc Init_cadd {} {

  global Cscale Process Mask
  global Cinp_file Cbase_file Cout_file
  global Psout_file Frame Igis View Tsum
  global maps_dir Map_file Color Map_proj Zoom 
  global PyDebug PySource_time_zone PyStreet_map PyOutput_format

  set Igis 0
  set View 1
  set Psout_file concplot

  if [ info exists Cinp_file ] { } else {
    set Cinp_file ""
  }

  if { $Cinp_file == "" } {
     if [file exists INFILE] {file delete INFILE}

     set Map_file ${maps_dir}/arlmap 
     set Map_proj 0
     set Zoom 50
     set Color 1
     set Frame 0

     set Cinp_file cdump
     set Cbase_file gdump
     set Cout_file concadd.bin
     if {"$Cscale" == ""} {set Cscale 1.0}
     set Process 0
     set Mask 0.0
     set Tsum 0

     set PyDebug false
     set PySource_time_zone false
     set PyStreet_map NOT_USED
     set PyOutput_format pdf
  }
}

#------------------------------------------------------------------------------
# create file listing (INFILE) using default file name as wildcard

proc Make_cadd {} {

  global Cinp_file Cbase_file

  if {"$Cinp_file" == ""} {
     msg_box " ERROR: Need wildcard in Input Name field! "
  } else {
     if [file exists INFILE] {file delete INFILE}
     set g [open INFILE w]
     foreach f [glob -nocomplain *${Cinp_file}*] {puts $g $f}
     close $g

     if {[file size INFILE] == 0} {
        msg_box " Contents of ${Cinp_file} empty! "
     } else {
        set Cinp_file INFILE
        set Cbase_file ""
        msg_box " File of file names created (${Cinp_file}) "
     }
  }
}
