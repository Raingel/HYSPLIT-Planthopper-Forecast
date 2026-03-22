proc asc2shp {Line_type} {

#-----------------------------------------------------------------------------
# ASC2SHP.TCL : Convert ASCII generate format to shape files      
# Last Revised: 30 Sep 2002 - initial version
#               17 Jun 2003 - shape directory
#               26 Jan 2005 - real option (-d) on ascii2shp conversion
#               27 May 2005 - option to set conversion type
#               03 Nov 2008 - simplified directory
#               01 Dec 2008 - attributes conversion
#               10 Feb 2011 - note about added projection file
#-----------------------------------------------------------------------------

global html_dir Work_path Grids_path Shape_file Shape_type Shape_att
if [winfo exists .ascshp] {destroy .ascshp}
set wr .ascshp
toplevel $wr
wm title $wr " Convert ASCII Generate format to Shapefile "
wm  geometry $wr +50+15

frame $wr.top
frame $wr.data
frame $wr.mid1
frame $wr.mid2
frame $wr.mid3
frame $wr.mid4
frame $wr.mid5
frame $wr.bot

pack $wr.top -pady 4 -padx 2 
pack $wr.data -pady 2
pack $wr.mid1 $wr.mid2 -side top -pady 2 -anchor w
pack $wr.mid3 $wr.mid4 $wr.mid5 -side top -pady 4 
pack $wr.bot -side top -pady 6 -padx 10

shp_set $Line_type

#-->information

label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "Convert ASCII text files of contours (Generate format) to ESRI\
 shapefiles with extensions .shp, .shx, .dbf, .prj.  The input generate\
 text file is created from the GIS option in selected display programs."
pack $wr.top.lab

#-->base input

frame $wr.data.lab
frame $wr.data.pick
pack $wr.data.lab $wr.data.pick -pady 2 -side top
button $wr.data.lab.set  -text "Name of input generate text file (GIS_???.txt)" \
       -width 55 -command {
   .ascshp.data.pick.src.list delete 0 end
   .ascshp.data.pick.dir.list delete 0 end
   do_file_list .ascshp.data.pick.src.list .ascshp.data.pick.dir.list 
   }
pack  $wr.data.lab.set -side left -padx 15

frame $wr.data.pick.dir
frame $wr.data.pick.src
pack $wr.data.pick.dir $wr.data.pick.src -side left -padx 5
listbox $wr.data.pick.dir.list -relief sunken -width 35 -height 1 -setgrid yes -bg LightCyan3
pack $wr.data.pick.dir.list -fill both -expand yes 
listbox $wr.data.pick.src.list -relief sunken -width 20 -height 1 -setgrid yes -bg LightCyan3
pack $wr.data.pick.src.list -fill both -expand yes

#-->set the conversion option type

label $wr.mid3.lab -text "Conversion method: "
radiobutton $wr.mid3.d1 -text "Points" -variable Shape_type -value "points" 
radiobutton $wr.mid3.d2 -text "Lines" -variable Shape_type -value "lines" 
radiobutton $wr.mid3.d3 -text "Polygons" -variable Shape_type -value "polygons" 
pack $wr.mid3.lab $wr.mid3.d1 $wr.mid3.d2 $wr.mid3.d3 -side left -padx 2

#-->set the conversion attributes

label $wr.mid4.lab -text "Enhanced attributes (dbf): "
radiobutton $wr.mid4.d1 -text "Trajectory" -variable Shape_att -value "traj" 
radiobutton $wr.mid4.d2 -text "Concentration" -variable Shape_att -value "conc" 
radiobutton $wr.mid4.d3 -text "None" -variable Shape_att -value "none" 
pack $wr.mid4.lab $wr.mid4.d1 $wr.mid4.d2 $wr.mid4.d3 -side left -padx 2

#-->output file name base

label $wr.mid5.lab2 -text "Output Shapefiles Base Name (base.shp .shx .dbf .prj)"
entry $wr.mid5.ent2 -textvariable Shape_file -relief sunken -width 50
pack $wr.mid5.lab2 $wr.mid5.ent2 

#-->bottom action buttons

button $wr.bot.dismiss  -bg red -text Quit -width 8 -command "destroy $wr"
button $wr.bot.help -text Help -width 8 \
       -command "load_html [file join $html_dir S135.htm ] "
button $wr.bot.save  -bg green -text "Process Data" -width 20 -command {
       set gdir  [.ascshp.data.pick.dir.list get 0 0]
       set gbase [.ascshp.data.pick.src.list get 0 0]
       gis_data $gdir $gbase
       }
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 
}

#-------------------------------------------------------------
proc shp_set {Line_type} {
global Shape_file Shape_type Shape_att

set Shape_type $Line_type

if { [ catch {set dummy $Shape_att} ] } { 
   set Shape_att none
#  switch $Shape_type {
#     points   {set Shape_att traj}
#     lines    {set Shape_att none}
#     polygons {set Shape_att conc}
#     }
   }
}

#-------------------------------------------------------------
proc gis_data {gdir gbase} {
global result tcl_platform Shape_file Shape_type Shape_att exec_dir

set tmp_file [file rootname $gbase]
set txt_file [file join $gdir $tmp_file]
set att_file [file join $gdir $tmp_file]
append txt_file .txt
append att_file .att

# conversion type options: points lines polygons

if { "$tcl_platform(platform)" == "unix" } {
     exec $exec_dir/ascii2shp -d $Shape_file $Shape_type <$txt_file
   } else {
     exec $exec_dir/ascii2shp.exe -d $Shape_file $Shape_type <$txt_file
   }

if {"$Shape_att" == "none" } {
   msg_box " Created shape files: $Shape_file for $Shape_type "  
   tkwait window .msg_win
   if { $result == 1 } {destroy .ascshp}
}

# optional enhanced attributes file

switch $Shape_att {
   traj {
         set arg1 -C7 
         set arg2 -C9
         set arg3 -C5 
         set arg4 -C9
         set arg5 -d,
         set arg6 -d,}
   conc {
         set arg1 -C11
         set arg2 -C5
         set arg3 -C9
         set arg4 -C5
         set arg5 -C6
         set arg6 -C6}
   none {return}
   }

set arg7 -d, 
set arg8 $att_file
set arg9 ${Shape_file}.dbf

if { "$tcl_platform(platform)" == "unix" } {
     exec $exec_dir/txt2dbf $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8 $arg9
   } else {
     exec $exec_dir/txt2dbf.exe $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8 $arg9
   }

msg_box " Created shape files: $Shape_file for $Shape_type "  
tkwait window .msg_win
if { $result == 1 } {destroy .ascshp}
}
