proc psc2gif {base_file} {

#------------------------------------------------------------------------------
# PSC2GIF.TCL: graphics conversion script
# Last Revised:    11 Mar 2002
#                  13 Aug 2002
#                  16 May 2003 - animation
#                  22 Sep 2004 - removed crop option from convert
#                  18 Jan 2005 - PowerPc Darwin compatibility
#                  21 Sep 2005 - multiframe fix
#                  16 May 2006 - resolution slider bar
#                  14 Dec 2006 - replace crop with trim
#                  21 May 2007 - renamed from call_magick
#                  18 Oct 2007 - changed graphics bit density
#                  23 Jun 2009 - +page to +repage
#                  22 Jul 2013 - -dispose Background for animations
#                  12 Aug 2015 - -quiet added to suppress warning messages
#                  28 Jul 2017 - defined psc_init for initialization
#                  01 Sep 2020 - added option "-background white -flatten"
#                  12 Oct 2020 - removed -flatten
#------------------------------------------------------------------------------

global Input_file Output_file Loop Crop Mult Density
global html_dir

if [winfo exists .magick] {destroy .magick}
set wr .magick
toplevel $wr
wm title $wr " Postscript Conversion  "
wm  geometry $wr +100+100

frame $wr.top
frame $wr.mid1
frame $wr.mid2
frame $wr.mid3
frame $wr.bot
pack $wr.top $wr.mid1 $wr.mid2 $wr.mid3 $wr.bot -side top -pady 10 -padx 10

#-->description
label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i -text \
"Converts the input file graphic format to the output file format as\
specified by the file name suffix. In files with multiple frames, each\
graphic frame may be written to a new file (frames button) where the\
file name is preceeded by the frame number. Multiple frames may also\
be combined into an animated graphic."
pack $wr.top.lab

#-->input file
label $wr.mid1.lab -text "Postscript Input File: "
entry $wr.mid1.ent -textvariable Input_file -relief sunken -width 15
pack $wr.mid1.lab $wr.mid1.ent -side top

#-->output file
label $wr.mid2.lab -text "Graphics Output File: "
entry $wr.mid2.ent -textvariable Output_file -relief sunken -width 15
checkbutton $wr.mid2.ent2 -variable Loop -text "Animate" -background grey
checkbutton $wr.mid2.ent3 -variable Crop -text "Crop" -background grey
checkbutton $wr.mid2.ent4 -variable Mult -text "Frames" -background grey
pack $wr.mid2.lab 
pack $wr.mid2.ent $wr.mid2.ent2 $wr.mid2.ent3 $wr.mid2.ent4 -side left

#-->output resolution                   
label $wr.mid3.lab -text "Output Resolution (pixels/inch)"
scale $wr.mid3.den -orient horizontal -length 500  -from 100 -to 300 \
  -tickinterval 25 -variable Density -resolution 5
pack $wr.mid3.lab $wr.mid3.den -side top 

#-->bottom action buttons
button $wr.bot.dismiss  -bg red -text Quit -width 10 -command "destroy $wr"
button $wr.bot.help -text Help -width 8 \
       -command "load_html [file join $html_dir S134.htm ] "

button $wr.bot.save  -bg green -text "Execute Conversion " -width 20 -command {run_magick}
pack  $wr.bot.dismiss $wr.bot.help $wr.bot.save -side left -padx 10 
set_defaultm $base_file
}

#-------------------------------------------------------------------------
# run conversion program

proc run_magick {} {

global Input_file Output_file Loop Crop Mult Density
global magick_pgm 

if [file exists $Input_file] {
} else {
   msg_box "File not found: $Input_file"
   destroy .magick
}

if [file exists $magick_pgm] {
} else {
   msg_box "ImageMagick Required: $magick_pgm"
   destroy .magick
}

#  Optional section if ImageMagick installed without proper link to ghostscript.
#  Installations without proper delegates file to convert PS requires the
#  following intermediate step, otherwise convert can be used directly.
 
#  if [file exists $gsc_pgm] {
#  } else {
#     msg_box "Ghostscript Required: $gsc_pgm"
#     destroy .magick
#  }
#  exec $gsc_pgm -dNOPAUSE -dSAFER -sDEVICE=pnmraw -q -sOutputFile=- \
#  $Input_file -c quit | $magick_pgm - $Output_file

if { $Loop == 1 } {

   if { $Crop == 1 } {
      exec $magick_pgm -quiet -density $Density -loop 100 -delay 30 -trim +repage -dispose Background -background white $Input_file $Output_file
   } else {
      exec $magick_pgm -quiet -density $Density -loop 100 -delay 30 -dispose Background -background white $Input_file $Output_file
   } 

} else {

   if { $Crop == 1 } {
      if { $Mult == 1 } {
        foreach f [glob -nocomplain F*-${Output_file}] {file delete $f}
        exec $magick_pgm -quiet -density $Density -trim +repage -adjoin -background white $Input_file F%02d-$Output_file
      } else {
        exec $magick_pgm -quiet -density $Density -trim +repage -background white $Input_file $Output_file
      }
   } else {
      if { $Mult == 1 } {
        foreach f [glob -nocomplain F*-${Output_file}] {file delete $f}
        exec $magick_pgm -quiet -density $Density -adjoin -background white $Input_file F%02d-$Output_file
      } else {
        exec $magick_pgm -quiet -density $Density -background white $Input_file $Output_file
      }
   } 

}
destroy .magick
}

#-------------------------------------------------------------------------------
# set defaults

proc set_defaultm {base_file} {

global Input_file Output_file Loop Crop Mult Density psc_init

if [ info exists psc_init ] { } else {set psc_init ""}
if { $psc_init == "" } {
   set psc_init "yes"
   set Density "100"
   set Loop 0
   set Crop 0
   set Mult 0
   }
set Input_file  ${base_file}.ps
set Output_file ${base_file}.gif

}
