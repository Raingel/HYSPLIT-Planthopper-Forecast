proc disp_txt { } {

#-----------------------------------------------------------------------------
# DISP_TXT.TCL: creates MAPTEXT.CFG file 
# Last Revised: 20 Jun 2002
#               13 Aug 2002
#               24 Sep 2009 - standardized names
#-----------------------------------------------------------------------------

global html_dir
global line1 line2 line3 line4 line5 line6 line7 line8 line9 line10
global line11 line12 line13 line14 line15 line16

if [winfo exists .disptxt] {destroy .disptxt}
set wr .disptxt
toplevel $wr
wm title $wr "Supplemental Label Information Menu"
wm  geometry $wr +50+50

frame $wr.top  
frame $wr.label 
frame $wr.exit

pack $wr.top -side top -pady 4
pack $wr.label -side top 
pack $wr.exit -side top -pady 12

#-->description

label $wr.top.lab -fg blue -relief raised -justify left -wraplength 6i \
 -text "Creates optional MAPTEXT.CFG file to add supplemental\
label information at the bottom of each plot."
pack $wr.top.lab

#-->label entry menus by line number  

foreach item [list 1 3 5 6 7 8 9 10 11 12 13 14 15] {
   entry $wr.label.e$item  -textvariable line$item  -relief sunken -width 50
   pack $wr.label.e$item -side top 
   }

#-->termination

button $wr.exit.dismiss  -bg red -text "Quit" -width 12 -command "destroy $wr"
button $wr.exit.reset  -text "Reset" -width 6 -command "clear_labels"
button $wr.exit.help -text "Help" -width 6 \
       -command "load_html [file join $html_dir S414.htm ] "
button $wr.exit.cont  -bg green -text "Save" -width 12 -command "save_labels MAPTEXT.CFG $wr"
pack  $wr.exit.dismiss $wr.exit.reset $wr.exit.help $wr.exit.cont \
      -side left -padx 4

if [file exists MAPTEXT.CFG] {
   load_labels MAPTEXT.CFG .dummy
   } else {
   reset_labels
   }
}

#--------------------------------------------------------------------------
proc clear_labels {} {
if [file exists MAPTEXT.CFG] {file delete -force MAPTEXT.CFG}
reset_labels
}

#--------------------------------------------------------------------------
proc reset_labels {} {

global line1 line2 line3 line4 line5 line6 line7 line8 line9 line10
global line11 line12 line13 line14 line15 line16

set line1  "METEOROLOGICAL COMPUTATIONAL CENTER"
set line2  "--------------------------------------------------" 
set line3  "Hysplit Dispersion Calculation                    "
set line4  "--------------------------------------------------"
set line5  "Source Location:                                  "
set line6  "Start Month/Day:                                  "
set line7  "Start Time(UTC):                                  "
set line8  "Meteorology Data Source:                          "
set line9  "Trajectory Computation Heights:                   "
set line10 "Pollutant Emission Rate:                          "
set line11 "Initial Pollutant Distribution:                   "
set line12 "Deposition Options Enabled:                       "
set line13 "Notes:                                            "
set line14 "                                                  "
set line15 "Issued:                                           "
set line16 "--------------------------------------------------"

if [file exists MAPTEXT.CFG] {file delete -force MAPTEXT.CFG}

}


#--------------------------------------------------------------------------
proc load_labels {Fname Wname} {

global line1 line2 line3 line4 line5 line6 line7 line8 line9 line10
global line11 line12 line13 line14 line15 line16

if [file exists $Fname] {
   set f [open "$Fname" r]

foreach item [list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16] {
   gets $f line$item
}

}

if [winfo exists $Wname] {destroy $Wname}
close $f
}


#------------------------------------------------------------------------------
proc save_labels {Fname Wname} {

global line1 line2 line3 line4 line5 line6 line7 line8 line9 line10
global line11 line12 line13 line14 line15 line16

set f [open "$Fname" w]

puts $f $line1    
puts $f $line2    
puts $f $line3    
puts $f $line4    
puts $f $line5    
puts $f $line6    
puts $f $line7    
puts $f $line8    
puts $f $line9    
puts $f $line10    
puts $f $line11    
puts $f $line12    
puts $f $line13    
puts $f $line14    
puts $f $line15    
puts $f $line16    

destroy $Wname
close $f
}
