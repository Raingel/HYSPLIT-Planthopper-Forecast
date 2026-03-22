#-----------------------------------------------------------------------------
# htmlbrws.tcl: HTML browser
# Last Revised: ?? Jul 2002 (ADT) - initial?
#               24 Aug 2021 (SYZ) - use a web browser

# set initial values
set src_dir [file dirname [info script]]
variable Home $src_dir/index.htm ;# home document

proc load_html {home {flag 0}} {
   global X_dir wb_pgm tcl_platform exec_dir

   variable Home
   if {[file isfile $home]} {
      set Home $home
   }
  
   set cur_dir [pwd]
   set html_path [file normalize [file join "${cur_dir}" "${Home}"]]
   set html_uri "file://${html_path}"

   if [file exists $Home] {
      if { "$tcl_platform(os)" == "Darwin" } {
   #       Apple Mac opens with OS supplied Preview program
           exec open "${html_uri}"
   
      } else {
   #      All other platforms use Web Browswer for display
          if [file exists $wb_pgm] {
             if { "$X_dir" == "" } {
                exec $wb_pgm "${html_uri}" &
             } else {
                exec $X_dir/xterm -fn fixed -e $wb_pgm "${html_uri}" & 
             }
          } else { 
             msg_box "Web Browser Required: $wb_pgm"
          }
      }
   } else { 
     msg_box "File not found: $Home" 
   }
}
