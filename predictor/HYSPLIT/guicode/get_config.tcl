proc getcfg {} {

#-----------------------------------------------------------------------------
# GET_CONFIG.TCL: sets default FTP addresses
# Last Revised: 16 Nov 2006 - initial version
#               12 Feb 2008 - removed NCEP links
#               14 Oct 2008 - simplified and added output to default_ftp
#               08 Sep 2016 - button text color changed to white
#-----------------------------------------------------------------------------

global ftparc ftpfor dirarc dirfor hostfor hostarc html_dir

if [winfo exists .get_win] {destroy .get_win}
set wr .get_win
toplevel $wr
wm title $wr "Set Default FTP Server Addresses"
wm  geometry $wr +100+100

frame $wr.title
frame $wr.ftp1
frame $wr.div1
frame $wr.ftp4
frame $wr.div2
frame $wr.end

pack  $wr.title -pady 10
pack  $wr.ftp1 $wr.div1 $wr.ftp4 $wr.div2 -side top -pady 5
pack  $wr.end -pady 10

label $wr.title.lab -fg blue -relief raised -justify left -wraplength 5i \
-text "Sets the FTP servers that are automatically accessed in the\
data FTP menus. Initial values are set in the default_ftp file. The\
server name and base directories can be modified and saved for the\
current server or to the default file."
pack  $wr.title.lab

#------------------
frame $wr.ftp1.top
frame $wr.ftp1.bot
frame $wr.ftp1.mid
pack  $wr.ftp1.top $wr.ftp1.bot $wr.ftp1.mid -anchor e

label $wr.ftp1.top.txt -fg red -text " Forecast FTP Data Server                  Directory"
pack  $wr.ftp1.top.txt

entry $wr.ftp1.bot.ent1 -textvar "ftpfor(0)" -relief sunken -width 25
entry $wr.ftp1.bot.ent2 -textvar "dirfor(0)" -relief sunken -width 25
pack  $wr.ftp1.bot.ent1 $wr.ftp1.bot.ent2 -side left

radiobutton $wr.ftp1.mid.d0 -variable hostfor -value 1 -text "Default    " \
  -command {set ftpfor(0) $ftpfor(1); set dirfor(0) $dirfor(1)} 
radiobutton $wr.ftp1.mid.d1 -variable hostfor -value 2 -text "Alternate  " \
  -command {set ftpfor(0) $ftpfor(2); set dirfor(0) $dirfor(2)} 
radiobutton $wr.ftp1.mid.d2 -variable hostfor -value 3 -text "Backup     " \
  -command {set ftpfor(0) $ftpfor(3); set dirfor(0) $dirfor(3)} 
pack $wr.ftp1.mid.d0 $wr.ftp1.mid.d1 $wr.ftp1.mid.d2 -side left

label $wr.div1.lab -text "__________________________________________________"
pack  $wr.div1.lab

#----------------------
frame $wr.ftp4.top
frame $wr.ftp4.bot
frame $wr.ftp4.mid
pack  $wr.ftp4.top $wr.ftp4.bot $wr.ftp4.mid -anchor e

label $wr.ftp4.top.txt -fg red -text "Archive FTP Data Server                   Directory"
pack  $wr.ftp4.top.txt 

entry $wr.ftp4.bot.ent1 -textvar ftparc(0) -relief sunken -width 25
entry $wr.ftp4.bot.ent2 -textvar dirarc(0) -relief sunken -width 25
pack  $wr.ftp4.bot.ent1 $wr.ftp4.bot.ent2 -side left

radiobutton $wr.ftp4.mid.d0 -variable hostarc -value 1 -text "Default    " \
  -command {set ftparc(0) $ftparc(1); set dirarc(0) $dirarc(1)}
radiobutton $wr.ftp4.mid.d1 -variable hostarc -value 2 -text "Alternate  " \
  -command {set ftparc(0) $ftparc(2); set dirarc(0) $dirarc(2)}
radiobutton $wr.ftp4.mid.d2 -variable hostarc -value 3 -text "Backup     " \
  -command {set ftparc(0) $ftparc(3); set dirarc(0) $dirarc(3)}
pack $wr.ftp4.mid.d0 $wr.ftp4.mid.d1 $wr.ftp4.mid.d2 -side left

label $wr.div2.lab -text "__________________________________________________"
pack  $wr.div2.lab

#----------------------
button $wr.end.exit  -bg red -text Quit -width 15 -command "destroy $wr"
button $wr.end.help -text "Help" -width 15 \
       -command "load_html [file join $html_dir S101.htm ] "
button $wr.end.save  -bg green -text "Save Edits" -width 15 -command "but_server"
pack $wr.end.exit $wr.end.help $wr.end.save -side left  -padx 10
}

#--------------------------------------------------------------------------

proc but_server {} {
if [winfo exists .saveftp] {destroy .saveftp}
set ws .saveftp
toplevel $ws
wm title $ws "Save FTP Server Edits"
wm  geometry $ws +150+150

frame $ws.win
frame $ws.bot
pack  $ws.win $ws.bot -side top -pady 10

label $ws.win.txt -text "FTP Server and/or Directory Modifications"
pack  $ws.win.txt -side left -padx 5

button $ws.bot.quit -text "Quit no save" -width 15 -bg red    \
       -command {destroy .saveftp}
button $ws.bot.save -text "Save Changes" -width 15 -bg green  \
       -command {set_server; destroy .saveftp}
button $ws.bot.file -text "Save to File" -width 15 -bg yellow \
       -command {put_server; destroy .saveftp}
pack   $ws.bot.quit $ws.bot.save $ws.bot.file -side left -padx 5 
}

#----------------------------------------------------------------------------

proc set_server {} {
  global ftparc ftpfor dirarc dirfor hostfor hostarc
  set ftpfor($hostfor) $ftpfor(0)
  set dirfor($hostfor) $dirfor(0) 
  set ftparc($hostarc) $ftparc(0)
  set dirarc($hostarc) $dirarc(0)
}

proc put_server {} {
  global ftparc ftpfor dirarc dirfor
  set_server
  set f [open default_ftp w]
  for {set n 0} {$n <= 3} {incr n} {
      puts  $f $ftpfor($n)
      puts  $f $dirfor($n)
      puts  $f $ftparc($n)
      puts  $f $dirarc($n)
  }
}
