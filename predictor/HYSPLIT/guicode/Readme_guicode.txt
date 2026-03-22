DIRECTORY: 	./guicode	
Last Revised: 	May 2020
_________________________________________________________________

This directory contains a Tcl/Tk GUI interface for Hysplit. The 
interface was originally created by GANG LIANG (gzl@bom.gov.au), 
at BMRC, Bureau of Meteorology, Melbourne, Australia.  Substantial
modifications were introduced by albion.taylor@noaa.gov.

The interface can be used to set up the input control file as well 
as run the graphical output display programs.  To use the interface 
you must first install Tcl/Tk on your PC.  It may be part of the
installation CD or it can be obtained from:

	http://www.activestate.com

The hysplit.tcl script is the top-level script that will run all 
the other tcl scripts.  If the tcl/tk installation was done correctly, 
the .tcl suffix will be properly associated with the Tcl/Tk programs. 
You can also create a shortcut for the hysplit.tcl script and move 
the shortcut to the desktop.

#################################################################
THE GUI SHOULD NOT BE RUN FROM THIS DIRECTORY! 
RUN IT FROM ./HYSPLIT/WORKING, ./HYSPLIT, OR FROM THE DESKTOP. 
################################################################# 

If the GUI does not start, perhaps because tcl/tk was installed to
another drive or directory, then it may be necessary to edit the
default directory pointers in file: /guicode/hysplit.tcl.

The GUI also contains links to Ghostscript, Ghostview, 
(www.cs.wisc.edu/~ghost) and ImageMagick (www.imagemagick.org), 
programs which are used to display or manipulate the model's 
Postscript graphics output files. These programs should startup 
automatically when required. However, as in the case of tcl/tk,
if installed to different directories, they may not work. If the 
GUI is functioning, then you can edit the startup directories 
configuration, in the /Advanced/Configuration/Directories menu
tab.  If the GUI is not working, then the *_dir variables in 
hysplit.tcl will need to be modified accordingly.  

The GUI contains an updated on-line version of the user's guide that 
contains information for both the PC and UNIX versions of the model.

The "Version Menu" should not be deleted while the interface 
is running. 

