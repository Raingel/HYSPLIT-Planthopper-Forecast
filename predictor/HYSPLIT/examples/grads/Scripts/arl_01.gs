*
*arl_01.gs
*
*Sample plot:
*Bill Ryan, wfr1@psu.edu
*2/14/2001
*Used on Win98 PC but should be portable
*
*For a user supplied pressure level, this
*script will plot temperature and wind streamlines.
*
*The control file is:  c:\pcgrads\grads_edas_1129.ctl
*and is directed to the ARL-processed EDAS analysis
*fields containing 2 week increments of data.
*
*These files are typically named:
*	edas.subgrd.xxxyy.00n
*         where xxx=mon, yy=year, n=1 or 2
*They are processed using:
*	arl2bin.exe
*The output is:  grads.bin and is a big file!
*
*This is a very simple plot, other varieties follow
*in files arl_xx.gs
*
*As always with GrADS:
*(1) Be sure to open the correct .ctl file!
*(2) Read the GrADS documentation to know what the
*  commands "c", "reset" and "reinit" will do
*(3) Always check the time stamp via "q dims" before
*  proceeding.
*
*The time increments are 3 hours so that t=1 is 00Z
*and t=5 is 12Z on day one of the file (1st or 16th of
*the month)
*
*For any day = N, the 12Z time step = 5+8*(N-1),
*where N = 1,2.........15 (*.001 files) with
*obvious adjustment for the second two weeks of the
*month (*.002 files)
*
*
'open c:\pcgrads\grads_edas_1129.ctl'
*lat lon set for the eastern US
'set lat 25 50'
'set lon -95 -65'
*high resolution map background with political boundaries
'set mpdset hires'
'set poli on'
*The default display is black, set here to white.  Display, like
*some other parameters, will not change until clear ('c') command
*is issued.
'set display color white'
'c'
*Input from terminal is accepted via the 'say/pull' commands
say 'enter pressure:'
pull xxx
'set lev 'xxx
*The first plot is a color contour with an interval of 2 C
'set gxout shaded'
'set cint 2'
*Temperature default is Kelvin
'd temp-273.15'
*A pre-packaged routine will provide a sidebar with color codes.  This
*can be done manually as well but it's just too much trouble in most cases.
'run cbar.gs'
*To better distinguish color margins, a black line contour is overlaid with
*labels forced wherever they can fit.  If you are not color blind like me
*you can dispense with this.
'set gxout contour'
'set cint 2'
'set ccolor 1'
'set clab forced'
'd temp-273.15'
*Finally wind streamlines are plotted - still do not know how to make the
*streamline arrowheads get bigger........
'set gxout stream'
'set ccolor 1'
*This sets the density of the plotted streamlines
'set strmden 4'
'set cthick 4'
'd uwnd;vwnd'
'set annot 4 4'
'draw title 'xxx' mb Temperature (C) and Streamlines\Month Day, Year(xxxx UTC)'

