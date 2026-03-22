*
*arl_06.gs:  Jet Stream Winds
*
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
*For a user supplied time step, this script will plot
*wind magnitude and wind barbs at 300 mb (jet stream)
*
*
*Be sure to open the correct .ctl file!
'open c:\pcgrads\grads_edas_1129.ctl'
'set lat 25 50'
'set lon -95 -65'
'set lev 300'
'set mpdset hires'
'set poli on'
'set display color white'
'c'
say 'enter time step (1-120):'
pull yyy
'set t 'yyy
'set gxout shaded'
'd mag(uwnd,vwnd)'
'run cbar.gs'
'set cint 5'
'set gxout contour'
'set cstyle 1'
'set ccolor 1'
'set cthick 5'
'd mag(uwnd,vwnd)'
'set gxout barb'
'set ccolor 1'
'd skip(uwnd,4,4);skip(vwnd,4,4)'
'draw title Winds at Jet Stream Level, 300 mb, mm/dd/yy tt UTC'

