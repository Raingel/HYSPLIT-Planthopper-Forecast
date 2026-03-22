*
*arl_05.gs:  Temperature Time Series
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
*
*For a user supplied time period and pressure level, this
*script will plot a time series of temperature for
*40 N and 75 W.
*
'open c:\pcgrads\grads_edas_1129.ctl'
'set lat 40'
'set lon -75'
'set t 5 29'
'set mpdset hires'
'set poli on'
'set display color white'
'c'
say 'enter pressure (mb):'
pull xxx
'set lev 'xxx
'd temp-273.15'
'draw title PHL Times Series:  Temperature (C), 'xxx' mb'

