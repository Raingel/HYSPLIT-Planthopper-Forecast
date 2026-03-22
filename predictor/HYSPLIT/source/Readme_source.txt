DIRECTORY: ./source
___________________________________________________

Contains the source code for all the executables
found in the ./exec directory which can be created
by running make:

Makefile        - create all HYSPLIT executables using the
                  default GNU compiler option gfortran
___________________________________________________

PROGRAM: /exec/accudiv

 calculates minimum mean square error between a the average of a series of combinations 
 of ensemble runs and measured values.
 Starting with groups of one, two, three, ...., up to nth member number, makes all
 possible model combinations.
 Requires DATEM formatted model outputs and measurements.

---------------------------------------------

PROGRAM: /exec/add_data
 Usage: edit_data [station data file name] [ARL gridded file name] ... 
       (optional -g#, where # is ngrid)
       (optional -t#, where # is 1 for blending temperature or 0 for not)
 -----------------------------------------------------------------
 Edit packed meteo data based upon selected observations from     
 input file. The file containing wind direction and speed at      
 various times, locations, and heights (any order) is read.       
 Observations are matched with the gridded data interpolated to   
 the same location. Gridded winds are then adjusted in direction  
 and speed to match the observation. Those winds are then inter-  
 polated (using 1/r^2) back into the gridded data domain with the 
 mixed layer. The program is not intended to be a replacement for 
 data assimilation but a quick way to adjust the initial transport
 direction to match local observations near the computational     
 starting point.   The optional ngrid parameter is used to limit  
 the radius of influence of the 1/r^2 weighting to ngrid number   
 of grid points in a radial direction from the observation        
 locations.  A value of 9999 will use the whole grid.
 -------------------------------------------------------------------
 sample ascii station observation input file (missing = -1.0)
 -------- required -----------------------  ---- optional ---
 YY MM DD HH MM  LAT   LON  HGT   DIR  SPD  TEMP Up2 Vp2 Wp2
                             m    deg  m/s  oK   ...m2/s2...
 95 10 16 00 00 39.0 -77.0 10.0 120.0 15.0 270.0 1.0 1.0 0.5
 95 10 16 02 00 39.0 -77.0 10.0 120.0 15.0 270.0 1.0 1.0 0.5
 95 10 16 04 00 39.0 -77.0 10.0 120.0 15.0 270.0 1.0 1.0 0.5
 --------------------------------------------------------------------
    
----------------------------------
PROGRAM: /exec/add_grid
 Program to interpolate an existing meteorological data file
 to a finer spatial resolution grid at integer intervals of
 the original grid spacing.
    
----------------------------------
PROGRAM: /exec/add_miss
 Examines a file for whole missing time periods (records missing)
 and creates a new output file with interpolated data inserted into
 the proper locations. The first two time periods in the file cannot
 be missing.
    
----------------------------------
PROGRAM: /exec/add_time
 Program to interpolate additional time periods into a meteorological
 data file. The new data output interval should be an even multiple
 of the existing data file.
    
----------------------------------
PROGRAM: /exec/add_velv
 Extracts a subgrid from a larger domain file and adds additional
 3D records for the velocity variances: u^2, v^2, and w^2. These are
 computed from the TKE field. The program requires the input
 meteorological file to contain the turbulent kinetic energy field.
 The output file may then used as an input file for other programs
 that will add observational data.
    
----------------------------------
PROGRAM: /exec/arl2grad
 Convert ARL packed meteorological data to Grads format using
 hysplit source code library routines
  
 Usage: arl2grad {/meteo_dir_name/} {arl_filename} {optional output filename}
    
----------------------------------
PROGRAM: /exec/arl2meds
 Converts one ARL SHGT (terrain height) data record to MEDS format. This file
 may be required by the meds2arl converter if the surface terrain (ST) variable
 is not included within the MEDS data file. Note that the data may need to be
 realigned with either the prime meridian or dateline to match the alignment
 of the MEDS data. The MEDS data alignment is written to the meds.txt
 diagnostic message output file by the meds2arl program. Therefore it may take
 one or two iterations of arl2meds and meds2arl to get the proper command line
 input parameters.
    
----------------------------------
PROGRAM: /exec/arw2arl
 USAGE: arw2arl [data file name]
 arw2arl - Advanced Research WRF to ARL format converts ARW
 NetCDF files to a HYSPLIT compatible format. When the input
 file contains data for a single time period, then the ARL format
 output file from each execution should be appended (cat >>) to
 the output file from the previous time periods execution.
    
----------------------------------
PROGRAM: /exec/ascii2shp
    
----------------------------------
PROGRAM: /exec/boxplots
 Program to read probability files output from conprob and
 create up to 12 (time periods) box plots per page.
  
 USAGE: boxplots [-arguments]
 -a[ascii output file]
 -c[concentration conversion factor]
 -m[minimum scale for plot]
 -M[maximum scale for plot]
 -s[start time period (1)]
 -x[longitude]
 -y[latitude]
    
------------------------------------------
PROGRAM: /exec/c2datem
 Program to read hysplit concentration file and merge
 results with measurements in the DATEM file format

 USAGE: c2datem -[arguments]
 -c[input to output concentration multiplier]
 -d[write to diagnostic file]
 -h[header info: 2=input text, (1)=file name, 0=skip]
 -i[input concentration file name]
 -m[measurement data file name]
 -o[output concentration file name]
 -p[pollutant index select for multiple species]
 -r[rotation angle:latitude:longitude]
 -s[supplemental lat-lon information to output]
 -x[n(neighbor) or i(interpolation)]
 -z[level index select for multiple levels, if z=-1 read height from DATEM file]

----------------------------------
PROGRAM: /exec/chk_data
 Simple program to dump the first few elements of the data array for each
 record of an ARL packed meteorological file. Used for diagnostic testing.
    
----------------------------------
PROGRAM: /exec/chk_file
 Usage: chk_file [-i{nteractive} -s{hort} -f{file} -t{ime}]
    
----------------------------------
PROGRAM: /exec/chk_index
 Check the extended header for each meteorological index record
    
----------------------------------
PROGRAM: /exec/chk_rec
 Simple program to dump the 50 byte ascii header on each record
    
----------------------------------
PROGRAM: /exec/chk_times
 Simple program to list all the time periods in a file
    
----------------------------------
PROGRAM: /exec/clusend
 USAGE: clusend - [ optioins (default)]
   -a[max # of clusters: #, (10)]
   -i[input file (DELPCT)]
   -n[min # of clusters: #, (3)]
   -o[output file (CLUSEND)]
   -t[min # of trajectories: #, (30)]
   -p[min % change in TSV difference from one step to next: %, (30)]
  
 NOTE: leave no space between option and value
    
----------------------------------
PROGRAM: /exec/cluslist
 USAGE: cluslist - [ options (default)]
   -i[input file (CLUSTER)]
   -n[number of clusters: #, (-9-missing)]
   -o[output file (CLUSLIST)]
  
 NOTE: leave no space between option and value
    
----------------------------------
PROGRAM: /exec/clusmem
 USAGE: clusmem - [ options (default)]
   -i[input file (CLUSLIST)]
   -o[output file (TRAJ.INP.Cxx)]
  
 NOTE: leave no space between option and value
    
----------------------------------
PROGRAM: /exec/clusplot
 USAGE: clusplot -[options (default)]
   -i[Input files: name (DELPCT)]
   -l[Label with no spaces (none)]
   -o[Output file name: (clusplot.ps)]
   -p[Process file name suffix: (ps) or label]
   
 NOTE: leave no space between option and value
    
----------------------------------
PROGRAM: /exec/cmp2arl
 Convert Navy COAMPS-2 output files to ARL format
  
 Usage: cmp2arl [Arg-1] [Arg-2] [Arg-3] [Arg-4]
   1-[Directory] : Data directory with coamps files
   2-[yyyymmddhh]: Time of first file to series
   3-[ngrid]     : Selected coamps grid number
   4-[hour]      : Starting forecast hour
    
----------------------------------
PROGRAM: /exec/cmp3arl
 Convert Navy COAMPS3 output files to ARL format
  
 Usage: cmp2arl [Arg-1] [Arg-2] [Arg-3] [Arg-4]
   1-[Directory] : Data directory with coamps files
   2-[yyyymmddhh]: Time of first file to series
   3-[ngrid]     : Selected coamps grid number
   4-[hour]      : Starting forecast hour
    
----------------------------------
PROGRAM: /exec/con2arcv
 Converts binary concentration file to ESRI Arcview binary
 raster file format.
  
 Usage: con2arcv [6 character file ID]
    
----------------------------------
PROGRAM: /exec/con2asc
 Converts binary concentration file to simple lat/lon based ascii file, one
 record per grid point, for any grid point with any level or pollutant not
 zero. One output file per sampling time period. 
  
 USAGE: con2asc -[options (default)]
   -c[Convert all records to one diagnostic file]
   -d[Delimit output by comma flag]
   -i[Input file name (cdump)]
   -m[Minimum output format flag]
   -o[Output file name base (cdump)]
   -s[Single output file flag]
   -t[Time expanded (minutes) file name flag]
   -u[Units conversion multiplier concentration (1.0)]
   -U[Units conversion multiplier deposition (1.0)]
   -x[Extended precision flag]
   -z[Zero points output flag]
   
 NOTE: leave no space between option and value
    
----------------------------------
PROGRAM: /exec/con2dose
 Temporally averages input binary concentration file, converts
 to dose units and outputs a new binary dose file.
  
 Usage: con2dose [input file] [output file]
    
----------------------------------
PROGRAM: /exec/con2grad
 Converts binary hysplit concentration file to grads binary format
 Usage: con2grads [filename]
    
----------------------------------
PROGRAM: /exec/con2stn
 Program to read a hysplit concentration file and
 print the contents at selected locations.
  
 USAGE: con2stn [-options]
 -i[input concentration file name]
 -o[output concentration file name]
 -c[input to output concentration multiplier]
 -s[station list file name]
 -x[(n)=neighbor or i for interpolation]
 -z[level index (1) or 0 for all levels]
 -p[pollutant index (1) or 0 for all pollutants]
 -r[record format output (default column format)]
 -m[maximum number of stations (200)]
 -d[mm/dd/yyyy format: (0)=no 1=w-Label 2=wo-Label]
    
------------------------------------------------------------
PROGRAM: /exec/conappend

PROGRAM TO READ MULTIPLE HYSPLIT CONCENTRATION FILES AND
 APPEND THE VALUES INTO A SINGLE FILE. FILES NEED TO BE
 IDENTICAL EXCEPT EACH REPRESENTS A DIFFERENT TIME PERIOD.
  
 USAGE: conappend -[options]
   -i[File name of containing input file names]
   -o[Output summation file]
   -c[Concentration conversion factor (1.0)]

----------------------------------
PROGRAM: /exec/conavgpd
 USAGE: conavgpd -[options(default)]
    -i[input file name (cdump)]
    -o[output file name (xdump)]
    -m[concentration multiplier (1.0)]
    -a[start averaging period (YYMMDDHH)]
    -b[stop averaging period (YYMMDDHH)]
    
----------------------------------
PROGRAM: /exec/concadd
Program to add together two gridded HYSPLIT concentration files, where the
 input file is added into the base file and written as a new file. The input
 and base file need not be identical in time, but they must have the same
 number of pollutants and levels. The file contents are matched by index
 number and not height or species. The horizontal grids need to be identical
 or even multiples of each other and they also need to intersect at the
 corner point. Summations will occur regardless of any grid mismatches.
 Options are also available to select the maximum value or define the input
 file as a zero-mask such that grid points with non-zero values become zero
 at those locations in the base file when written to the output file. The
 intersect option only adds the input file to the base file when both
 have values greater than zero at the intersection point, otherwise the
 value is set to zero. The replace option will replace the value from the
 base file with the value from the input file. This option is normally used
 in conjunction with a non-zero radius.

 USAGE: concadd -[options(default)]
    -i[input file name (cdump)]
    -b[base file name to which input file is added (gdump)]
    -o[output file name with base file format (concadd.bin)]
    -g[radius (grid points around center) to exclude; default=0]
    -c[concentration conversion factor (1.0)]
    -p[sum process (0)=add | 1=max value | 2=zero mask | 3=intersect]
                           | 4=replace   | 5=divide c1/c2]
    -z[zero mask value (base set to zero when input>mask; default=0.0)]
        if zero mask value < 0 :
         base set to zero when input> zero mask value * base
    
----------------------------------
PROGRAM: /exec/concplot
 USAGE: concplot -[options (default)]
   -a[Arcview GIS: 0-none 1-log10 2-value 3-GoogleEarth]
   -b[Bottom display level: (0) m]
   -c[Contours: (0)-dyn/exp 1-fix/exp 2-dyn/lin 3-fix/lin 4-set 50-0,interval 10 51-1,interval 10]
   -d[Display: (1)-by level, 2-levels averaged]
   -e[Exposure units flag: (0)-concentrations, 1-exposure, 2-threshold, 3-hypothetical volcanic ash]
   -f[Frames: (0)-all frames one file, 1-one frame per file]
   -g[Circle overlay: ( )-auto, #circ(4), #circ:dist_km]
   -h[Hold map at center lat-lon: (source point), lat:lon]
   -i[Input file name: (cdump)]
   -j[Graphics map background file name: (arlmap) or shapefiles.<txt|suffix>]
   -k[Kolor: 0-B&W, (1)-Color, 2-No Lines Color, 3-No Lines B&W]
   -l[Label options: ascii code, (73)-open star]
   -L[LatLonLabels: none=0 auto=(1) set=2:value(tenths)]
   -m[Map projection: (0)-Auto 1-Polar 2-Lamb 3-Merc 4-CylEqu]
   -n[Number of time periods: (0)-all, numb, min:max, -incr]
   -o[Output file name: (concplot.ps)]
   -p[Process file name suffix: (ps) or process ID]
   -q[Quick data plot: ( )-none, filename]
   -r[Removal: 0-none, (1)-each time, 2-sum, 3-total]
   -s[Species: 0-sum, (1)-select, #-multiple]
   -t[Top display level: (99999) m]
   -u[Units label for mass: (mass), see "labels.cfg" file]
   -v[Values[:labels (optional)] for fixed contours: val1:lab1+val2:lab2+val3:lab3+val4:lab4]
   -w[Grid point scan for contour smoothing (0)-none 1,2,3, grid points]
   -x[Concentration multiplier: (1.0)]
   -y[Deposition multiplier:    (1.0)]
   -z[Zoom factor: 0-least zoom, (50), 100-most zoom]
   
 NOTE: leave no space between option and value
    
----------------------------------
PROGRAM: /exec/concrop
 USAGE: trajplot -[options (default)]
   -f[FCSTHR.TXT: (0)-none 1-output]
   -i[Input file name: (cdump)]
   -o[Output file name: (ccrop)]
   
 NOTE: leave no space between option and value
    
----------------------------------
PROGRAM: /exec/conlight
 Extracts individual records from the binary concentration file
 where every {count}th record is output and if the file contains
 multiple species the {species} is selected for output. Multiplier
 and minimum values may be applied.
  
 USAGE: conlight -[options(default)]
    -i[input file name (cdump)]
    -o[output file name (xdump)]
    -m[concentration multiplier (1.0)]
    -p[period MMDD:MMDD]
    -s[species: 0-sum, (1)-select]
    -t[time periods to extract (1)]
    -z[concentration minimum (0.0)]
    -y[concentration minimum for sum pollutants (0.0)]
    
----------------------------------
PROGRAM: /exec/conmask
 PROGRAM READS TWO GRIDDED HYSPLIT CONCENTRATION FILES AND
 APPLIES THE SECOND FILE AS A MASK TO THE FIRST. ANY NON-ZERO
 VALUE IN FILE #2 BECOMES ZERO IN FILE #1. Also see concadd!
  
 Usage: conmask [input] [mask] [output] [cmin]
    [input]  - name of input file of concentrations
    [mask]   - name of mask file where input may = 0
    [output] - name output file from input*mask
    [cmin]   - input gt cmin mask then set input = 0
    
------------------------------------------------------------------
 PROGRAM: /exec/concmbn

 Program to combine two gridded HYSPLIT concentration files; one a fine
 grid and one a coarse grid.  The coarse grid is recalculated on a grid
 covering the same area except that the grid has the resolution of the
 fine grid provided. 
  
 The file contents are matched by index number but not height or species.
 You must make sure that the fine grid spacing and span are even multiples of 
 the coarse grid spacing and span (ie., coarse grid points must be also be 
 points on the fine grid. Summations will occur regardless of any grid mismatche
 s.
  
 As an example:
 Initial fine grid spacing:     0.005 and span: 0.2
 Initial coarse grid spacing:   0.015 and span: 0.6
 Final large fine grid spacing: 0.005 and span: either 0.6 or less if cropped
  
 Prior to writing the coarse grid values into to the final large fine
 grid, the new fine grid values are given a 1/r2 weighting using the surrounding
 coarse grid values. 
  
 Options:
  
 -s/-S: additional smoothing is available to smooth the new large
 area fine grid before the original limited area fine grid replaces
 the grid points on the new large area fine grid.
  
 -t/-T: average the perimeter initial fine grid points with the underlying
 new large fine grid points to provide smoother transition between the grids.
  
 -b/-B: the final large fine grid is cropped to reduce the file size and white
 space.
  
 USAGE: concmbn -[options(default)]
    -c[coarse grid file name (cdump)]
    -f[fine grid file name (fdump)]
    -o[output file name (concmbn.bin)]
    -s[number of surrounding coarse grid points to average, (0)=none, x]
    -t[number of perimeter fine grid points to average, (0)=none, x]
    -p[percent of white space added around plume (10)]
    -b[crop the final grid: yes:(1), no:0]

----------------------------------
PROGRAM: /exec/conmerge
 PROGRAM TO READ MULTIPLE HYSPLIT CONCENTRATION FILES AND
 SUM THE VALUES TO A FILE.  OPTIONS TO SUM ONLY THE SAME
 TIME PERIODS OR SUM ALL TIME PERIODS INTO ONE
  
 USAGE: conmerge -[options]
   -d[Date to process: YYMMDDHH]
   -i[Input file name of file names]
   -o[Output summation file]
   -t[Time summation flag]
   
 NOTE: leave no space between option and value
       First input file should contain all time periods
    
----------------------------------
PROGRAM: /exec/conprob
 READS MULTIPLE HYSPLIT CONCENTRATION FILES AND COMPUTES VARIOUS PROBABILIY
 LEVELS AT EACH GRID POINT AND THEN WRITTING A NEW OUTPUT FILE FOR EACH
 PROBABILITY LEVEL
  
 Usage: conprob [-options]
   -b[base] input file name
   -c[concentration] value set
   -d[dignostics = true]
   -p[pollutant] index number
   -t[temporal] aggregation period
   -v[value] below which equals zero
   -z[level] index number
    
----------------------------------
PROGRAM: /exec/conpuff
 READS THE GRIDDED HYSPLIT CONCENTRATION FILE AND PRINTS THE
 MAXIMUM CONCENTRAION AND PUFF MASS CENTROID LOCATION FOR
 EACH CONCENTRATION AVERAGING PERIOD
    
----------------------------------
PROGRAM: /exec/conread
 SAMPLE PROGRAM TO READ THE GRIDDED HYSPLIT CONCENTRATION FILE AND
 DUMP SELECTED STATISTICS EACH TIME PERIOD TO STANDARD OUTPUT
    
----------------------------------
PROGRAM: /exec/constats
 Compare two HYSPLIT concentration files by computint the FMS
 overlap Statistic with the assumption that both grids must be identical
 in terms of grid size, levels, pollutants, and number of time periods.
  
 USAGE: constats {arguments}
 -f#[concentration file name (#<=2)]
 -o[output file name; undef stdout]
 -t[temporal match skip hour]
 -v[verbose output]
    
----------------------------------
PROGRAM: /exec/content
 Dump the contents, by section, of a GRIB1 file
    
----------------------------------
PROGRAM: /exec/contour
 Contour fields from a meteorological data file
  
 Usage: contour [-options]
   -D[Input metdata directory name with ending /]
   -F[input metdata file name]
   -Y[Map center latitude]
   -X[Map center longitude]
   -R[Map radius (deg lat)]
   -V[Variable name to contour (e.g. TEMP)]
   -L[Level of variable (sfc=1)]
   -O[Output time offset (hrs)]
   -T[Output time interval (hrs)]
   -C[Color (1) or black and white (0)]
   -G[Graphics map file (arlmap) or shapefiles.txt]
   -M[Maximum contour value (Auto=-1.0)]
   -I[Increment between contours (Auto=-1.0)]
   -A[Arcview text output]
  
 NOTE: leave no space between option and value
  
    
----------------------------------
PROGRAM: /exec/dat2cntl
 Converts DATEM format observational file into a CONTROL file
  
 USAGE: dat2cntl -c[conversion] -d[0|1] -i[datem data file name]
   -c[units conversion factor (1.0)]
   -d[weight measured data to denominator (0)]
   -i[input file name]
    
----------------------------------
PROGRAM: /exec/data_avrg
 Averages gridded meteorological data according to input options

------------------------------------------
PROGRAM: /exec/data_del
This program deletes a variable from the ARL formatted meteorogical file 
    
----------------------------------
PROGRAM: /exec/data_year
Simple program to create annual averages of selected meteorological variables
at a one or more locations.

----------------------------------
PROGRAM: /exec/edit_flux
 Edit the flux fields based upon a pre-determined roughness length
 From U = U* k / ln(Z/Zo) write a new equation with a modified Zo^ that
 represents the new larger roughness length. Take the ratio of the two
 equations such that U*^/U* = ln(Z/Zo)/ln(Z/Zo^). For computational
 purposes, assume that Z is always one meter greater than Zo^. Then
 the the momentum flux fields are multiplied by this ratio while T* is
 divided by the ratio. Output written to editflux.bin
    
----------------------------------
PROGRAM: /exec/edit_head
 Edit 50 byte ASCII header of each data record of a pre-existing
 meteorological data file. Program should be recompiled to perform
 other edits besides changing the incorrect time labels.
    
----------------------------------
PROGRAM: /exec/edit_index
 Edit the extended header for each index record of an existing
 meteorological data file. The program needs to be modified and
 then recompiled to customized the edit for each problem
    
----------------------------------
PROGRAM: /exec/edit_miss
 Program edit_miss is used to interpolate missing variables
 in an existing meteorological data file from adjacent time
 periods. The missing data must already exist in the file as
 valid records with either a blank or missing code in the field.
 For files with missing records, use program add_miss
    
----------------------------------
PROGRAM: /exec/edit_null
 This program replaces one record per time period in a file
 where the missing record is identified by NULL. The new
 field is read from another file than only contains the one
 variable. The records are matched by time. The program can be
 used to insert precipitation record into a file.

-----------------------------------------

PROGRAM: /exec/ensperc 
 calculates percentiles of each ensemble runs and measured values.
 Requires DATEM formatted model outputs and measurements.    

----------------------------------
PROGRAM: /exec/ensplots
 Reads the individual ensemble files to show the member number
 distribution by concentration for a single location.
  
 USAGE: ensplots [-arguments]
 -b[base name for concentration files]
 -c[concentration conversion factor]
 -m[minimum scale for plot]
 -M[maximum scale for plot]
 -x[longitude]
 -y[latitude]
    
----------------------------------
PROGRAM: /exec/file_copy
 Append meteorological data files
 Yes ... its called file_copy
  
 Usage: file_copy [file1] [file2]
    
----------------------------------
PROGRAM: /exec/file_merge
 Merges file #1 into file #2 by matching the initial time period
    
----------------------------------
PROGRAM: /exec/filedates
 Usage: filedates [input file]
 Unable to find:-h                                                                              
    
----------------------------------
PROGRAM: /exec/findgrib
 Finds the starting GRIB string at the beginning of each grib record
 and determines the record length between GRIB records from the byte
 count and the actual record length encoded in the binary data
    
----------------------------------
PROGRAM: /exec/grad2arl
 Usage: grad2arl [-options {default}]
  -b[Input binary file name {gdas.bin}]
  -i[Input control file name {gdas.grads.cntl}]
  -o[Output ARL file name {gdas.arl}]
  -k[Number of output levels above sfc {40}]
  -c[A,B coordinate file for hybrid levels {abfile}]
  -m[Model identification (4-characters) {MDID}]
 
 File not found: gdas.grads.cntl                                                                 
 File not found: gdas.bin                                                                        
    
----------------------------------
PROGRAM: /exec/gridplot
 Create postscript file to show concentration field evolution
 by using color fill of the concentration grid cells, designed
 especially for global grids
  
 USAGE: gridplot -[options(default)]
   -i[input file name (cdump.bin)]
   -o[output name (plot)]
   -l[lowest interval value (1.0E-36)]
   -m[multi frames one file (0)]
   -d[delta interval value (1000.0)]
   -s[species number to display: (1); 0-sum]
   -h[height of level to display (m) (integer): (0 = dep)]
   -a[scale: 0-linear, (1)-logarithmic]
   -x[longitude offset (0), e.g., -90.0: U.S. center; 90.0: China center]
   -f[(0), 1-ascii text output files for mapped values]
    
----------------------------------
PROGRAM: /exec/gridxy2ll
 Return lat/lon of an x/y point on a meteorological grid
  
 Usage: gridxy2ll [-filename options]
      -D[input metdata directory name with ending /]
      -F[input metdata file name]
      -P[process ID number for output file]
      -X[x point]
      -Y[y point]
      -W[image width]
      -H[image height]
  
 NOTE: leave no space between option and value
  
 ------------------------------------------

PROGRAM: /exec/hysptest

This program is similar to the main program for the transport and 
dispersion model HYSPLIT. However, it does no calculations, only evaluates
the settings in the CONTROL and SETUP.CFG namelist files for consistency.
The option exists to write a new corrected CONTROL and namelist file.

   
----------------------------------
PROGRAM: /exec/inventory
 Produces and inventory listing of all the records in GRIB1 file
    
----------------------------------
PROGRAM: /exec/isochron
 Shows contours of the time (hours from start) at which the
 concentration first exceeds a specific value
  
 USAGE: isochron -[options(default)]
   -i[input file name (cdump)]
   -o[output name (chron)]
   -t[threshold value (1.0E-36)]
   -c[color fill no:0 yes:(1)]
   -f[Frames: (0)-all frames one file, 1-one frame per file]
   -n[number of contours (10)]
   -d[difference (h) between contours (24), -1 to use conc interval]
   -j[Graphics map background file name: (arlmap)]
    
----------------------------------
PROGRAM: /exec/laps2arl
 USAGE: laps2arl [data file name]
 Initial configuration when no BIN or CFG file
 Insert into existing BIN file when CFG exists
 First call should always be a 3D input file
    
----------------------------------
PROGRAM: /exec/latlon
 Generates multiple lat-lon source points from a control file in which the
 first lat lon points are considered to be the corner point and the third
 point is the delta lat-lon increment.
    
----------------------------------
PROGRAM: /exec/matrix
 The MATRIX program is used to extract source or receptor information from the
 HYSPLIT generated source-receptor matrix.  Two output modes are available. In
 receptor mode a receptor location is specified and the program computes the
 contribution of each source to the selected receptor point.  In source mode
 a source location is specified and the program outputs the concentrations at
 all the receptor locations. All output files are in standard HYSPLIT binary.
 In forward mode and providing a data file of measured values in DATEM format
 the code will compute the inverse of the coefficient matrix and solve for
 the values of the source emission rate vector to satisfy the measured values.
 The default approach is to use Singular Value Decomposition on the CM, which
 is defined by M equations (receptors) and N unknowns (sources).
  
 Usage: matrix [-options]
   -i[input file name]
   -o[output file name]
   -y[latitude]
   -x[longitude]
   -z[level vertical index]
   -m[source (s) or receptor (r) mode]
   -d[yymmddhh date]
   -n[normalization on]
   ----------------------------------
   -s[datem format station file name]
   -b[datem zero base value (0.0)]
   -c[convert measured data units (1.0)]
   -r[number of iterations (1)]
   -v[verbose output to matrix.txt]
    
----------------------------------
PROGRAM: /exec/meds2arl
 Usage: meds2arl [-options {default}]
  -i[Input MEDS file name {meds.bin}]
  -t[Input MEDS terr name {meds.fix}]
  -o[Output ARL file name {meds.arl}]
  -r[Resolution MEDS data degs {1.0}]
    
----------------------------------
PROGRAM: /exec/merglist
 USAGE: merglist -[options (default)]
   -i[Input files: name1+name2+... or +listfile or (tdump)]
   -o[Output file base name: (mdump)]
   -p[Process file name suffix: (tdump) or process ID]
   
 NOTE: leave no space between option and value
    
----------------------------------
PROGRAM: /exec/metpoint
 Determine if a location is within the domain of an ARL formattted
 meteorological data file. Program command line contains lat, lon, file,
 and returns the i,j of the position.  Negative values are outside the grid.
  
 Usage: metpoint [directory filename latitude longitude]
    
----------------------------------
PROGRAM: /exec/nam2arl
 USAGE: nam2arl [data_file_name] [lat1] [lon1] [lat2] [lon2]
 nam2arl - NCEPs NAM to ARL format converts NAM
 NetCDF files to a HYSPLIT compatible format. When the input
 file contains data for a single time period, then the ARL format
 output file from each execution should be appended (cat >>) to
 the output file from the previous time periods execution.
 An optional extraction subgrid can be specified the corner lat
 -lon points, where 1 is the lower left and 2 is the upper right.
    
-----------------------------------------
PROGRAM: /exec/nuctree 
 This program will display the daughter nuclides produced by a parent 
 nuclide along with the half-life and branching fractions of each nuclide.
 It will also read a CONTROL file and use the pollutant ID to determine 
 the daughter products and create an additional CONTROL.DAUGHTER file containing  
 the daughter products information.  
  Usage: nuctree -n [-options]
  -o[output activity file name {DAUGHTER.TXT}]
  -n[Parent nuclide name (ie, Cs-137, I-131)]
    
----------------------------------
PROGRAM: /exec/parhplot
 Plot the horizontal mass distribution from a PARDUMP file
  
 USAGE: parhplot -[options(default)]
    -a[GIS output: (0)-none, 1-GENERATE, 3-kml]
    -i[input file name (PARDUMP)]
    -k[Kolor: (0)-B&W 1-Color]
    -m[scale output by mass (1)-yes 0-no]
    -o[output file name (parhplot.ps)]
    -j[Map background file: (arlmap) or shapefiles.txt]
    -p[Process file name suffix: (ps) or process ID]
    -z[Zoom factor:  0-least zoom, (50), 100-most zoom]
    
----------------------------------
PROGRAM: /exec/parmerge
 Merge multiple PARDUMP.XXX files into a single file
 These files are normally created by the MPI version
  
 USAGE: parmerge -[options(default)]
  -i[input base file name (PARDUMP).000]
  -o[output file name (PARDUMP)]
  Loops sequentially 001->999 through
  pardump files to the first missing file
  and merges the contents into one file
    
----------------------------------
PROGRAM: /exec/parvplot
 Plot the vertical mass cross-section from a PARDUMP file
  
 USAGE: parvplot -[options(default)]
    -i[input file name (PARDUMP)]
    -k[Kolor: (0)-B&W 1-Color]
    -m[scale output by mass (1)-yes 0-no]
    -o[output file name (parvplot.ps)]
    -p[Process file name suffix: (ps) or process ID]
    -z[Zoom factor:  0-least zoom, (50), 100-most zoom]
    
----------------------------------
PROGRAM: /exec/parxplot
 Plot the vertical cross-section through the plume center
  
 USAGE: parxplot -[options(default)]
    -i[input file name (PARDUMP)]
    -k[Kolor: (0)-B&W 1-Color]
    -m[scale output by mass (1)-yes 0-no]
    -o[output file name (parxplot.ps)]
    -j[Map background file: (arlmap) or shapefiles.txt]
    -p[Process file name suffix: (ps) or process ID]
    -x[Force cross section: lat1,lon1,lat2,lon2]
    -z[Zoom factor:  0-least zoom, (50), 100-most zoom]
    
----------------------------------
PROGRAM: /exec/pole2merc
 Convert NH-SH polar sterographic meteorology to mercator projection
    
----------------------------------
PROGRAM: /exec/profile
 Usage: profile [-options]
   -d[Input metdata directory name with ending /]
   -f[input metdata file name]
   -y[Latitude]
   -x[Longitude]
   -o[Output time offset (hrs)]
   -t[Output time interval (hrs)]
   -w[Wind direction instead of components]
  
 NOTE: leave no space between option and value
  
    
----------------------------------
PROGRAM: /exec/rec_copy
 Copies one record from file #1 to file #2 at the time specified
    
----------------------------------
PROGRAM: /exec/rec_insert
 This program inserts (replaces) one record per time period
    
----------------------------------
PROGRAM: /exec/rec_merge
 This variation merges in an additional data file with one record
 per time period, reading an old archive format data sets (without
 the index record) to the new style format.
    
----------------------------------
PROGRAM: /exec/showgrid
 Usage: showgrid [-filename options]
      -D[input metdata directory name with ending /]
      -F[input metdata file name]
      -P[process ID number for output file]
      -I[grid point plotting increment (0=none)]
      -L[lat/lon label interval in degrees]
      -A[location of arlmap or shapefiles.<txt|suffix>]
      -X[Read from standard input]
      -Q[subgrid lower left latitude (xxx.xx)]
      -R[subgrid lower left longitude (xxxx.xx)]
      -S[subgrid upper right latitude (xxx.xx)]
      -T[subgrid upper right longitude (xxxx.xx)]
      -B[plot symbol at each lat/lon in file SYMBPLT.CFG]
  
 NOTE: leave no space between option and value
  
    
----------------------------------
PROGRAM: /exec/snd2arl
 Usage: snd2arl [filein] [fileout] [clat] [clon] [mixing] [yymmddhh] [optional process ID number]
 Creates one time period ARL packed format from a single rawinsonde
    1007.00    27.40    18.40    93.00   150.00     4.63
    1000.00    26.60    17.60   146.00   151.60     4.55
     925.00    20.20    16.10   828.00   169.54     3.63
     891.00    17.40    15.30  1150.44   178.16     3.18
     883.89    17.20    15.25  1219.00   180.00     3.09
    
----------------------------------
PROGRAM: /exec/stabplot
 USAGE: stabplot -i -n -y
 -i[process ID number]
 -n[sequential station number (1+2+...+n]
 -y[auto y axis log scaling]
    
----------------------------------
PROGRAM: /exec/stn2arl
 Usage: stn2arl [filein] [fileout] [clat] [clon] [optional process ID number]
 Creates multiple time periods ARL packed from surface obs
 07 09 04 11 00 270 5.0 1500.0 4
 07 09 04 12 00 280 5.0 1500.0 4

------------------------------------------

PROGRAM: /exec/testnuc
 tests the daughter product subroutines
    
----------------------------------
PROGRAM: /exec/timeplot
 USAGE: timeplot -i -n -y
 -i[input concentration file name]
 -m[minimum ordinate value (0.0)]
 -n[sequential station number (1+2+...+n]
 -p[draw only points, no connecting line]
 -y[auto y axis log scaling]
 -z[three char time label]
    
----------------------------------
PROGRAM: /exec/timeplus
 Usage: timeplus year(I2) month(I2) day(I2) hour(I2) fhour(I4)
    
----------------------------------
PROGRAM: /exec/trajfind
 Usage: trajfind [in_file] [out_file] [lat] [lon]
 Processes multiple trajectory in_file from splitting option
 to extract a single trajectory to out_file that passes
 nearest to the selected lat/lon given on the command line.
    
----------------------------------
PROGRAM: /exec/trajfreq
 Converts multiple trajectory input files into a concentration
 file that represents trajectory frequencies.
  
 USAGE: trajfreq -[options (default)]
   -f[frequency file name (tfreq.bin)]
   -g[grid size in degrees (1.0)]
   -i[input file of file names (INFILE)]
    
----------------------------------
PROGRAM: /exec/trajfrmt
 Usage: trajfmt [file1] [file2]
 Reads file1, reformats, and writes file2
    
----------------------------------
PROGRAM: /exec/trajgrad
 Usage: trajgrad [file name]
    
----------------------------------
PROGRAM: /exec/trajmean
 USAGE: trajplot -[options (default)]
   -d[Check for dateline 0-no (1)-yes]
   -i[Input files: name1+name2+... or +listfile or (tdump)]
   -o[Output file name: (tmean)]
   -p[Process file name suffix: (ps) or process ID]
   -v[Vertical: 0-pressure (1)-agl]
   
 NOTE: leave no space between option and value
    
----------------------------------
PROGRAM: /exec/trajmerg
 Usage: trajmerg [file1] [file2] [file3]
 Merges trajectories in file1 and file2 into file3
    
----------------------------------
PROGRAM: /exec/trajplot
 USAGE: trajplot -[options (default)]
   -a[GIS output: (0)-none 1-GENERATE 3-Google Earth]
   -e[End hour to plot: #, (all) ]
   -f[Frames: (0)-all files on one  1-one per file]
   -g[Circle overlay: ( )-auto, #circ(4), #circ:dist_km]
   -h[Hold map at center lat-lon: (source point), lat:lon]
   -i[Input files: name1+name2+... or +listfile or (tdump)]
   -j[Map background file: (arlmap) or shapefiles.<txt|suffix>]
   -k[Kolor: 0-B&W, (1)-Color, N:colortraj1,...colortrajN]
      1=red,2=blue,3=green,4=cyan,5=magenta,6=yellow,7=olive
   -l[Label interval: ... -12, -6, 0, (6), 12, ... hrs 
      <0=with respect to traj start, >0=synoptic times)]
   -L[LatLonLabels: none=0 auto=(1) set=2:value(tenths)]
   -m[Map proj: (0)-Auto 1-Polar 2-Lambert 3-Merc 4-CylEqu]
   -o[Output file name: (trajplot.ps)]
   -p[Process file name suffix: (ps) or process ID]
   -s[Symbol at trajectory origin: 0-no (1)-yes]
   -v[Vertical: 0-pressure (1)-agl, 2-theta 3-meteo 4-none]
   -z[Zoom factor:  0-least zoom, (50), 100-most zoom]
   
 NOTE: leave no space between option and value
    
----------------------------------
PROGRAM: /exec/unpacker
 Decodes all records of a GRIB1 file
    
----------------------------------
PROGRAM: /exec/velvar
 Creates a time series of velocity variance and diagnostic values
    
----------------------------------
PROGRAM: /exec/vmixing
 Creates a time series of meteorological stability parameters
  
 USAGE: vmixing (optional arguments)
 -p[process ID]
 -s[KBLS - stability method]
 -t[KBLT - PBL mixing scheme]
    
----------------------------------
PROGRAM: /exec/volcplot
 USAGE: volcplot -[options (default)]
 
   -1[Run:           0-NCEP run 1-NCEP hypothetical
                     2-Web run  3-Web hypothetical
                     4-other ]                          REQUIRED
   -2[Alert Type:    -1-Test 0-None 1-Update 2-Ended  3-Watch]
                      4-Web  5-hypothetical             REQUIRED
   -3[Ash Reduction: 0-None 1-Small  2-Medium 3-Large]  REQUIRED
   -4[Comment:       0-None 1-Ash column top estimated] REQUIRED
   -5[Volcano Name: (no blanks, 16-character max)]      REQUIRED
   -6[Eruption duration (hours)]                        REQUIRED
   -a[Arcview GIS: (0)-no dump, 1-dump to files]
   -i[Input concentration file name: (cdump)]
   -j[Graphics map background file: (arlmap) or shapefiles.txt]
   -k[Color:  0-B&W (1)-Color]
   -l[Label options: ascii code, (73)-open star]
   -L[LatLonLabels: none=0 auto=(1) set=2:value(tenths)]
   -m[Map projection: (0)-auto, 1-polar, 2-lambert, 3-mercator, 4-CylEqu]
   -n[Number of time periods: (0)-all, numb, min:max, -incr]
   -o[Output postscript file name: (volcplot.ps)]
   -p[Process file name suffix: (ps) or process ID]
           process ID REQUIRED for option -24 web run
   -w[Web URL where this is run... REQUIRED for option -24...]
                        ... (e.g. http://www.arl.noaa.gov/READY.php)
   -z[Zoom factor: 0-least zoom, (50), 100-most zoom]
   
 NOTE: leave no space between option and value
    
----------------------------------
PROGRAM: /exec/xtrct_grid
 Extracts a subgrid from a larger domain meteorological file
    
----------------------------------
PROGRAM: /exec/xtrct_stn
 Creates a time series of meteorological variables from file
 interpolated to a specific lat-lon point
 Usage: xtrct_stn [-i{nteractive}]
    
----------------------------------
PROGRAM: /exec/xtrct_time
 PROGRAM TO EXTRACT A SELECTED NUMBER OF TIME PERIODS FROM
 A METEOROLOGICAL DATA FILE. THE [-d] DELETE OPTION CAN BE
 USED TO CHANGE THE PROGRAM TO DELETE RATHER THAN EXTRACT
 THE SELECTED TIME PERIOD. THE SKIP [-s] OPTION ONLY SKIPS
 DUPLICATE TIME PERIODS MATCHING THE DAY HOUR [DDHH].
 Usage: -p[process_id] -s[skip DDHH] -d[delete] -h[help]

---------------------------------------
PROGRAM:  /exec/conavg
  Reads multiple HYSPLIT concentration files and computes the mean
at each grid point and then writes a new output file

 Usage: conavg [-options]
   -b[base input file name (cdump)]
   -d[dignostic output turned on]

-----------------------------------------------------------------------

    
