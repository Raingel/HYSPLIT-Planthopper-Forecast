---------------------------------
February 1, 2019

Most of these legacy codes are WMO grib edition 1 (grib1)
and were moved from ../../source to here for reference. Grib 
edition 2 (grib2) is now the standard at NCEP.  Many of these 
codes were for NCEP grib1 output (eta, avn, etc.).  

The Makefile creates executables in ../../exec.  None of the 
executables were tested. 

Below is moved from ../../source/Readme_source.txt.

----------------------------------
PROGRAM: /exec/afwa2arl
 Converts an eta model pressure grib file to arl packed format
 processes only one time period per execution. Multiple input files
 may be specified on the command line, each containing different
 variables required for the specific time period.  Input data are
 assumed to be already on a conformal map projection on model levels.
 Only packing to ARL format and units conversion is required.

 Usage: afwa2arl [file_1] [file_2] [...]
 Convert pressure eta grib file to arl format

----------------------------------
PROGRAM: /exec/avn2arl
 Converts global AVN model GRIB1 files to ARL format.
 Has been replaced by the more general program: grib2arl

 Usage: avn2arl [-options]
  -i[grib input file]
  -x[extract center longitude]
  -y[extract center latitude]
  -g[0:conformal 1:nhem 2:shem 3:global]
  -n[number of x,y extract grid points]

----------------------------------
PROGRAM: /exec/avn2gbl
 Usage" avn2arl [file] [clat] [clon]

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

-------------------------------------------------------------------------
PROGRAM: /exec/ecm2arl

Program to convert ECMWF data to ARL format based on grib2arl

Usage: ecm2arl [-options]
  -i[primary grib data: file name {required}]
  -s[supplemental grib data: file name {optional}]
  -e[supplemental grib data: file name {optional}]
  -c[constant grib data: file name {optional}]
  -d[distance (km) extract grid resolution {100.0}]
  -x[subgrid extract center longitude {-80.0}]
  -y[subgrid extract center latitude {60.0}]
  -g[output projection  0 :conformal extract
                        1 :fixed northern hemisphere polar
                        2 :fixed southern hemisphere polar
                       {3}:lat-lon global grid (as input)
                        4 :lat-lon extract grid
  -n[number of (x:y) extract grid points {100}]
  -k[number of output levels including sfc {20}]
  -p[surface defined by {1}:pressure or 0:terrain height]
  -q[analyze grib file {0} or use saved configuration: 1]
  -r[rain fall accumulation time hours: default is 12 for ECMWF,6 for other cent
 ers.]
  -t[the number of time periods to process: {744}]
  -z[zero initialization of output file 0:no {1}:yes]

----------------------------------
PROGRAM: /exec/eta04arl
 Converts 4 km eta model pressure grib file to arl format

 Usage: eta04arl [file_1] [file_2] [...]
 Convert pressure 4 km eta grib file to arl format

----------------------------------
PROGRAM: /exec/eta12arl
 Converts eta model pressure grib file (12 km, 218) to arl format

 Usage: eta12arl [file_1] [file_2] [...]
 Convert pressure 12 km eta grib (grid 218) file to arl format

----------------------------------
PROGRAM: /exec/gdas2arl
 Usage: gfs2arl [-options]
  -i[grib input file]
  -x[extract center longitude]
  -y[extract center latitude]
  -g[0:conformal 1:nhem 2:shem 3:global]
  -n[number of x,y extract grid points]
  -z[zero initialization of output 0:no {1}:yes]

----------------------------------
PROGRAM: /exec/gfs2arl
 Usage: gfs2arl [-options]
  -i[grib input file]
  -x[extract center longitude]
  -y[extract center latitude]
  -g[0:conformal 1:nhem 2:shem 3:global]
  -n[number of x,y extract grid points]
  -z[zero initialization of output 0:no {1}:yes]

----------------------------------
PROGRAM: /exec/gfslr2arl
 Usage: gfs2arl [-options]
  -i[grib input file]
  -x[extract center longitude]
  -y[extract center latitude]
  -g[0:conformal 1:nhem 2:shem 3:global]
  -n[number of x,y extract grid points]
  -z[zero initialization of output 0:no {1}:yes]

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
PROGRAM: /exec/grib2arl
 Usage: grib2arl [-options]
  -i[primary grib data: file name {required}]
  -s[supplemental grib data: file name {optional}]
  -c[constant grib data: file name {optional}]
  -x[subgrid extract center longitude {-80.0}]
  -y[subgrid extract center latitude {60.0}]
  -g[output projection  0 :conformal extract
                        1 :fixed northern hemisphere polar
                        2 :fixed southern hemisphere polar
                       {3}:lat-lon global grid (as input)
                        4 :lat-lon extract grid
  -n[number of (x:y) extract grid points {100}]
  -k[number of output levels including sfc {16}]
  -p[surface defined by {1}:pressure or 0:terrain height]
  -q[analyze grib file {0} or use saved configuration: 1]
  -t[the number of time periods to process: {744}]
  -z[zero initialization of output file 0:no {1}:yes]

----------------------------------
PROGRAM: /exec/meds2arl
 Usage: meds2arl [-options {default}]
  -i[Input MEDS file name {meds.bin}]
  -t[Input MEDS terr name {meds.fix}]
  -o[Output ARL file name {meds.arl}]
  -r[Resolution MEDS data degs {1.0}]

----------------------------------
PROGRAM: /exec/nam40arl
 Convert pressure 40-km nam grib file to arl format
 Usage: nam2arl [file_1] [file_2] [...]

----------------------------------
PROGRAM: /exec/nams2arl
 Converts output from the nonhydrostactic mesoscale model to hysplit
 format. Input grib data are on the native model staggered Arakawa
 e-grid and the hybrid vertical coordinate.

 Usage: nams2arl [-options]
  -i[primary grib data: file name {required}]
  -s[supplemental grib data: file name {optional}]
  -g[grid: {none}=aqm; 0,0,0,0=full;
     lower left lat,lon, upper right lat,lon]
  -p[precip options:  p=previous, {d}=difference]
  -v[vertical resolution: {1}=cmaq, 2=<700 3=full 4=full+]
  -x[extract: {1}=all, 2=2D variables]

----------------------------------
PROGRAM: /exec/ncr2arl
 Converts ncar/ncep reanalysis grib files to arl format
 Usage: ncr2arl [file] [clat] [clon]

----------------------------------
PROGRAM: /exec/nmm2arl
 Converts output from the nonhydrostactic mesoscale model
 Replaced by nams2arl
 Usage: nmm2arl [file_1] [file_2] [...]

----------------------------------
PROGRAM: /exec/pNA05
 Usage: pmm52arl [file_1] [file_2] [...]
 Convert 15 km pressure mm5 grib file to arl format
 File name: us057g1010t02v${cyc}000${fhr}00.gz

----------------------------------
PROGRAM: /exec/pNA15
 Usage: pmm52arl [file_1] [file_2] [...]
 Convert 15 km pressure mm5 grib file to arl format
 File name: us057g1010t02v${cyc}000${fhr}00.gz

----------------------------------
PROGRAM: /exec/pNA45
 Usage: pNA45 [file_1] [file_2] [...]
 Convert 45 km pressure mm5 grib file to arl format
 File: us057g1010t02u${cyc}000${fhr}00.gz

----------------------------------
PROGRAM: /exec/rsmp2arl
 Usage: rsmp2arl [file_1] [file_2] [...]
 Convert pressure rsm grib file to arl format

----------------------------------
PROGRAM: /exec/rsms2arl
 Usage: rsms2arl [file_1] [file_2] [...]
 Converts an rsm model sigma grib file to arl format

----------------------------------
PROGRAM: /exec/ruc2arl
 Usage: ruc20arl [file_1] [file_2] [...]
 Convert 20 km pressure ruc grib file to arl format



 
