Programs to convert GRIB2 format data into ARL HYSPLIT format
using the ECMWF ecCodes library (formerly used the ECMWF grib_api library)
-----------------------------------------------------------------------
Last Revised: 24 Apr 2023
-----------------------------------------------------------------------

api2arl_v1 : original version processes pressure level data
             from the NCEP NOMADS server

api2arl_v2 : modified to handle the hybrid sigma coordinate 
             on a Lambert Conformal projection and where the
             vertical index increases with height

api2arl_v3 : revised to handle 0.5 degree data archived by
             from NOAA (pressure) and ECMWF (hybrid) and
             where the vertical index decreases with height

api2arl_v4 : revised to handle GSD HRRR data on pressure  
             level surfaces; uncomment sections for precipitation
             accumulations at intervals different than one hour

api2arl_v6 : similar to api2arl_v4, except reads in 
             'scaledValueOfFirstFixedSurface' for pressure level 
             instead of 'level'. For GDAS 0.5 degree files, 
             'scaledValueOfFirstFixedSurface' units are always in 
             Pa, while 'level' is in mb from the surface to 1 mb 
             and Pa above 1 mb.

Note that downward compatibility has not been tested on all versions.

======================================================================
Install various libraries prior to installing ECMWF grib_api before
compiling the ARL grib decoder. Note that some meteorological centers
may use other compression routines besides the default routines
recommended by ECMWF. 

10/27/2017
The api2arl utilities were updated to use eccodes library which is replacing the grib_api
libraries provided by ECMWF. 

Installation instructions for ecCodes can be found at
https://software.ecmwf.int/wiki/display/ECC/ecCodes+installation.

If you would still like to use the grib_api libraries, then
simply change the "use" statement at the beginning of the code from use eccodes to use grib_api.
Then use Makefile_api1.8 or Makefile_api1.9 to compile the code. 

======================================================================
Instructions below on installing grib_api are no longer needed unless the user has
some reason to use grib_api over eccodes.

1.2 grib_api installation

The grib_api installation is based on the standard configure utility. 
It is tested on several platforms and with several compilers. However for some
platforms modifications to the installation engine may be required.
If you encounter any problem during the installation procedure please send 
an e-mail with your problem to Software.Services@ecmwf.int.

The only required package for a standard installation is jasper which enables
the jpeg2000 packing/unpacking algorithm. It is possible to build grib_api 
without jasper, by using the –disable-jpeg configure option, but to install 
a fully functional library, its download is recommended.

1.2.1 Standard installation

1. Download grib_api from 
http://www.ecmwf.int/products/data/software/download/grib_api.html
########
The api2arl program has been tested with grib_api version 1.8 and 1.9

########

2. Unpack distribution:
> gunzip grib_api-X.X.X.tar.gz
> tar xf grib_api-X.X.X.tar

3. Create the directory where to install grib_api say grib_api_dir
> mkdir grib_api_dir

4. Run the configure in the grib_api-X.X.X
> cd grib_api-X.X.X
> ./configure --prefix=grib_api_dir

5. make, check and install
> make
...
> make check
...
> make install
...

========================================================================

compiling and linking api2arl

There are two Makefile files available depending on the grib_api version you installed (1.9.18 on uses libtool):

for version 1.8:

> make -f Makefile_api1.8 api2arl

for version 1.9.18 on:

> make -f Makefile_api1.9 api2arl

========================================================================





 Usage: api2arl [-options]
  -h[help information with extended discussion]
  -e[encoding configuration file {name | create arldata.cfg}]
  -d[decoding configuration file {name | create api2arl.cfg}]
  -i[input grib data file name {DATA.GRIB2}]
  -o[output data file name {DATA.ARL}]
  -g[model grid name (4 char) default = {center ID}]
  -s[sigma compute=1 or read=2 or ignore=(0)]
  -t[top pressure (hPa) or level number for processing {20}]
  -a[averge vertical velocity no=(0) or yes=numpts radius]
  -z[zero fields (no=0 yes=1)initialization flag {1}]
  
 The API2ARL program converts model output data in GRIB2 format
 to the ARL packed format required for input to HYSPLIT. The GRIB
 data must be a global latitude-longitude or regional Lambert grid
 defined on pressure surfaces. The program will only convert one
 time period in any one input file. Multiple time period output
 files can be created by appending each time period processed using
 the cat command (e.g. cat DATA.ARL_t2 >> DATA.ARL_t1).
  
 The GRIB conversion is defined by the decoding configuration file
 which defines the relationship between the grib file variable names,
 the ARL packed format variable names, and the units conversion factors.
 The default name for this file is api2arl.cfg and if it does not exist,
 the file will be created. It can be subsequently edited to define a
 customized set of variables. Each user must know which variables are
 available in the GRIB input file. The current default configuration
 defines the following GRIB variables:
 levelType  level shortName
 pl         {hPa} gh t u v r w
 sfc        0     sp orog tp lhtfl shtfl uflx vflx dswrf hpbl tcc
 sfc        2     2t r
  
 The GRIB messages are scanned and an encoding configuration file is
 created (arldata.cfg) that defines the record structure for packing
 into the ARL format. Variables defined in api2arl.cfg that are not
 found in the GRIB file will not be defined in arldata.cfg. An old file
 may be used if the current configuration does not contain all the fields
 (e.g. fluxes) and those records are to be filled during another pass
 through the program with a different GRIB file. Note that all variables
 passed to the packing routines must be defined in this file, but not all
 defined variables have to be written during any one pass.
  
 Two pass processing is accomplished by turning off the initialization
 flag (-z0) during the second pass. In this way missing fields are not
 written into an existing output file (DATA.ARL). The example below shows
 how this process can be scripted assuming a new forecast is available
 every six hours and flux fields are not available at the initial time.
  
 rm -f DATA.ARL ARLDATA.BIN arldata.cfg api2arl.cfg
 ./api2arl -z1 -iCYCLE06Z_FCST06.GRIB
 ./api2arl -z0 -iCYCLE12Z_FCST00.GRIB
 mv DATA.ARL ARLDATA.BIN
 for HH in 03 06 09 12; do
 ./api2arl -z1 -iCYCLE12Z_FCST${HH}.GRIB
 cat DATA.ARL >>ARLDATA.BIN
 rm -f DATA.ARL
 done


------------------------
Version_2 notes:

1) Create sigma.txt file by processing through all the levels
   and using the average pressure at each level and the surface
   to compute an equivalent sigma value for the packing
   configuration file. The sigma value is only needed for 
   many of the auxiliary programs, HYSPLIT will use the 3D
   pressure field instead of computing a value from the sigma.

   rm *.cfg
   api2arl -idata.grib2 -t41 -s1

2) Create arldata.cfg with sigma levels reads the sigma.txt file
   created in the previous step to write a new arldata.cfg file
   with the simga levels replacing the model level index number.

   rm *.cfg
   api2arl -idata.grib2 -t41 -s2

3) Edit arldata.cfg and api2arl.cfg to remove unwanted fields
   such as the 3D height field. PRES should always occur before
   THET so that HYSPLIT can convert THET to ambient. Also rename
   the default precipitation accumulation from TPP6 to TPP3.

   api2arl -idata.grib2 -t41


-------------------------
Version_3 notes:

   ECMWF: api2arl -iEN12082515 -t40
   NOAA:  api2arl -iGD12082515 -t100


-------------------------
Version_4 notes:

Uncomment as appropriate:
!# IF(if2-if1.EQ.3) sfcarl(kv)='TPP3'
!# IF(if2-if1.EQ.6) sfcarl(kv)='TPP6'


