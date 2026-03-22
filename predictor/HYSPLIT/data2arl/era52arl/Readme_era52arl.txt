!-------------------------------------------------------------
! ERA52ARL - Converts ECMWF ERA5 data using ECMWF ecCodes library
! routines to HYSPLIT packed (ARL) data format.
!-------------------------------------------------------------
! Last Revised 10/24/2017

Install ECMWF eccodes library prior to compiling era52arl.

! Input files 
! grib1 file with pressure levels
! grib1 file with analysis surface fields
! grib1 file with forecast surface fields
! api2arl.cfg file detailing fields in the grib files
! ...
!
!-------------------------------------------------------------
!This program will currently only convert ERA5 data on the pressure levels.
!Pressure level data is in grib1 format.

!ERA5 is also available on model levels. There are 137 model levels
!model level data is output in grib2 format.

!Pressure levels to use in the ARL file are specified in the api2arl.cfg file.

 Usage: era52arl [-options]
 The pressure level file and surface analysis file must have
 the same time periods. The surface forecast file should have
 all the time periods that the analysis files do but they do
 not have to match. Step times of 0 in the forecast file will be
 ignored since accumulated fields are not available for 0 step.
    
 a default era52arl.cfg will be created if none exists or
 alternate name is not specified with the -d option.
 This file specifies variables and pressure levels to be written.

 OPTIONS:
  -d[decoding configuration file {name | create era52arl.cfg}]
  -i[input grib1 file with pressure level fields name {DATA.GRIB}]
  -a[input grib1 analysis surface fields name {SFC.GRIB}]
  -f[input grib1 forecast surface fields name {SFC2.GRIB}]
  -o[output data file name {DATA.ARL}]
  -v[verbose mode {False}]



DEFAULT era52arl.cfg file shown below.
If variables or pressure levels are not available in the grib file then they will not be packed into the
ARL file.
If variables or pressure levels are not specified in the cfg file but are available in the grib file they will NOT
be packed into the ARL file.

This file defines the relationship between the grib file variable names and the ARL packed format variable names.
The atmgrb and atmcat fields define the grib names/codes for the variables and
the atmarl defines the corresponding HYSPLIT variable.

For the surface variables the sfcgrb and sfccat define the grib file variable names/codes and the
sfarl define the corresponding HYSPLIT variable.

HYSPLIT will assume that the variables are matched appropriatley. It is possible to do odd things such as convert the
grib u wind field into the HYSPLIT w wind field. Please take care when modifying this file.

&SETUP
 numatm = 6,
 atmgrb = 'z','t','u','v','w','r',
 atmcat =      129 ,   130 ,    131 ,   132 ,   135 ,    157 ,
 atmnum =      129 ,   130 ,    131 ,   132 ,   135 ,    157 ,
 atmcnv =     0.102 ,  1.0 ,   1.0 ,   1.0 ,  0.01,   1.0 ,
 atmarl = 'HGTS','TEMP','UWND','VWND','WWND','RELH',
 numsfc = 14,
 sfcgrb = '2t','10v','10u','tcc','sp','2d','blh','cape','z','tp','sshf','ssrd','slhf','zust',
 sfccat =   167,   166,  165,  164, 134, 168, 159, 59,  129, 228, 146, 169, 147, 3
 sfcnum =   167,   166,  165,  164, 134, 168, 159, 59,  129, 228, 146, 169, 147, 3
 sfccnv =   1.0, 1.0, 1.0,  1.0, 0.01 ,1.0, 1.0, 1.0 ,0.102, 1.0, 0.00028, 0.00028, 0.00028, 1.0
 sfcarl = 'T02M','V10M','U10M','TCLD','PRSS','DP2M','PBLH','CAPE','SHGT','TPP1','SHTF','DSWF','LTHF','USTR',
 numlev = 37
 plev = 1000, 975, 950, 925, 900, 875, 850, 825, 800, 775, 750, 700, 650, 600, 550, 500, 450, 400, 350, 300, 250, 225, 200, 175, 150, 125, 100, 70,  50, 30, 20, 10, 7, 5, 3, 2, 1 
/


