DIRECTORY: ./data2arl			
___________________________________________________________________

This directory contains various directories with example programs to 
convert meteorological data in various formats to the format (ARL 
packed) that Hysplit can read. The use of the packing routines is 
described in the User's Guide.  Routines may require some customiztion
depending upon the structure of the input data files.  

Briefly a ARL packed data file consists of a series of fixed length 
records, one for each meteorological variable. The records are 
arranged in time series, with surface fields followed by upper air
fields.  Each record contains a 50 byte ascii header that provides
information about the date, time, variable, and packing constants.
This is followed by the packed binary data, one byte per grid point.
Each byte represents the difference in value between that point and
the previous grid point. A group of records at the same time is 
preceeded by an index record that describes all the variables, levels,
and grid information for that time period.

The programs in this directory tend to be self contained or rely
upon the installation of other packages such as NetCDF. Originally, 
other conversion programs, that rely upon the subroutines in the 
hysplit library, were in the source directory, however these have been 
moved to the legacy subdirectory, including the sample program 
dat2arl.f.  

--------------------------------------------------------------------

mm5v3.tar
	Converts MM5 Version 3 DOMAIN files.

rams5.tar
	Reads history files from the RAMS model version 5.

rams6.tar
	Reads history files from the RAMS model version 6.
	Requires the installation of version 5 software.

arw2arl
	Converts WRF-ARW NetCDF output files and requires the
	installation of the NetCDF library (version 3 or 4).

api2arl
	Converts lat-lon files encoded in GRIB2 but requires
	the ECMWF grib_api libraries, which can be obtained from:
	http://www.ecmwf.int/products/data/software/download/grib_api.html

cdf2arl 
	Converts NCEP/NCAR Reanalysis data in NetCDF4 file to a HYSPLIT compatible format.
	Requires NetCDF library version 4.

era2arl 
	Converts ERA5 data to ARL format. Requires ECMWF eccodes library.

legacy
	Legacy codes to convert various meteorology model output to 
	HYSPLIT compatible format.  

mer2arl
	Converts MERRA2 (NASA) meteorological data to ARL format. 
	Requires NetCDF library version 4.

narr2arl
	Converts NARR meteorological data to ARL format.  

cesm2arl
        Converts CESM NetCDF output files and requires the
        installation of the NetCDF library (version 3 or 4).
