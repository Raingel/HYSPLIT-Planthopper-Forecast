HYSPLIT Installation Notes:

================================================================
Lionel Roubeyrie - 2 May 2010

There is a conflicting type issue between the utils.h provided 
and the stdio.h 2.6 linux kernels for the "getline" function.
Changing "getline" to "parseline" in utils.h/utils.c/text2dbf.c
solves this problem.

================================================================
R Potts - 19 Oct 2004

Generation of GIS shapefiles

Hysplit supports the generation of ESRI ‘generate’ format files. 
These can  be converted to ESRI shapefiles. Refer to ESRI_Shapefile_TechDesciption.199807.pdf 
for technical details on shapefiles. 

The ‘-a’ option for concplot will create a file GIS*.txt and GIS*.att. 
The tar file includes some example output files:
GIS_01001-16800_ps_0?.txt, GIS_01001-16800_ps_0?.att.

To generate the shape files
../exec/ascii2shp GIS_01001-16800_ps_01 polygons < GIS_01001-16800_ps_01.txt

This will generate 3 files
GIS_01001-16800_ps_01.dbf - dBASE table
GIS_01001-16800_ps_01.shp - main data file
GIS_01001-16800_ps_01.shx -index file 

At this stage the dbf file will have limited details regarding attributes. 
To add the attributes to the dbf file:
../exec/txt2dbf -v -R7.3 -C4 -I8 -I4 -I5 -I5 -d','  \
    GIS_01001-16800_ps_01.att  GIS_01001-16800_ps_01.dbf

dbfdump then returns the following:
dbfdump GIS_01001-16800_ps_01.dbf
   CONC NAME     DATE TIME LLEVEL HLEVEL
-17.000 VOLC 20041017 1300  1000  16800
-16.000 VOLC 20041017 1300  1000  16800
-15.000 VOLC 20041017 1300  1000  16800
-14.000 VOLC 20041017 1300  1000  16800

gen2shp
(http://www.intevation.de/~jan/gen2shp)
This package is the basis for the ascii2shp that is available in Hysplit. 

dBase Tools
(http://www.usf.uni-osnabrueck.de/~fkoorman/software/dbftools.en.html)
This package has two applications txt2dbf and dbf2txt that allow the conversion of ASCII tables
into dbase files and vice versa. 

txt2dbf - Conversion of ASCII tables into dbase files. 
txt2dbf -v -R7.3 -C4 -I8 -I4 -I5 -I5 -d','  text.att dbf_file

dbf2txt - Conversion of dbase4 files into ASCII tables. 
