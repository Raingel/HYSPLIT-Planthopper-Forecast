DIRECTORY: ./hysplit
___________________________________________________

The HYSPLIT home directory contains all the sub-
directories with scripts, source code, and 
documentation. The default installation procedure
assumes the GNU gfortran compiler is available.  
Other compiler options can be set in Makefile.inc. 
Default compile flags for the intel compiler are 
in Makefile.inc.intel and can be copied to 
Makefile.inc. Initial compilation and installation 
of the code is done by copying Makefile.inc.gfortran
to Makefile.inc and typing make followed by make 
install (see top level Makefile).

Subsequent changes to either library routines or
main programs just require running make in the 
appropriate directory.

Note that only the basic set of executable programs
will be created. If additional functionality is 
required then specific subdirectories need to be
extracted from their tar files and compiled. See
the following directories for more information:

./cmaq		- cmaq interface requires IOAPI
./data2arl	- decoders requiring NetCDF
./gisprog	- shapefile conversions

Some of these programs require the installation of
additional libraries, such as NetCDF, IOAPI, or GRIB2.

To insure that the model has compiled corrected, go to
the ./testing directory and execute ./xrun.scr. This
script will sequence through a variety of simulations
and the output graphics from each will be appended to
a file called results.ps. These graphics can then be
compared, frame-by-frame, with results.pdf.  The xrun
script can also be used as a prototype to configure other
simulations, some of which are not disucssed in the 
User's Guide.

For non-tagged versions, the update.sh script may be run
to update the version message shown at the beginning of each
HYSPLIT run. The script should be run before compiling.
The update.sh script also provides some
basic options for exporting and checking out from the 
repository.
