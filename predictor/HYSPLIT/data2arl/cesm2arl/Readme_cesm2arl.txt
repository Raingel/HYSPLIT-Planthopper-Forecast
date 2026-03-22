CESM2ARL

Community Earth System Model to ARL format converts CESM
NetCDF files to a HYSPLIT compatible format. When the input
file contains data for a single time period, then the ARL format
output file from each execution should be appended (cat >>) to
the output file from the previous time periods execution.

Requires the installation of the NCAR NetCDF libraries!
NetCDF version 3 use -lnetcdf
NetCDF version 4 use -lnetcdff

_______________________________________________
Notes:

- HYSPLIT assumes CESM met fields are time averaged.

- CESM is based on a no leap year calendar, while HYSPLIT 
includes leap days. A couple options for running HYSPLIT 
with meteorology from CESM output:
1) Copy an ARL file from another day and change the internal 
date in the file to the leap day using edit_head utility program.
2) Change the internal year in an ARL file from a leap year to a 
non-leap year using the edit_head utility program. 

_______________________________________________
Sample script for copying CESM file for Feb 28 and changing the 
internal date in the file to Feb 29:

#!/bin/sh
cp CESM_Feb28.ARL CESM_Feb29.ARL
../exec/edit_head  -i << EOF
{directory path to ARL file}/
CESM_Feb29.ARL
00 02 28 -1 -1
00 02 29 -1 -1
EOF

_______________________________________________
Sample script for changing all internal dates in an ARL file 
from year 00 to year 01:

#!/bin/sh
../exec/edit_head  -i << EOF
{directory path to ARL file}/
CESM_2000Feb28.ARL
00 -1 -1 -1 -1
01 -1 -1 -1 -1
EOF

