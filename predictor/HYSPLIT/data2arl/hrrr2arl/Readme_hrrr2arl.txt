HRRR2ARL

HRRR to ARL format converts HRRR GRIB files to a HYSPLIT
compatible format.

Requires the installation of the GRIB libraries:
   grib_api   - for compiling hrrrv12arl_v1.f and
                hrrrv12arl_v2.f
   NCEP grib2 - for compiling hrrr2arl.f and hrrrv1arl.f


This directory contains four FORTRAN files:

hrrr2arl.f      - converter for the current HRRR 3 km
                  sigma model.

hrrrv12arl_v1.f - converter for the ARL's hrrr.v1 archive.
                  applicable up to the 2016-08-23 12z data.

hrrrv12arl_v2.f - converter for the ARL's hrrr.v1 arhives.
                  applicable from the 2016-08-23 13z data.

hrrrv12arl.f    - equivalent to hrrrv12arl_v1.f in the above.
                  kept for historical reasons.
