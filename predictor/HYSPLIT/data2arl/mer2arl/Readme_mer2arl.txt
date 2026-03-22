MER2ARL

MERra2 to ARL format converts MERRA2 data in NetCDF4 file
to a HYSPLIT compatible format.

Required library
NetCDF version 4 (use -lnetcdff)

MERRA2 download website:
   https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2
   https://goldsmr5.gesdisc.eosdis.nasa.gov/data/MERRA2

MERRA2 files: 
   MERRA2_100.tavg3_3d_asm_Nv.YYYYMMDD.nc4
   ----------                          ---
    prefix                             suffix
   MERRA2_100.tavg1_2d_flx_Nx.YYYYMMDD.nc4
   MERRA2_100.tavg1_2d_slv_Nx.YYYYMMDD.nc4
   MERRA2_100.tavg1_2d_int_Nx.YYYYMMDD.nc4

_______________________________________________
Notes:

Running script - run.mer2arl
The script downloads MERRA2 data using wget and executes mer2arl convert. 
It does not keep MERRA2 netcdf files once ARL files are generated. 

Downloading MERRA2 data
- Register on https://urs.earthdata.nasa.gov/
- Follow the instruction to set up wget with URS authentication
  https://disc.gsfc.nasa.gov/data-access

Since different streams were used in the data processing, the change between the original streams
occurred after one full year of spin up time. first file in each data stream is then:
MERRA2_100.*.19800101.nc4
MERRA2_200.*.19920101.nc4
MERRA2_300.*.20010101.nc4
MERRA2_400.*.20110101.nc4

Hybrid sigma-pressure vertical coordinate
Hybrid Ap and Bp parameter are listed in METLAY.INC
http://wiki.seas.harvard.edu/geos-chem/index.php/GEOS-Chem_vertical_grids#72-layer_vertical_grid

