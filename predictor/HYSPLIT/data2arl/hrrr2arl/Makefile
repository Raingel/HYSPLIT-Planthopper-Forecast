#------------------------------------------------------------------
# Makefile for the ../data2arl/hrrr2arl data converter 
# Last Revised: 18 Mar 2022

include ../../Makefile.inc
# ../Makefile.inc should define the following variables:
# GRIB_INC  - compiler flag for the GRIB2 include files
# GRIB_LINK - linker flags for the GRIB2 and its dependent libraries

EXE=./

LIBHYS = -L../../library -lhysplit

FCFLAGS = -g $(FFLAGS_STRICT) $(FFLAGS_FREEFORM) -I$(GRIB_INC)

GRIB_API_DIR=${HOME}/grib_api/grib_api_dir
AM_FCFLAGS= -I$(GRIB_API_DIR)/include -I$(GRIB_API_DIR)/include/jasper -I$(GRIB_API_DIR)/lib/gfortran/modules
AM_LDFLAGS= $(GRIB_API_DIR)/lib/libgrib_api_f90.so $(GRIB_API_DIR)/lib/libgrib_api.so
api2arl_LDADD= $(AM_LDFLAGS)

.SUFFIXES:
.SUFFIXES: .f .lo .o .obj

build: hrrr2arl hrrrv12arl_v1 hrrrv12arl_v2

hrrr2arl: hrrr2arl.f
	$(FC) $(FCFLAGS) -o $(EXE)/$@ hrrr2arl.f $(LIBHYS) $(GRIB_LINK)

hrrrv12arl: hrrrv12arl.f
	$(FC) $(FCFLAGS) -o $(EXE)/$@ hrrrv12arl.f $(LIBHYS) $(GRIB_LINK)

hrrrv12arl_v1: hrrrv12arl_v1.f
	$(FC) $(FCFLAGS) $(AM_FCFLAGS) -o $(EXE)/$@ hrrrv12arl_v1.f $(api2arl_LDADD) $(LIBS) $(LIBHYS)

hrrrv12arl_v2: hrrrv12arl_v2.f
	$(FC) $(FCFLAGS) $(AM_FCFLAGS) -o $(EXE)/$@ hrrrv12arl_v2.f $(api2arl_LDADD) $(LIBS) $(LIBHYS)

clean :
	 rm -f hrrr2arl hrrrv12arl hrrrv12arl_v1 hrrrv12arl_v2
	 rm -f *.o
