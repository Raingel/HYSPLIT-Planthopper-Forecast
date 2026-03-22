# Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002,
# 2003, 2004, 2005, 2006, 2007, 2008, 2009  Free Software Foundation,
# Inc.
# This Makefile.in is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.

# Makefile to be used with eccodes libraries

#------------------------------------------------------------------
# Makefile for the ../data2arl/api2arl data converter 
# This makefile is for use with the eccodes libraries.
# v1 - original converter for NCEP archive
# v2 - modified for JMA mesoscale data on hybrid coordinate
# v3 - revised for CTBTO ECMWF hybrid and NOAA pressure levels
#------------------------------------------------------------------
# Last Revised:
# 26 Aug 2019 - use a Makefile.inc. 
# 24 Apr 2023 - add api2arl_v6

include ../../Makefile.inc

EXE=../../exec/

LIBHYS = -L../../library -lhysplit

FCFLAGS = -g $(FFLAGS) $(FFLAGS_FREEFORM)
#               12 Feb 2013 - version 4 GSD HRRR

api2arl_DEPENDENCIES =  \
	$(ECCODES_TOPDIR)/lib/x86_64-linux-gnu/libeccodes_f90.so \
	$(ECCODES_TOPDIR)/lib/x86_64-linux-gnu/libeccodes.so
FCCOMPILE = $(FC) $(ECCODESINC) $(FCFLAGS)
FCLD = $(FC)
FCLINK = $(FCLD) $(FCFLAGS) $(AM_LDFLAGS) \
	 $(LDFLAGS) -o $(EXE)/$@

.SUFFIXES:
.SUFFIXES: .f .lo .o .obj

all : api2arl_v1 api2arl_v2 api2arl_v3 api2arl_v4 api2arl_v6 apidump

api2arl_v1: api2arl_v1.o $(api2arl_DEPENDENCIES) 
	@rm -f api2arl_v1
	$(FCLINK) api2arl_v1.o $(ECCODESLIBS) $(LIBHYS)

api2arl_v2: api2arl_v2.o $(api2arl_DEPENDENCIES) 
	@rm -f api2arl_v2
	$(FCLINK) api2arl_v2.o $(ECCODESLIBS) $(LIBHYS)

api2arl_v3: api2arl_v3.o $(api2arl_DEPENDENCIES) 
	@rm -f api2arl_v3
	$(FCLINK) api2arl_v3.o $(ECCODESLIBS) $(LIBHYS)

api2arl_v4: api2arl_v4.o $(api2arl_DEPENDENCIES) 
	@rm -f api2arl_v4
	$(FCLINK) api2arl_v4.o $(ECCODESLIBS) $(LIBHYS)

api2arl_v6: api2arl_v6.o $(api2arl_DEPENDENCIES)
	@rm -f api2arl_v6
	$(FCLINK) api2arl_v6.o $(ECCODESLIBS) $(LIBHYS)

apidump: apidump.o $(api2arl_DEPENDENCIES) 
	@rm -f apidump
	$(FCLINK) apidump.o $(ECCODESLIBS) $(LIBHYS)

.f.o:
	$(FCCOMPILE) -c -o $@ $<  

clean :
	 rm -f $(EXE)/apidump
	 rm -f $(EXE)/api2arl_v1
	 rm -f $(EXE)/api2arl_v2
	 rm -f $(EXE)/api2arl_v3
	 rm -f $(EXE)/api2arl_v4
	 rm -f $(EXE)/api2arl_v6
	 rm -f *.o
