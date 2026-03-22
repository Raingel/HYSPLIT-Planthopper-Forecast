#------------------------------------------------------------------
# Master Makefile for the ../cmaq/mcip2arl directory
# Last Revised: 21 Nov 2005 
#               21 Dec 2005 - put hysplit lib at end
#               09 Aug 2007 - support for gfortran
#               15 Jul 2010 - directory mod for repository
#               03 Nov 2010 - added conc2cdf (no IOAPI)
#               25 Aug 2014 - netCDF libraries previously exported
#               25 Jan 2018 - replaced conc2cdf with con2cdf3/con2cdf4
#               23 Aug 2019 - moved compiler options and library
#                             definitions to ../Makefile.inc.
#               26 Aug 2019 - redefined NETINC and NETLIBS.
#------------------------------------------------------------------

include ../Makefile.inc
# ../Makefile.inc should define the following variables:
# IOAPIINC	- compiler flag for the IOAPI include files
# IOAPILIBS	- linker flags for the IOAPI library
# NETINC	- compiler flag for the NetCDF include files
# NETLIBS	- linker flags for the NetCDF library

SHELL=/bin/sh
EXE=../exec

FFLAGS += $(FFLAGS_FREEFORM) $(IOAPIINC) $(NETINC)
LINKS = $(NETLIBS) -L../library -lhysplit

all : traj2api conc2api mcip2arl con2cdf3 con2cdf4

traj2api : traj2api.f 
	$(FC) -o $(EXE)/$@ $(FFLAGS) $@.f $(IOAPILIBS) $(LINKS) 
	rm -f *.o

conc2api : conc2api.f 
	$(FC) -o $(EXE)/$@ $(FFLAGS) $@.f $(IOAPILIBS) $(LINKS)  
	rm -f *.o

mcip2arl : mcip2arl.f 
	$(FC) -o $(EXE)/$@ $(FFLAGS) $@.f $(IOAPILIBS) $(LINKS) 
	rm -f *.o

con2cdf3 : con2cdf3.f 
	$(FC) -o $(EXE)/$@ $(FFLAGS) $@.f $(LINKS) 
	rm -f *.o

con2cdf4 : con2cdf4.f
	$(FC) -o $(EXE)/$@ $(FFLAGS) $@.f $(LINKS) 
	rm -f *.o

clean:
	rm -f $(EXE)/traj2api
	rm -f $(EXE)/conc2api
	rm -f $(EXE)/mcip2arl
	rm -f $(EXE)/con2cdf3
	rm -f $(EXE)/con2cdf4
