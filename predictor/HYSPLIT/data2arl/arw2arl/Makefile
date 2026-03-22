#------------------------------------------------------------------
# Makefile for the ../data2arl/arw2arl data converter 
# Last Revised: 28 Jan 2007 
#               09 Aug 2007 - support for gfortran
#               16 May 2013 - use netcdff for version 4
#               25 Aug 2014 - netCDF libaries previously exported
#               23 Aug 2019 - moved compiler options and library
#                             definitions to ../../Makefile.inc
#               26 Aug 2019 - redefined NETINC and NETLIBS.
#------------------------------------------------------------------

include ../../Makefile.inc
# ../Makefile.inc should define the following variables:
# NETINC	- compiler flag for the NetCDF include files
# NETLIBS	- linker flags for the NetCDF library

SHELL = /bin/sh
PGM = arw2arl
EXE = ../../exec
LIB = libarw.a

LINKS = $(NETLIBS) -L../../library -lhysplit

LFLAGS = -vr

CODE = cfgrec.f \
       cfgvar.f \
       get1dv.f \
       get2dv.f \
       get3dv.f \
       get4dv.f \
       getdim.f \
       setmap.f \
       setvar.f 

all : $(PGM) 

$(LIB) : $(CODE)
	for member in $?; do make member MODULE=$$member;done

member : $(MODULE)
	$(FC) -c $(FFLAGS) $(FFLAGS_FREEFORM) $(NETINC) $?
	$(AR) $(LFLAGS) $(LIB) $(?F:.f=.o)
	ranlib $(LIB)
	rm -f $(?F:.f=.o)

$(PGM) : $(PGM).f $(LIB) 
	$(FC) -o $(EXE)/$@ $(FFLAGS) $(FFLAGS_FREEFORM) $(NETINC) $? $(LINKS)
	rm -f $(LIB)

clean :
	rm -f $(EXE)/$(PGM)  
	rm -f $(LIB)
