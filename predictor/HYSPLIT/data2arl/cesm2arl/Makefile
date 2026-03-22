#------------------------------------------------------------------
# Makefile for the ../data2arl/cesm2arl data converter 
# Last Revised: 19 Jun 2020
#------------------------------------------------------------------

include ../../Makefile.inc
# ../Makefile.inc should define the following variables:
# NETINC	- compiler flag for the NetCDF include files
# NETLIBS	- linker flags for the NetCDF library

SHELL = /bin/sh
PGM = cesm2arl
EXE = ../../exec
LIB = libcesm.a

LINKS = $(NETLIBS) -L../../library -lhysplit

LFLAGS = -vr

CODE = cfgrec.f \
       cfgvar.f \
       get3dv.f \
       get4dv.f \
       getdim.f \
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
