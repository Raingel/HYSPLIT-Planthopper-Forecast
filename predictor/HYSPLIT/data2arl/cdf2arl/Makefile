#------------------------------------------------------------------
# Makefile for the ../data2arl/cdf2arl data converter 
# Last Revised: 28 Jan 2007 
#               09 Aug 2007 - support for gfortran
#               16 May 2013 - use netcdff for version 4
#               25 Aug 2014 - netCDF libaries previously exported
#               26 Aug 2019 - use a Makefile.inc.
#------------------------------------------------------------------

include ../../Makefile.inc

SHELL = /bin/sh

PGM=cdf2arl
EXE=../../exec/
LIB=libarw.a

LINKS = $(NETLIBS) -L../../library -lhysplit

LFLAGS = -vr

CODE = get2dv.f \
       get3dv.f \
       get4dv.f \
       getdim.f \
       gaussl.f \
       intp2d.f \
       makndx.f \
       mkgrid.f \
       ncjulh.f \
       regrid.f \
       tmcovt.f \
       setndx.f \
       setxy.f  \
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
