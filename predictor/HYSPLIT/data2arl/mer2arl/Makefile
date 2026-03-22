#------------------------------------------------------------------
# Makefile for the ../data2arl/mer2arl data converter 
# Last Revised: 10 May 2017 
#               26 Aug 2019 - use a Makefile.inc
#------------------------------------------------------------------

include ../../Makefile.inc

SHELL = /bin/sh

PGM=mer2arl
EXE=../../exec/
LIB=libarw.a

LINKS = $(NETLIBS) -L../../library -lhysplit

LFLAGS = -vr

CODE = cfgvar.f \
       cfgrec.f \
       get2dv.f \
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
