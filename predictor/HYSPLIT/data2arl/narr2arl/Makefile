#--------------------------------------------------------
# Master Makefile for exectables directory /exec
# Last Revised: 
#               07 Feb 2019 - narr2arl
#               23 Aug 2019 - moved compiler options and library
#                             definitions to ../Makefile.inc.
#               26 Aug 2019 - removed the INC variable.
#--------------------------------------------------------

include ../../Makefile.inc

SHELL = /bin/sh

# directory with include files
FFLAGS += -I../../library/hysplit


# directory location for source and libary
SRC  = .
LIB  = ../../library
EXE  = ../../exec


all : narr2arl
 
narr2arl : $(SRC)/narr2arl.f 
	$(FC) $(FFLAGS) $(FFLAGS_FREEFORM) -o $(EXE)/$@ $(SRC)/$@.f -L$(LIB) $(CLIB) -lhysplit

clean :
	rm -f *.mod
	rm -f $(EXE)/narr2arl
