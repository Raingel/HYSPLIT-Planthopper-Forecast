include ../Makefile.inc

SHELL = /bin/sh

PGM=bdy2bin
EXE=.

$(PGM) : $(PGM).f 
	$(FC) -o $(EXE)/$@ $(FFLAGS) $(FFLAGS_FREEFORM) $? 

clean :
	rm -f $(EXE)/$(PGM)  
