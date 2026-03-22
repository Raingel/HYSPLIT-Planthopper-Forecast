#---------------------------------------------------------
# Makefile for the lbfgsb library 
# Last Revised: 26 Aug 2016
#		23 Aug 2019 - moved compiler options and
#			      library definitions to
#			      ../../Makefile.inc.
#		26 Aug 2019 - removed the INC variable.
#
#----------------------------------------------------------

include ../../Makefile.inc

SHELL = /bin/sh

DIR=..
LIB=$(DIR)/liblbfgsb.a

LFLAGS = -vr

CODE =              \
       lbfgsb.f \
       linpack.f \
       blas.f \
       timer.f 

$(LIB) : $(CODE)
	for member in $?; do make member MODULE=$$member;done

member : $(MODULE)
	$(FC) -c $(FFLAGS) $?
	$(AR) $(LFLAGS) $(LIB) $(?F:.f=.o)
	ranlib $(LIB)
	rm -f $(?F:.f=.o)

clean :
	rm -f *.o
	rm -f $(LIB)



