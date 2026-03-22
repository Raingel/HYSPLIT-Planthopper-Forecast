SHELL = /bin/bash

prefix = /usr/local
exec_prefix = ${prefix}
bindir = ${exec_prefix}/bin
srcdir = .
libdir = ${exec_prefix}/lib
top_srcdir = .

F77 = gfortran
CC = gcc
FFLAGS = -g -O2
CFLAGS = -O -g -O2
LDFLAGS = 
ARFLAGS = 
RANLIB = ranlib
EXEEXT = 
FCTEST = fctest

LIBNAME = $(F77)fcsubs
LIBA = lib$(LIBNAME).a

INSTALL = /usr/bin/install -c

all: library

library : $(LIBA)

fcsubs.o: fcsubs.c fcsubs.h
swapobj.o: swapobj.c fcsubs.h

$(LIBA) : fcsubs.o swapobj.o
	$(AR) $(ARFLAGS) r $(LIBA) $?
	- $(RANLIB) $(LIBA)

$(FCTEST) : $(FCTEST).f $(LIBA)
	${F77} ${FFLAGS} -o $(FCTEST) $(FCTEST).f -L. -l$(LIBNAME)

test : $(FCTEST)
	@./$(FCTEST) | tr -s "" " " >result.ot1
	@if diff result.ot1 result.txt >/dev/null;then echo test okay;else echo failure;fi

install: library
	$(INSTALL) -d $(libdir)
	$(INSTALL) -m 644 $(LIBA) $(libdir)
# for use with install-sh, perms 644 must be separated from -m

clean:
	rm -f *.o *.a $(FCTEST)$(EXEEXT)
	rm -f $(libdir)/$(LIBA)

distclean: clean
	rm -f fcsubs.h Makefile config.* test.out result.ot1
