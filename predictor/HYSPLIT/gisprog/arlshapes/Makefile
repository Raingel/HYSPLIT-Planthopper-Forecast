#-----------------------------------------------------------------------------
# Makefile.in - input for automake.
#
# Last Revised: 23 Aug 2019 - added the 'install' make target.
#-----------------------------------------------------------------------------
SHELL=/bin/bash

# Package and Version Info:
#     arlshapes 1.0
#  Report bugs to albion.taylor@noaa.gov


prefix=/usr/local
srcdir=.

EXEEXT=
OBJEXT=o


CC=gcc

CFLAGS=

LIBS=-lshapext -lshp -lcontour -lshaputil -lm 
LDFLAGS=-L. 
RANLIB=ranlib

AR=@AR@
ARFLAGS=
INCLUDES=-I arcview2 -I arcview1 -I contour -I shapeutil

INSTALL=/usr/bin/install -c

EXECUTABLES=shapeHysplitOut ascii2shp dbf2csv shapesplit shapearea shapecat shapesvg

EXECUT2=shapematch shapeunion
# shapeplot

all: libshaputil.a $(EXECUTABLES) $(EXECUT2)

libshaputil.a:
	cd shapeutil;$(MAKE)

libshapext.a:
	cd arcview1;$(MAKE)

shapeHysplitOut: libshapext.a libshp.a libcontour.a

libshp.a:
	cd arcview2;$(MAKE)

ascii2shp: libshapext.a libshp.a libcontour.a

libcontour.a:
	cd contour;$(MAKE)

dbf2csv: libshapext.a libshp.a

.c:
	$(CC) $(CFLAGS) -o $@ $< $(INCLUDES) $(LDFLAGS) $(LIBS)

install: ascii2shp
	$(INSTALL) -c -m 755 ascii2shp ../../exec

clean:
	cd shapeutil;$(MAKE) clean
	cd contour;$(MAKE) clean
	cd arcview2;$(MAKE) clean
	cd arcview1;$(MAKE) clean
	rm -f *.$(OBJEXT)
	rm -f *.a
	@for pgm in $(EXECUTABLES); do rm -f $${pgm}$(EXEEXT) ;done
	@for pgm in $(EXECUT2); do rm -f $${pgm}$(EXEEXT) ;done

distclean: clean
	cd shapeutil;$(MAKE) distclean
	cd contour;$(MAKE) distclean
	cd arcview2;$(MAKE) distclean
	cd arcview1;$(MAKE) distclean
	rm -fR autom4te.cache Makefile config.h config.log config.status

