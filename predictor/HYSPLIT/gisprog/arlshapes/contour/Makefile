SHELL = /bin/bash

iprefix = /usr/local
bindir = ${exec_prefix}/bin
srcdir = .
libdir = ${exec_prefix}/lib
top_srcdir = ..

CC  = gcc

CFLAGS= $(DEBUG)
ARFLAGS=

#INSTALL = /usr/bin/install -c

INCLUDES= -I $(top_srcdir) -I $(top_srcdir)/arcview1 -I $(top_srcdir)/arcview2 -I .

# $Id: Makefile,v 1.5 2000/06/12 13:24:08 jan Exp $

# add -DDEBUG if you want lots of debugging infos

prefix = ..
EXEEXT = 

OBJECTS= gridwork.o splitobj.o shapeContour.o

all: libcontour.a

.c.o:
	$(CC) $(CFLAGS) $(INCLUDES) -c $?

#install: libshapext.a shapext.h
#	$(INSTALL) -d $(iprefix)/include
#	$(INSTALL) -d $(iprefix)/lib
#	$(INSTALL) libshapext.a $(iprefix)/lib
#	$(INSTALL) shapext.h $(iprefix)/include

#vector2.o: vector2.c
#	$(CC) $(CFLAGS) $(INCLUDES) -c $?

#shapext.o: shapext.c
#	$(CC) $(CFLAGS) $(INCLUDES) -c $?

libcontour.a: $(OBJECTS)
	$(AR) $(ARFLAGS) cru $@  $(OBJECTS)
	cp $@ ..

#utils.o: utils.c utils.h
#	$(CC) $(CFLAGS) -c utils.c


clean:
	rm -f *.o *.a

distclean: clean
	rm -f Makefile config.h config.cache config.log config.status
