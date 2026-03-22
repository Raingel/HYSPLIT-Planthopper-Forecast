SHELL = /bin/bash

iprefix = /usr/local
bindir = ${exec_prefix}/bin
srcdir = .
libdir = ${exec_prefix}/lib
top_srcdir = ..

CC  = gcc

EXEEXT = 
DEBUG = 
CFLAGS= $(DEBUG)
ARFLAGS=

INSTALL = /usr/bin/install -c

INCLUDES = -I $(top_srcdir) -I $(top_srcdir)/arcview1 -I $(top_srcdir)/arcview2

# $Id: Makefile,v 1.5 2000/06/12 13:24:08 jan Exp $

# add -DWITH_STRICMP if you are compiling i.e. with MS VC
# add -DDEBUG if you want lots of debugging infos

prefix = ..

all: libshapext.a

.c.o:
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

libshapext.a: shapext.o vector2.o utils.o
	$(AR) $(ARFLAGS) cru $@  $?
	cp $@ ..

clean:
	rm -f ascii2shp$(EXEEXT) *.o *.a

distclean: clean
	rm -f Makefile config.h
