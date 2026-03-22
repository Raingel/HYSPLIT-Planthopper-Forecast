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

INCLUDES = -I $(top_srcdir) -I $(top_srcdir)/arcview1 -I $(top_srcdir)/arcview2 -I .

MODULES = shapeuidraw.o arrayutil.o intersectable.o unformio.o

# add -DWITH_STRICMP if you are compiling i.e. with MS VC
# add -DDEBUG if you want lots of debugging infos

prefix = ..

.c.o:
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

all: libshaputil.a

libshaputil.a: $(MODULES)
	$(AR) $(ARFLAGS) cru $@  $(MODULES)
	cp $@ ../

clean:
	rm -f *.o *.a

distclean: clean
	rm -f Makefile
