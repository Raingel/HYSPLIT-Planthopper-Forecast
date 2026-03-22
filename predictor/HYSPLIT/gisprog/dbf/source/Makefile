# Change history:
# 16 Nov 2023 - Use Makefile.inc to use the CC and LDFLAGS_CPROG variables.
#
include ../../../Makefile.inc

SHELL = /bin/sh

#CC     = gcc
LFLAGS = -vr
#CFLAGS = -I$(VPATH)
CFLAGS = -I$(VPATH) $(LDFLAGS_CPROG)
VPATH  = ../include
LIB    = libDbf.a
EXE    = ../../../exec

CODE   = endian.c \
         utils.c  \
         dbf.c 

all: txt2dbf dbf2txt 

$(LIB) : $(CODE)
	for member in $?; do make member MODULE=$$member;done

member : $(MODULE)
	$(CC) -c $(CFLAGS) $?                                 
	$(AR) $(LFLAGS) $(LIB) $(?F:.c=.o)
	ranlib $(LIB)

txt2dbf : txt2dbf.c $(LIB)
	$(CC) $(CFLAGS) -o $(EXE)/$@ txt2dbf.c -L./ -lDbf

dbf2txt : dbf2txt.c $(LIB)
	$(CC) $(CFLAGS) -o $(EXE)/$@ dbf2txt.c -L./ -lDbf 

clean:
	rm -f *.o 
	rm -f $(LIB)
