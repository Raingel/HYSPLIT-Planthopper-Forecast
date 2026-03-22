#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <stdarg.h>
#define PROGNAME "shapeHysplitOut"
#include "../config.h"
#include <unformio.h>

#ifdef WORDS_BIGENDIAN
#define RDTYPE(out,type,inbuf) out= * (type *) inbuf;
#else
#define RDTYPE(out,type,inbuf) {int k;for(k=0;k<sizeof(type);k++) hold[sizeof(type)-1-k]=inbuf[k];out=*(type *) hold;}
#endif

/*Note: these routines read files formatted as FORTRAN "Unformatted IO".
 * For Hysplit files, the data are apparently big-endian, even when
 * created on a little-endian platform.  Generally, an "Unformatted" record
 * is a 4-byte integer giving a byte count, folllowed by that many
 * bytes of data, followed by the same 4-byte integer byte count.
 */
static unfRecord record = {0,0,0,NULL}; 
static char hold[8];

int unfscanf(const char * format,int offset, ... ) {
va_list ap;
va_start(ap,offset);
char * copy = (char *) malloc(strlen(format)+1);strcpy(copy,format);
char * tok = strtok(copy," ,\t");
char * ostr;
int nchar;
  for ( ;tok != NULL; tok = strtok(NULL," ,\t")) {
    switch (*tok) {
    case 'i':case 'I':

  /*    * va_arg(ap,int *) =  bin2int(record.data + offset); */
/*      * va_arg(ap,int *) =  * (int *) (record.data + offset);*/
      RDTYPE(* va_arg(ap,int *),int, (record.data + offset));
      offset += sizeof(int);
      break;
    case 'f':case 'F':
  /*    * va_arg(ap,double *) =  bin2float(record.data + offset); */
/*      * va_arg(ap,double *) =  * (float *) (record.data + offset);*/
      RDTYPE(* va_arg(ap,double *),float,(record.data + offset));
      offset += sizeof(float);
      break;
    case 's':case 'S':
/*      * va_arg(ap,int *) =  short2int(record.data + offset); */
/*      * va_arg(ap,int *) =  * (short *) (record.data + offset);*/
      RDTYPE(* va_arg(ap,int *),short,(record.data + offset));
      offset += sizeof(short);
      break;
    case 'c':case 'C':
      sscanf(tok+1,"%d",&nchar);
      if(nchar>0) {
        ostr = va_arg(ap,char *);
        strncpy( ostr, record.data + offset,nchar);
        * (ostr + nchar) = '\0';
        offset += nchar;
      }
      break;
    }
    if (offset >= record.firstToken){
      free(copy);
      return -1;
    }
  }
  free(copy);
  return offset;
}

int readUnfRecord (FILE * infile) {
char buffer[sizeof(int)];
size_t size;
size_t bytesRead;
DEBUG_OUT1("File position %ld\n",ftell(infile));
  if( fread(buffer,sizeof(int),1,infile) != 1) {
    return -1;
  }
/*  size = bin2int(buffer); */
/*  size = * (int *) buffer;*/
  RDTYPE(size,int,buffer);
DEBUG_OUT1("size=%d\n",size);
  if (record.dataSize < size) {
    record.data = realloc(record.data,size);
    if (record.data == NULL) fprintf(stderr,"Error: unsuccessful realloc \n");fflush(stderr);
    record.dataSize = size;
  }
  record.firstToken = size;
  bytesRead = fread(record.data, sizeof(char), size, infile);
  if (bytesRead != size) {
    fprintf(stderr,"Warning:  bytesRead=%d, size=%d \n",bytesRead,size);fflush(stderr);
    return -1;
  }
  fread(buffer,sizeof(int),1,infile);
/*  record.lastToken = bin2int(buffer); */
/*  record.lastToken =  * (int *) buffer; */
  RDTYPE(record.lastToken,int,buffer);
  if (record.firstToken != record.lastToken) {
   fprintf(stderr,"Warning : first %d(%x) not last %d(%x)\n",record.firstToken,record.firstToken,
          record.lastToken, record.lastToken);fflush(stderr);
   return -1;
  }
  return size;
}

void freeUnfRecord(void) {
  if (record.data != NULL) {
    free(record.data);
    record.data = NULL;
    record.dataSize = record.firstToken = record.lastToken = 0;
  }
}

#if 1==0
double bin2float(char * c) {
int k,l;
union {
float f;
char c[sizeof(float)];
} retval;
#ifndef WORDS_BIGENDIAN
  for (k=sizeof(float)-1,l=0; k>=0; k--,l++) {
#else 
  for (k=0,l=0;k<sizeof(float);k++,l++) {
#endif
    retval.c[l] = c[k];
  }
  return retval.f;
}

int bin2int(char * c)  {
union {
int i;
char c[sizeof(int)];
} retval;

int k,l;

#ifndef WORDS_BIGENDIAN
  for (k=sizeof(int)-1,l=0,retval.i=0; k>=0; k--,l++) {
#else
  for (k=0,l=0,retval.i=0;k<sizeof(int);k++,l++) {
#endif
    retval.c[l] = c[k];
  }
  return retval.i;
}

int short2int(char * c) {
union {
short i;
char c[sizeof(short)];
} retval;
int k,l;

#ifndef WORDS_BIGENDIAN
  for (k=sizeof(short)-1,l=0,retval.i=0; k>=0; k--,l++) {
#else 
  for (k=0,l=0,retval.i=0;k<sizeof(short);k++,l++) {
#endif
    retval.c[l] = c[k];
  }
  return retval.i;
}
#endif
