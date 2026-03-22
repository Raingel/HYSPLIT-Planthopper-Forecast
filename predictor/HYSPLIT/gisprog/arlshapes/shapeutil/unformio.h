#ifndef _UNFORMIO_H_
#define _UNFORMIO_H_
#if 1==0
double bin2float(char * c) ;
int bin2int(char * c) ;
int short2int(char * c) ;
#endif

typedef struct {
  int firstToken,lastToken,dataSize;
  char * data;
} unfRecord;

int unfscanf(const char * format,int offset, ... ) ;
int readUnfRecord (FILE * infile) ;
void freeUnfRecord(void) ;

#endif /* _UNFORMIO_H_ */
