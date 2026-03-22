#include <config.h>
#include <stdlib.h>
#include <limits.h>
#include <stdio.h>
#include <unistd.h>
#include  "shapext.h"
#include <string.h>
#include <shapefil.h>

#define ERR_EXIT 0
#define PROGNAME "dbf2csv"

#define MAJORV 1
#define MINORV 1
#define PATCHLEVEL 0
#define LVERSION MAJORV.MINORV.PATCHLEVEL
#define XSUB(V) #V
#define SUB(V) XSUB(V)


int verb_level=0;

void Usage(char * prog) {
  fprintf(stderr, "ARL " PROGNAME " version " SUB(LVERSION) "\n\n");
  fprintf(stderr, "\tUsage: %s [options] filename\n"
         "\twhere filename is a .dbf file\n"
         "Options:\n"
         "\t-h no header lines on output\n"
         ,prog);
  exit(ERR_EXIT);
}

int main(int argc,char ** argv) {
char dbffname[PATH_MAX];
DBFHandle hDBF;
fieldType * ft;
int j,k,fieldCount,recordCount;
int printHeader=1;
extern int optind;
extern char *optarg;
int ch;
  while ((ch = getopt(argc, argv, "v:h")) != EOF) {
    switch (ch) {
    case 'v' :
      if(sscanf(optarg,"%d",&verb_level) != 1) Usage(argv[0]);
      break;
    case 'h' :
      printHeader=0;
    break;
    default:
      Usage(argv[0]);
    }
  }

  if (argc-optind != 1) Usage(argv[0]);

    strcpy(dbffname,argv[optind]);
    if( strstr(dbffname,".dbf") == NULL) strcat(dbffname,".dbf");
    hDBF = DBFOpen( dbffname, "r+b" );
    if( hDBF == NULL ) {
      fprintf(stderr, "DBFOpen(%s,\"r+b\") failed.\n", dbffname );
    exit(ERR_EXIT);
  }
  fieldCount = DBFGetFieldCount(hDBF);
  recordCount = DBFGetRecordCount(hDBF);
  /*printf ("field count for %s is %d, record count is %d\n",
       dbffname,fieldCount,recordCount);fflush(stdout);*/
  ft = fillDBFTypes(hDBF);

  if (printHeader) {
    for (k=0;k<fieldCount;k++) {
      printf("\"%s\"",ft[k].fieldName);
      if (k<fieldCount-1) {
        printf(",");
      } else {
        printf("\n");
      }
    }
    fflush(stdout);
  }
  for (j=0;j<recordCount;j++) {
    for (k=0;k<fieldCount;k++) {
      switch (ft[k].Type) {
        case FTInteger:
          printf("%d",DBFReadIntegerAttribute( hDBF, j, k));
        break;
        case FTDouble:
          printf("%f",DBFReadDoubleAttribute( hDBF, j, k));
        break;
        case FTString:
          printf("\"%s\"",DBFReadStringAttribute( hDBF, j, k));
        break;

      }
      if (k<fieldCount-1) {
        printf(",");
      } else {
        printf("\n");
      }
    }
  }

  emptyDBFTypes(hDBF, ft); 
  return 0;
}
