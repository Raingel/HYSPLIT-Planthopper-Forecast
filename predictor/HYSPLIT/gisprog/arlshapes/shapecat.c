#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <limits.h>
#include <shapefil.h>
#include "shapext.h"

/* Object Type codes used in main(): */
/* Error codes for exit() routine: */

#define PROGNAME "shapecat"
#define ERR_USAGE       0

#define MAJORV 1
#define MINORV 1
#define PATCHLEVEL 0
#define LVERSION MAJORV.MINORV.PATCHLEVEL
#define XSUB(V) #V
#define SUB(V) XSUB(V)

void Usage(char * progname) {
  fprintf(stderr, "ARL " PROGNAME " version " SUB(LVERSION) "\n\n");
  fprintf(stderr,"Usage %s [options] file [... file] \n\n"
    "Where file [... file] represents a sequence of shape files.\n\n"
    "Options:\n"
    "\t-f filename [Output Shape File:Default a]\n\n"
    "\tOutputs to outputsahpefile the shapes and corresponding\n"
    "\tdatabase records of the one or more input shapefiles.\n"
    ,progname);
  exit(ERR_USAGE);
}


int main(int argc, char ** argv ) {
int kfilein;

aShape shapeFile,shapeFileRef,shapeFileOut;

extern int optind;
extern char *optarg;
char fileout[PATH_MAX+1] = "a";
int kRecOut,kRecIn;
int fieldCount,fieldCountRef;
fieldType * ft,* ftp;

int ch;
  while ((ch = getopt(argc, argv, "f:p:nlb:r:R:t:v:")) != EOF) {
    switch (ch) {
    case 'f':
      strcpy(fileout,optarg);
      break;

    default:
      Usage(argv[0]);
    }
  }

  if (optind >= argc) Usage(argv[0]);
  ShapeOpen(&shapeFileRef,argv[optind]);
  fieldCountRef = DBFGetFieldCount(shapeFileRef.hDBF);
  ft = fillDBFTypes(shapeFileRef.hDBF) ;
  newShapeFile(&shapeFileOut, fileout, shapeFileRef.nShapeType,
                                            fieldCountRef,ft) ;
  kRecOut=0;

  for (kfilein=optind;kfilein<argc;kfilein++) {
    SHPObject * Obj;
    /* printf("file %d (%s)\n",kfilein-optind,argv[kfilein]); */
    if (ShapeOpen(&shapeFile,argv[kfilein]) <= 0) continue;
    fieldCount = DBFGetFieldCount(shapeFile.hDBF);
    ftp = fillDBFTypes(shapeFile.hDBF) ;
    if ((fieldCountRef != fieldCount) || !matchDBFTypes(ft,ftp,fieldCountRef)) {
      printf("DBF Types for %s do not match\n",argv[kfilein]);
    } else {
      for (kRecIn = 0;kRecIn < shapeFile.nRecds;kRecIn++) {
        Obj = SHPReadObject( shapeFile.hSHP, kRecIn);
        SHPWriteObject(  shapeFileOut.hSHP, -1, Obj);
        cpShapeData(shapeFileOut.hDBF, kRecOut, shapeFile.hDBF, kRecIn,
                 fieldCountRef, ft) ;
        SHPDestroyObject( Obj);
        kRecOut++;
      }
    }
    emptyDBFTypes(shapeFile.hDBF,ftp) ;
    ShapeClose(& shapeFile);
  }
  ShapeClose(& shapeFileOut);
  emptyDBFTypes(shapeFileRef.hDBF,ft) ;
  ShapeClose(& shapeFileRef);
  return 0;
}
