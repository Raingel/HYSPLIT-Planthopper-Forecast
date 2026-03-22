/*--------------shapearea.c---------------------------------------
 *
 */
#include "config.h"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <shapext.h>
#include <shapefil.h>

#define PROGNAME "shapearea"

#define MAJORV 1
#define MINORV 1
#define PATCHLEVEL 0
#define LVERSION MAJORV.MINORV.PATCHLEVEL
#define XSUB(V) #V
#define SUB(V) XSUB(V)


int verb_level=0;
double radEarth=REARTH;

static void Usage(char * progname) {
  fprintf(stderr, "ARL " PROGNAME " version " SUB(LVERSION) "\n\n");
  fprintf(stderr,"Usage %s [options] file [... file ] \n\n"
    "where all files are polygon-type shape files\n"
    "Output on stdout will list each polygon's area,\n"
    " followed by the contents of its' .dbf file.\n"
    "Options:\n"
    " -r Radius of Earth in km. (default %.2f)\n"
    " -c Output coordinates of centroid as well\n"
    ,progname,radEarth);
  exit(1);
}

int main(int argc, char ** argv) {

aShape shapeFile;
int kfile,krecd,kfield;
int printCentroid=0;
vector2 centroid;
vector2 * locus=NULL;
extern int optind;
extern char *optarg;
int ch;
  while ((ch = getopt(argc, argv, "v:r:c")) != EOF) {
    switch (ch) {
    case 'v' :
      if(sscanf(optarg,"%d",&verb_level) != 1) Usage(argv[0]);
      break;
    case 'r' :
      if(sscanf(optarg,"%lf",&radEarth) != 1) Usage(argv[0]);
      break;
    case 'c' :
      printCentroid=1;
      locus = &centroid;
      break;
    default:
      Usage(argv[0]);
    }
  }

  if (argc-optind < 1) Usage(argv[0]);

  for (kfile=optind; kfile<argc; kfile++) {
    ShapeOpen(&shapeFile,argv[kfile]);
    if (shapeFile.nShapeType != SHPT_POLYGON) {
      fprintf(stderr,"Error: shape file %s not type polygon\n\n",argv[kfile]);
      ShapeClose(& shapeFile);
      continue;
    }
    printf("file %s\n",argv[kfile]);
    printf("   %10s","Area");
    if (printCentroid) printf (" %10s %10s","Cent-x","Cent-y");
    for (kfield=0; kfield<shapeFile.nFields; kfield++) {
      printf("%10s ",shapeFile.f[kfield].fieldName);
    }
    printf("\n");
    for (krecd=0; krecd<shapeFile.nRecds; krecd++) {
      SHPObject * obj;
      double Area;
      shapeGetObj(& shapeFile,krecd,&obj);
      Area = (radEarth/REARTH)*(radEarth/REARTH)*areaObjSph(obj,locus);
      printf("   %#10.2f",Area);
      if (printCentroid) printf(" %#10.2f %#10.2f",centroid.x,centroid.y);
      for (kfield=0; kfield<shapeFile.nFields; kfield++) {
        int ival;double dval;const char * sval;
        switch (shapeFile.f[kfield].Type) {
          case FTInteger:
            ival = DBFReadIntegerAttribute(shapeFile.hDBF,krecd,kfield);
            printf(" %10d",ival);
          break;
          case FTDouble:
            dval = DBFReadDoubleAttribute(shapeFile.hDBF,krecd,kfield);
            printf(" %#10.2f",dval);
          break;
          case FTString:
            sval = DBFReadStringAttribute(shapeFile.hDBF,krecd,kfield);
            printf(" %10s",sval);
          break;
          default:
            fprintf(stderr,"Type %s not recognized\n",
                             FTLabel(shapeFile.f[kfield].Type));
        }
      }
      SHPDestroyObject( obj );
      printf("\n");
    }
    ShapeClose(& shapeFile);
  }
  return 0;
}
