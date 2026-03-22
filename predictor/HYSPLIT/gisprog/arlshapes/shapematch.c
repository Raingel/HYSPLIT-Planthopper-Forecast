/*--------------shapematch.c---------------------------------------
 *
 */
#include "config.h"
#include <stdlib.h>
#include <stdio.h>                                       
#include <shapext.h>
#include <string.h>
#include <shapefil.h>
#include <unistd.h>
#include "arrayutil.h"
#include "shapeuidraw.h"

/*#include "utils.h"*/
#include "vector2.h"

/* Object Type codes used in main(): */
/* Error codes for exit() routine: */

#define PROGNAME "shapematch"
#define	ERR_USAGE	0

int verb_level=0;

#ifndef  LVERSION
#define MAJORV 1
#define MINORV 1
#define PATCHLEVEL 0
#define LVERSION MAJORV.MINORV.PATCHLEVEL
#endif
#define XSUB(V) #V
#define SUB(V) XSUB(V)

static void Usage(char * progname) {
  fprintf(stderr, "ARL " PROGNAME " version " SUB(LVERSION) "\n\n");
  fprintf(stderr,"Usage %s [-options] filein1 filein2 fileout\n\n"
    "where all files are polygon-type shape files\n"
    "Output file fileout will contain all intersections of\n"
    "a polygon from filein1 and a polygon from filein2.\n"
    "options:\n"
    "\t-r xmin,ymin,xmax,ymax [range of values covered: Default -180,-90,180,90]\n"
    ,progname);
  exit(ERR_USAGE);
}

struct {
double xmin,ymin,xmax,ymax;
} vBox = {-180.,-90.,180.,90.};

int xyrange(char * spec,double * xmin,double * ymin,double * xmax,double * ymax) {
double tmp;
char * arg;
  arg = strtok(spec,",");
  sscanf(arg,"%lf",xmin);
  arg = strtok(NULL,",");
  sscanf(arg,"%lf",ymin);
  arg = strtok(NULL,",");
  sscanf(arg,"%lf",xmax);
  arg = strtok(NULL,",");
  sscanf(arg,"%lf",ymax);
  if (* xmin > * xmax) {tmp=* xmin;* xmin=* xmax;* xmax=tmp;}
  if ( * ymin >  * ymax) {tmp= * ymin; * ymin= * ymax; * ymax=tmp;}
  return 0;
}

int doesIntersect(SHPObject * objA, SHPObject * objB) {
SHPObject * objTest;
int retval;
  if ((retval = findIntersect(objA, objB, &objTest)) == 0) {
    SHPDestroyObject( objTest );objTest = NULL;
  }
  return retval; 
}

/*---------------main--------------------------------
 *
 */
int main(int argc, char ** argv ) {
aShape shapeFile[3];
/*Input files 0 and 1; output file 2.*/
SHPObject * objA, * objB, * xYR;

double areaA,areaB,areaI,temp;

/*int * index[2]={NULL,NULL};*/
/* Establish .dbf field types for output files.*/
fieldType t[]={{FTInteger,"id",10,0},{FTInteger,"recdA",10,0},
               {FTInteger,"recdB",10,0},{FTDouble,"areaA",10,2},
               {FTDouble,"areaB",10,2},{FTDouble,"areaInt",10,2},
               {FTDouble,"pctA",10,2},{FTDouble,"pctB",10,2},
               {FTDouble,"FigMerit",10,2}};
int i,j,k;
int recno=0; /*Output record number*/

/*arrinfoIntersect a = {0,0,NULL}, b = {0,0,NULL};*/
/* arrint aPartTyp={0,0,NULL},bPartTyp={0,0,NULL};*/
#ifdef DEBUGGER
FILE * logfile;
#endif

extern int optind;
extern char *optarg;
int ch;
  while ((ch = getopt(argc, argv, "v:r:h")) != EOF) {
    switch (ch) {
    case 'v' :
      if(sscanf(optarg,"%d",&verb_level) != 1) Usage(argv[0]);
      break;
    case 'r':
      xyrange(optarg , &vBox.xmin, &vBox.ymin, &vBox.xmax, &vBox.ymax) ;
      break;
    case 'h':
    default:
      Usage(argv[0]);
    }
  }

  if (argc-optind < 3) Usage(argv[0]);

/* Create shape of rangeview
 */
  { double xYRx[5],xYRy[5];
    int ii[1]={0};
    xYRx[0] = xYRx[1] = xYRx[4] = vBox.xmin;
    xYRx[2] = xYRx[3] = vBox.xmax;
    xYRy[0] = xYRy[3] = xYRy[4] = vBox.ymin;
    xYRy[1] = xYRy[2] = vBox.ymax;
    xYR = SHPCreateObject( SHPT_POLYGON, 0, 1, ii, NULL, 5, xYRx, xYRy, NULL, NULL);
  }

/*--------open Input Shapefiles ---------------------------------*/
  for(i=0;i<2;i++) {
    ShapeOpen(&shapeFile[i],argv[optind+i]);
    if (shapeFile[i].nShapeType != SHPT_POLYGON) {
      fprintf(stderr,"Error: shape file %s not type polygon\n\n",argv[i+1]);
      Usage(argv[0]);
    }
  }
/*-----------and create new Output Shapefile -------------------*/
  newShapeFile(& shapeFile[2],argv[optind+2], SHPT_POLYGON,sizeof(t)/sizeof(t[0]),t) ;

  for (j=0;j < shapeFile[0].nRecds;j++) {
/*--------- get j'th record from first shapefile ------------------ */
    DEBUG_OUT1("get %d'th record from first shapefile\n",j);
    shapeGetObj(& shapeFile[0],j,&objA);
    if (checkObj(objA) !=0) {
      fprintf(stderr,"Caution: record %d of first file may be invalid."
                     " Skipping.\n",j);
      continue;
    }
/*-------- Detect whether object objA intersects range ------------------*/
    if( doesIntersect(objA, xYR) == 0) continue;
    for (k=0;k<shapeFile[1].nRecds;k++) {
/*------------- and k'th record from second shapefile ================== */ 
      SHPObject * objOut;
      DEBUG_OUT1("get %d'th record from second shapefile\n",k);
      shapeGetObj(& shapeFile[1],k,&objB);

      if (checkObj(objB) !=0) {
        fprintf(stderr,"Caution: record %d of second file may be invalid"
                       " Skipping.\n",k);
        continue;
      }
/*-------- Detect whether object objB intersects range ------------------*/
    if( doesIntersect(objB, xYR) == 0) continue;

/*Output new object to output file.*/
      DEBUG_OUT("Entering findIntersect\n");
      if (findIntersect(objA, objB, &objOut) != 0) {
        DEBUG_OUT("Leaving findIntersect\n");
        if( doesIntersect(objOut, xYR) != 0) {
          recno = shapePutObj(& shapeFile[2],objOut);
          DEBUG_OUT3("Output record # %d input records %d and %d\n",recno,j,k);
          DBFWriteIntegerAttribute(shapeFile[2].hDBF,recno,0,recno);
          DBFWriteIntegerAttribute(shapeFile[2].hDBF,recno,1, j);
          DBFWriteIntegerAttribute(shapeFile[2].hDBF,recno,2, k);
          areaI = areaObjSph(objOut,NULL);
          shapeFieldSet(&shapeFile[2],recno,5,&areaI);
          areaA = areaObjSph(objA,NULL);
          shapeFieldSet(&shapeFile[2],recno,3,&areaA);
          areaB = areaObjSph(objB,NULL);
          shapeFieldSet(&shapeFile[2],recno,4,&areaB);
          temp = 100. * areaI/areaA;
          shapeFieldSet(&shapeFile[2],recno,6,&temp);
          temp = 100. * areaI/areaB;
          shapeFieldSet(&shapeFile[2],recno,7,&temp);
          temp = 100. * areaI/(areaA+areaB-areaI);
          shapeFieldSet(&shapeFile[2],recno,8,&temp);
          #ifdef DEBUGGER
          printObject(objA,"Object A");
          printObject(objB,"Object B");
          printObject(objOut,"Intersection");
          #endif
        }
        SHPDestroyObject( objOut );objOut = NULL;
      } else {
DEBUG_OUT2("Leaving findIntersect no int input records %d and %d\n",j,k);
      }
      SHPDestroyObject( objB );objB = NULL;
      if (verb_level > 0) printf("Arecd=%d, Brecd=%d\n",j,k);
    }
    SHPDestroyObject( objA );objA = NULL;
  }
  if (verb_level > 0) printf("Output file has %d objects.\n",recno);

  for (i=0;i<3;i++) {
    ShapeClose(& shapeFile[i]);
  }
  closeShop();
  return 0;
}
