/*----------shapeunion.c----------------------------------------------
 *
 */
#include "config.h"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include  <shapext.h>
#include <shapefil.h>
#include "shapeuidraw.h"

#define ERR_USAGE 0
#define ERR_FILE 0
#define PROGNAME "shapeunion"

#define MAJORV 1
#define MINORV 1
#define PATCHLEVEL 5
#define LVERSION MAJORV.MINORV.PATCHLEVEL

int verb_level=0;

static void Usage(char * progname) {
  fprintf(stderr, "ARL " PROGNAME " version " SUB(LVERSION) "\n\n");
  fprintf(stderr,"Usage %s [options] filein fileout\n\n"
    "where all files are polygon-type shape files.\n"
    "Forms the union of all shapes in the input file.\n"
    "Options:\n"
    "\t -v verblevel(0-2) verbosity level\n"
    "\t -c Output file has a single object covering all parts of the union\n"
    "\t -s (default) Output file has separate object for any contiguous\n"
    "\t\t part of the union\n"
    ,progname);
  exit(ERR_USAGE);
}

int inside (int j, int k, SHPObject * obj) {
/*returns -1 if part j inside part k, +1 if part k inside part j, 0 otherwise.*/
int venn;
vector2 partAmin,partAmax;
vector2 partBmin,partBmax;
vector2 point;
  limitsPartObj(j, obj, &partAmin.x,&partAmin.y, &partAmax.x,&partAmax.y);
  limitsPartObj(k, obj, &partBmin.x,&partBmin.y, &partBmax.x,&partBmax.y);
    venn = boxVenn(&partAmin, &partAmax,  &partBmin, &partBmax);
  if ((venn == 0) || (venn == 3) ) return 0;   /*Neither a inside b nor b inside a*/
  switch (venn) {
  case 0:case 3:  return 0;   /*Neither a inside b nor b inside a*/
  case 1: /*Limits of j(A) inside limits of k(B).*/
    point.x = obj->padfX[obj->panPartStart[j]];
    point.y = obj->padfY[obj->panPartStart[j]];
    if ( insidePartObj(k, obj, 1, NULL, &point)) {
      return -1;
    }
    return 0;
  case 2:
    point.x = obj->padfX[obj->panPartStart[k]];
    point.y = obj->padfY[obj->panPartStart[k]];
    if ( insidePartObj(j, obj, 1, NULL, &point)) {
      return 1;
    }
    return 0;
  }
  return -1;
}

void addPart(SHPObject * obj,int objPart,double * XX,double * YY,int nPart,int * startVertex) {
int first,last,j,k,firstout;
  startVertex[0]=0;
  firstout=startVertex[nPart];
  objPartIndices(obj, objPart, &first, &last) ;
  for (j=first,k=firstout;j<last;j++,k++) {
    XX[k] = obj->padfX[j];
    YY[k] = obj->padfY[j];
  }
  startVertex[nPart+1] = last - first + firstout;
}


int main(int argc, char ** argv ) {
aShape shapeFileIn,shapeFileOut;
SHPObject * objA, * objUnion, *objOut;
int i,firstObj,k,m,n;
int recno=0; /*Output record number*/

#ifdef DEBUGGER
FILE * logfile;
#endif

int splitOut=TRUE;
extern int optind;
extern char *optarg;
int ch;

  while ((ch = getopt(argc, argv, "v:cs")) != EOF) {
    switch (ch) {
    case 'v' :
      if(sscanf(optarg,"%d",&verb_level) != 1) Usage(argv[0]);
      break;
    case 'c':
      splitOut=FALSE;
      break;
    case 's':
      splitOut=TRUE;
      break;
    default:
      Usage(argv[0]);
    }
  }

  if (argc-optind < 2) Usage(argv[0]);
#ifdef DEBUGGER
  logfile = fopen("debug.log","w");
#endif         
  ShapeOpen(&shapeFileIn,argv[optind]);
  if (shapeFileIn.nShapeType != SHPT_POLYGON) {
    ShapeClose(& shapeFileIn);
    fprintf(stderr,"Error: shape file %s not type polygon\n\n",argv[optind]);
    Usage(argv[0]);
  }
  
  newShapeFile(& shapeFileOut,argv[optind+1], SHPT_POLYGON,
              shapeFileIn.nFields, shapeFileIn.f) ;
/*sizeof(t)/sizeof(t[0]),t) ;*/
/*Find first usable object; stack it on output object:
 */
  for (firstObj=0;firstObj < shapeFileIn.nRecds;firstObj++) {
    shapeGetObj(& shapeFileIn,firstObj,&objOut);
    if (checkObj(objOut) !=0) {
      fprintf(stderr,"Caution: record %d of first file may be invalid."
                     " Skipping.\n",firstObj);
      SHPDestroyObject(objOut);
      continue;
    }
    break;
  }
  if (firstObj >= shapeFileIn.nRecds) {
    SHPDestroyObject(objOut);
    fprintf(stderr,"Caution: All records of first file invalid."
                     " Skipping.\n");
    exit(ERR_FILE);
  }

  for (i= firstObj +1;i < shapeFileIn.nRecds;i++) {
    shapeGetObj(& shapeFileIn,i,&objA);
    if (checkObj(objA) !=0) {
      fprintf(stderr,"Caution: record %d of first file may be invalid."
                     " Skipping.\n",i);
      continue;
    }
    objUnion = objOut;
    findUnion(objA,objUnion,&objOut);
    SHPDestroyObject( objA );objA = NULL;
    SHPDestroyObject( objUnion );objUnion = NULL;

    if (verb_level > 0) printf("Arecd=%d\n",i);
  }
  if (!splitOut) {
   /*Save as Single object with multiple parts*/
    if (verb_level > 0) printf("Output Object has %d parts.\n",objOut->nParts);
    recno =  shapePutObj(& shapeFileOut,objOut);
    for (k=0;k<shapeFileOut.nFields;k++) {
      if ((k==0) && shapeFileOut.f[0].Type==FTInteger) {
        DBFWriteIntegerAttribute(shapeFileOut.hDBF,recno,0,recno);
      }
      switch(shapeFileOut.f[k].Type) {
        case FTInteger:
          DBFWriteIntegerAttribute( shapeFileOut.hDBF, recno, k,
            DBFReadIntegerAttribute( shapeFileIn.hDBF, recno, k) );
          break;
        case FTDouble:
          DBFWriteDoubleAttribute( shapeFileOut.hDBF, recno, k,
                   DBFReadDoubleAttribute( shapeFileIn.hDBF, recno, k) );
          break;
        case FTString:
          DBFWriteStringAttribute( shapeFileOut.hDBF, recno, k,
                   DBFReadStringAttribute( shapeFileIn.hDBF, recno, k) );
          break;
        default:
          break;
      }
    }
  } else { 
#if 1==0
/*Split up into component parts before saving*/
    if (verb_level > 0) printf("Output Object has %d parts.\n",objOut->nParts);
    chain = (link *) malloc ((objOut->nParts)*sizeof(link));

    for(k=0;k<objOut->nParts;k++) {
      chain[k].prev =-1;
      chain[k].layer = 0;
      limitsPartObj(k, objOut,&partMin.x,&partMin.y, &partMax.x,&partMax.y);
      if (verb_level > 0) printf("part %d: (%f,%f) - (%f,%f)\n",
                      k, partMin.x,partMin.y, partMax.x,partMax.y);
    }
    for(k=1; k<objOut->nParts; k++) {
      for (j=0; j<k; j++) {
DEBUG_OUT3("inside function %d,%d yields %d\n",j,k,inside ( j, k, objOut));
        switch (inside ( j, k, objOut)) {
        case -1: /*Part j inside part k.*/
          n = j;
          while (n >= 0 ) {
            m=n;n=chain[m].prev;
            if (n == k) break; /*k's ancestry already recorded.*/
            if (n == -1) {
              chain[m].prev = k; /*record k's ancestry.*/
              break;
            }
            if (inside ( n, k, objOut)) continue;
/*At this stage, k is ancester to m but not to n. Record k
 * as a more immediate ancester.*/
            chain[m].prev = k;
            break;
          }
          break;
        case 1: /*Part k inside part j.*/
          n = k;
          while (n >= 0 ) {
            m=n;n=chain[m].prev;
            if (n == j) break; /*j's ancestry already recorded.*/
            if (n == -1) {
              chain[m].prev = j; /*record j's ancestry.*/
              break;
            }
            if (inside ( n, j, objOut)) continue;
/*At this stage, j is ancester to m but not to n. Record j
 * as a more immediate ancestor.*/
            chain[m].prev = j;
            break;
          default:
            /*printf("inside function yields %d\n",inside ( j, k, objOut));*/
            break;
          }
          break;
        }
      }
    }
    for(k=0; k<objOut->nParts; k++) {
      n = k;
      for (j=0;j<objOut->nParts; j++) {
        m=n;n=chain[m].prev;
        if ( n == -1) {
          chain[k].layer = j;
          break;
        }
      }
    }
    XX = (double *) malloc(objOut->nVertices * sizeof(double));
    YY = (double *) malloc(objOut->nVertices * sizeof(double));
    startVertex = (int *) malloc(objOut->nVertices * sizeof(int));
    for(k=0,recnum=0; k<objOut->nParts; k++,recnum++) {
      if(verb_level>0) printf("index %d, layer %d prev %d\n",k,chain[k].layer,chain[k].prev);
      if ((chain[k].layer & 1) == 0) {
      count = 0;
        addPart(objOut, k, XX, YY, count, startVertex) ;
        for (j=0;j<objOut->nParts; j++) {
          if (chain[j].prev == k) {
            count++;
            addPart(objOut, j, XX, YY, count, startVertex) ;
          }
        }
      }
      objPut = SHPCreateObject( SHPT_POLYGON, recnum, count+1, startVertex,
                       NULL, startVertex[count+1], XX, YY, NULL, NULL);
      recno =  shapePutObj(& shapeFileOut,objPut);
    for (m=0;m<shapeFileOut.nFields;m++) {
      if ((m==0) && shapeFileOut.f[0].Type==FTInteger) {
        DBFWriteIntegerAttribute(shapeFileOut.hDBF,recno,0,recno);
      }
      switch(shapeFileOut.f[m].Type) {
        case FTInteger:
          DBFWriteIntegerAttribute( shapeFileOut.hDBF, recno, m,
            DBFReadIntegerAttribute( shapeFileIn.hDBF, 0, m) );
          break;
        case FTDouble:
          DBFWriteDoubleAttribute( shapeFileOut.hDBF, recno, m,
                   DBFReadDoubleAttribute( shapeFileIn.hDBF, 0, m) );
          break;
        case FTString:
          DBFWriteStringAttribute( shapeFileOut.hDBF, recno, m,
                   DBFReadStringAttribute( shapeFileIn.hDBF, 0, m) );
              /*strcpy(buffr,DBFReadStringAttribute( shapeFileIn.hDBF, recno, m));
 DBFWriteStringAttribute( shapeFileOut.hDBF, recno, m,buffr);*/
          break;
      }
    }
    }
    free(XX);
    free(YY);
    free(startVertex);
    free(chain);
#else
/*Split up into component parts before saving*/
  SHPObject ** objArray;
  int nobj;
    if (verb_level > 0) printf("Output Object has %d parts.\n",objOut->nParts);
    objArray = (SHPObject **) malloc(objOut->nParts * sizeof(SHPObject *));
    nobj = splitObj(objOut,objArray);
    for (n=0;n<nobj;n++) {
      recno =  shapePutObj(& shapeFileOut,objArray[n]);
      SHPDestroyObject(objArray[n]);objArray[n]=NULL;
      for (m=0;m<shapeFileOut.nFields;m++) {
        if ((m==0) && shapeFileOut.f[0].Type==FTInteger) {
          DBFWriteIntegerAttribute(shapeFileOut.hDBF,recno,0,recno);
        } else {
          switch(shapeFileOut.f[m].Type) {
            case FTInteger:
              DBFWriteIntegerAttribute( shapeFileOut.hDBF, recno, m,
                DBFReadIntegerAttribute( shapeFileIn.hDBF, 0, m) );
              break;
            case FTDouble:
              DBFWriteDoubleAttribute( shapeFileOut.hDBF, recno, m,
                       DBFReadDoubleAttribute( shapeFileIn.hDBF, 0, m) );
              break;
            case FTString:
              DBFWriteStringAttribute( shapeFileOut.hDBF, recno, m,
                       DBFReadStringAttribute( shapeFileIn.hDBF, 0, m) );
              break;
          }
        }
      }
    }
    free(objArray);
#endif
  }
  SHPDestroyObject( objOut );objOut = NULL;
  ShapeClose(& shapeFileIn);
  ShapeClose(& shapeFileOut);
  closeShop();
  return 0;
}

