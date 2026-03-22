/*-------------------- shapeuidraw.c -------------------------------------------
 *
 */
#include "config.h"
#include <stdlib.h>
#include <stdio.h>                                       
#include  <shapext.h>
#include <string.h>
#include <shapefil.h>
#include "arrayutil.h"
#include "shapeuidraw.h"
/*#include "utils.h"*/
#include "vector2.h"

/* Object Type codes used in main(): */
/* Error codes for exit() routine: */

#define SIGNOF(x) ((x<0)?-1:((x>0)?1:0))

#define NEWMKINT (1==0)

#define	ERR_USAGE	1

#define DEBUGPR
#undef DEBUGPR

typedef enum {
 drawa,drawb}
abdraw;

 int verb_level;

static arrdouble pdfX={0, 0, NULL},pdfY={0,0, NULL};

static arrint panPartStart={0,0,NULL};

static MKEXTENDARRTYPE(infoIntersect)
static MKRESETARRTYPE(infoIntersect)
/*@unused@*/static MKCLEARARRTYPE(infoIntersect)

static int infoIcompA (const void * a0, const void * b0) ;
static int infoIcompB (const void * a0, const void * b0) ;

static arrinfoIntersect aTable={0,0,NULL},bTable={0,0,NULL};
static arrint aPartInt={0,0,NULL},bPartInt={0,0,NULL};

static void partShapeObj(SHPObject * a, int nPart, int * first, int * last) {
/*returns first and last indices of part npart of object a */
  *first =  a -> panPartStart[nPart];
  if (nPart+1 >= a-> nParts) {
    *last = a->nVertices;
  } else {
    *last = a->panPartStart[nPart+1];
  }
}


static void prInfoIntersect(arrinfoIntersect * a) {
#define PRFLAG(x) printf("%10s",#x);for(k=0;k<a->nused;k++)printf("%10d",(a->arr+k)->x);printf("\n")
#define PRFFLAG(x) printf("%10s",#x);for(k=0;k<a->nused;k++)printf("%10f",(a->arr+k)->x);printf("\n")
  int k;
  if (a->nused > 0) {
  printf("Used:%d, Avail %d\n",a->nused,a->navail);
    PRFLAG(partA);PRFLAG(partB);
    PRFLAG(vertexA);PRFLAG(vertexB);
    PRFFLAG(fractA);PRFFLAG(fractB);
    PRFLAG(crossDir);
    PRFFLAG(xPoint.x);PRFFLAG(xPoint.y);
    PRFLAG(jumpIndex);
  }
}
 
static void cpInfoIntersect(arrinfoIntersect * dest,arrinfoIntersect * src) {
int k;
  if (dest->navail < src->nused)
          extendArrinfoIntersect(dest,src->nused -dest->navail);
  for (k=0;k<src->nused;k++) (dest->arr)[k] = src->arr[k];
  dest->nused = src->nused;
}


static void addVertex(double x,double y) {
  if (pdfX.nused >= pdfX.navail) {
    extendArrdouble(&pdfX,10);
    extendArrdouble(&pdfY,10);
  }
  pdfX.arr[pdfX.nused] = x;
  pdfY.arr[pdfX.nused] = y;
  pdfY.nused = pdfX.nused = pdfX.nused + 1;
}


static void addPart(int pointPtr) {
  if (panPartStart.nused >= panPartStart.navail) extendArrint(&panPartStart,10);
  panPartStart.arr[panPartStart.nused] = pointPtr;
  panPartStart.nused = panPartStart.nused + 1;
}


static int nxtJump(arrinfoIntersect * info,int jumpIndex,abdraw drawab ) {
int part,result;
  switch (drawab) {
  case drawa:
    part = info->arr[jumpIndex].partA;
    result = ((jumpIndex+1) < info->nused)?(jumpIndex+1):0;
    while (info->arr[result].partA != part) {
      result = ((result + 1) < info->nused)?(result + 1):0;
    }
    if (info->arr[result].jumpIndex < 0) return -1;
    break;
    
  case drawb:
    part = info->arr[jumpIndex].partB;
    result = ((jumpIndex+1) < info->nused)?(jumpIndex+1):0;
    while (info->arr[result].partB != part) {
      result = ((result + 1) < info->nused)?(result + 1):0;
    }
    if (info->arr[result].jumpIndex < 0) return -1;
    break;
  default:
    result=-1;
    break;
  }
  if (result == jumpIndex) return -1;
  return result;
}

void findUnion(SHPObject * objA,SHPObject * objB,SHPObject ** objOut) {
/*For Polygon Type Objects objA and objB, return objOut bounding their union.*/
int isTable;
  resetArrinfoIntersect(&aTable);
  isTable = mkIntersectTable(objA, objB,  &aTable, &aPartInt, &bPartInt);
  drawUnion(&aTable,& bTable,objA,objB,objOut, &aPartInt, &bPartInt);
}

void printObject(SHPObject * obj,char * text) {
int j,k0,k1,k;
  printf("Printing Object %s\n",text);
  for (j=0;j<obj->nParts;j++) {
    partShapeObj(obj, j, &k0, &k1) ;
    printf("part %d,indices %d through %d\n",j,k0,k1);
    for (k=k0;k<k1;k++) {
      printf ("%d %lf %lf\n",k,obj->padfX[k],obj->padfY[k]);
    }
  }
}

void drawUnion(arrinfoIntersect * a,arrinfoIntersect * b,
                    SHPObject * objA,SHPObject * objB,SHPObject ** objOut,
                    arrint * iA, arrint * iB) {
int jumpIndex=0,tempi;
/* int jpI2; */
abdraw drawab=drawa;
int partno;
static int first,last;
int outPart=0,outVertex=0;
int k,j;

/*Sanity Checking Polygon Intersect Table*/

/*Intersect table should have an even number of entries.*/
  if ((a->nused & 1) != 0) {
    if(verb_level>0) printf("Caution: Odd Count in Table of Polygon Intersections.\n");
    fprintf(stderr,"Caution: Odd Count in Table of Polygon Intersections.\n");
/*    printIntersectTable(a);*/
    prInfoIntersect(a);
    printObject(objA,"Object A");
    printObject(objB,"Object B");
/*    exit(1);*/
  }

/*Sort the intersect table*/
  qsort((void *) a->arr, (size_t) a->nused, sizeof(infoIntersect), infoIcompA);

/*Set up jump indices*/
  if (a->nused>0) {
  int aPart,bPart;
    for (k = 0; k < a->nused; k++) a->arr[k].jumpIndex = k;
/* Sanity check indices.  A crossover area is>0 if crossing from outside to
 * inside, <0 from outside to inside, and ==0 if not crossing. Signs of
 *  crossover areas from the same part must alternate.*/
    for (k=0,tempi=SIGNOF(a->arr[a->nused-1].crossDir),
               aPart=a->arr[a->nused-1].partA;  k < a->nused; k++) {
      if (SIGNOF(a->arr[k].crossDir) == 0) {
        if(verb_level>0) printf("A area %d zero.\n",k);
      }
      if (tempi == SIGNOF(a->arr[k].crossDir)) {
        if (a->arr[k].partA == aPart) {
          if(verb_level>0) printf("A areas %d and %d same sign:"
                                " Intersect Table Bad.\n",
                ((k>0)?k-1:a->nused-1),k);
          fprintf(stderr,"A areas %d and %d same sign: Intersect Table Bad.\n",
                ((k>0)?k-1:a->nused-1),k);
        }
      }
      tempi = SIGNOF(a->arr[k].crossDir);
      aPart = a->arr[k].partA;
    }

/*Set up jump back table. (Create b table and sort it according to Polygon b.*/
    cpInfoIntersect(b, a);
    qsort((void *) b->arr, (size_t) b->nused, sizeof(infoIntersect), infoIcompB);

/* Set up jump back indices from b to a.*/
    for (k = 0; k < a->nused;k++) {
      j = b->arr[k].jumpIndex;
      a->arr[j].jumpIndex = k;
    }
    if (verb_level > 0) {printf ("\nA-table\n"); prInfoIntersect(a);}

/* Sanity check indices.  A crossover area is>0 if crossing from outside to
 * inside, <0 from outside to inside, and ==0 if not crossing. Signs must
 *  alternate.*/
    for (k=0,tempi=SIGNOF(b->arr[b->nused-1].crossDir),
            bPart=b->arr[b->nused-1].partB; k < b->nused;k++) {
      if (SIGNOF(b->arr[k].crossDir) == 0) {
        if(verb_level>0) printf("B area %d zero.\n",k);
      }
      if (tempi == SIGNOF(b->arr[k].crossDir)) {
        if (b->arr[k].partB == bPart) {
          if(verb_level > 0) printf("B areas %d and %d same sign:"
                                " Intersect Table Bad.\n",
                ((k>0)?k-1:b->nused-1),k);
          fprintf(stderr,"B areas %d and %d same sign: Intersect Table Bad.\n",
                ((k>0)?k-1:b->nused-1),k);
          printf ("\nA-table\n"); prInfoIntersect(a);
          printf ("\nB-table\n"); prInfoIntersect(b);
printObject( objA,"Object A");
printObject( objB,"Object B");
        }
      }
      tempi = SIGNOF(b->arr[k].crossDir);
      bPart = b->arr[k].partB;
    }
    if (verb_level > 0) {printf ("\nB-table\n"); prInfoIntersect(b);}
  } /* end of a->nused > 0 */
/*---------------------------------------------------------------------*/

/*Reset arrays for polygons with which to create output objects.*/
  resetArrdouble(&pdfX);resetArrdouble(&pdfY);
  resetArrint(&panPartStart);
  addPart(0);
/*---------------------------------------------------------------------*/
  for (outPart = 0; ; outPart++) {
/*Look for first vertex of current part.*/
    for (j = 0, drawab = drawa, jumpIndex = -1; j < a->nused;j++) {
      if (a->arr[j].jumpIndex < 0) continue;
      jumpIndex = j;break;
    }
    if (verb_level > 1) printf("outPart=%d jumpIndex: %d drawab:%d\n",
                                  outPart,jumpIndex,drawab);
    if (jumpIndex < 0) break; /*No more intersections; we're done.*/
    if (a->arr[jumpIndex].crossDir < 0.) {/*Switch to objB*//*Union Area Test*/
      drawab = drawb;
      jumpIndex = a->arr[jumpIndex].jumpIndex;
    }

/*Trace rest of current part.*/
    while (jumpIndex >= 0) {
    int jpI2;
    if (verb_level > 1) printf("jumpIndex: %d drawab:%d\n",jumpIndex,drawab);
/*Draw next segment of current part.*/

      switch (drawab) {

      case drawa:  /*draw segment from objA*/
        /*If area > 0, cross over to B.*/
        if (a->arr[jumpIndex].crossDir < 0.) {/*Switch to objB*/ /*Union Area Test*/
          drawab = drawb;
          j = a->arr[jumpIndex].jumpIndex;
          a->arr[jumpIndex].jumpIndex=-1;jpI2 = jumpIndex = j;
          break;
        } else if (a->arr[jumpIndex].crossDir == 0.) {
          /* Look ahead to next Intersection, cross if next <0. */
          jpI2 = nxtJump(a, jumpIndex, drawab);
          if (a->arr[jpI2].crossDir > 0.) {/*Switch to objB*/ /*Union Area Test*/
            drawab = drawb;
            j = a->arr[jumpIndex].jumpIndex;
            a->arr[jumpIndex].jumpIndex=-1;jumpIndex=j;
          break;
          } else { /*Flag objB side done before continuing objA.*/
            b->arr[a->arr[jumpIndex].jumpIndex].jumpIndex=-1;
          }
        }


        addVertex(a->arr[jumpIndex].xPoint.x,a->arr[jumpIndex].xPoint.y);
        if (verb_level > 0) printf("x:%f,%f\n",
              a->arr[jumpIndex].xPoint.x,a->arr[jumpIndex].xPoint.y);
        if ((jpI2 = nxtJump(a, jumpIndex, drawab)) < 0) break ;
        partno = a->arr[jumpIndex].partA;
        partShapeObj(objA, partno, &first, &last) ;
        outVertex = a->arr[jumpIndex].vertexA;
        if (verb_level > 1) printf("Limits %d,%d %d,%d %d,%d\n",first,last,
              a->arr[jumpIndex].vertexA,a->arr[jpI2].vertexA,jumpIndex,jpI2);
        while (outVertex != a->arr[jpI2].vertexA) {
          outVertex = ((outVertex+1)<(last-1))?outVertex+1:first;
          addVertex (objA->padfX[outVertex],objA->padfY[outVertex]);
          if (verb_level > 0) printf("a %d: %f %f\n",outVertex,
              objA->padfX[outVertex],objA->padfY[outVertex]);
        }
/*Mark last index*/
        a->arr[jumpIndex].jumpIndex=-1;
        jumpIndex = jpI2;
        break;

      case drawb: /*draw segment from objB*/
        if (b->arr[jumpIndex].crossDir > 0.) {/*Switch to objA*/ /*Union Area Test*/
          drawab = drawa;
          j = b->arr[jumpIndex].jumpIndex;
          b->arr[jumpIndex].jumpIndex=-1;jpI2 = jumpIndex = j;
          break;
        } else if (b->arr[jumpIndex].crossDir == 0.) {
          /* Look ahead to next Intersection, cross if next <0. */
          jpI2 = nxtJump(b, jumpIndex, drawab);
          if (b->arr[jpI2].crossDir < 0.) {/*Switch to objA*/ /*Union Area Test*/
            drawab = drawa;
            j = b->arr[jumpIndex].jumpIndex;
            b->arr[jumpIndex].jumpIndex=-1;jumpIndex=j;
          break;
          } else { /*Flag objA side done before continuing objB.*/
            a->arr[b->arr[jumpIndex].jumpIndex].jumpIndex=-1;
          }

        }

        addVertex(b->arr[jumpIndex].xPoint.x,b->arr[jumpIndex].xPoint.y);
        if (verb_level > 0) printf("x:%f,%f\n",
              b->arr[jumpIndex].xPoint.x,b->arr[jumpIndex].xPoint.y);
        if ((jpI2 = nxtJump(b, jumpIndex, drawab)) < 0) break ;
        partno = b->arr[jumpIndex].partB;
        partShapeObj(objB, partno, &first, &last) ;
        outVertex = b->arr[jumpIndex].vertexB;
        if (verb_level > 1) printf("Limits %d,%d %d,%d %d,%d\n",first,last,
              b->arr[jumpIndex].vertexB,b->arr[jpI2].vertexB,jumpIndex,jpI2);
        while (outVertex != b->arr[jpI2].vertexB) {
          outVertex = ((outVertex+1)<(last-1))?outVertex+1:first;
          addVertex (objB->padfX[outVertex],objB->padfY[outVertex]);
          if (verb_level > 0) printf("b %d: %f %f\n", outVertex,
              objB->padfX[outVertex],objB->padfY[outVertex]);
        }

/*Mark last index*/
        b->arr[jumpIndex].jumpIndex=-1;
        jumpIndex = jpI2;
        break;
      default:
        fprintf(stderr,"Invalid value for drawab=%d\n",drawab);
        exit(1);
      } /* end switch drawab */

      if (jumpIndex <0 || jpI2 < 0 ) break; /*Done with current part.*/
    }

/*finish up current output part*/
/*Add to panPartStart*/
    addPart(pdfX.nused);
    if (verb_level > 0) {int ii,jj;
    printf("OutVertex %d,%d nparts %d\n",panPartStart.arr[0],
         panPartStart.arr[panPartStart.nused-1],panPartStart.nused);
      ii = panPartStart.arr[panPartStart.nused-2];
      jj = panPartStart.arr[panPartStart.nused-1]-1;
      printf ("First %f,%f; Last %f,%f; Diff=%f,%f\n",
                pdfX.arr[ii],pdfY.arr[ii],
                pdfX.arr[jj],pdfY.arr[jj],
                pdfX.arr[ii]-pdfX.arr[jj],pdfY.arr[ii]-pdfY.arr[jj]);
    }
  } /* end outPart Loop */
/*---------------------------------------------------------*/

/* Now add parts, if any, that are totally outside with no intersections:*/
  for (partno=0; partno < objA->nParts; partno++) {
    if (iA->arr[partno] == 0) {
    /*Add part partno of objA*/
      if (verb_level>0) printf("Drawing from objA\n");
      partShapeObj(objA, partno, &first, &last) ;
      for (outVertex=first; outVertex < last; outVertex++) {
        if (verb_level>0) printf("Adding %d: %f,%f\n",
                          outVertex,objA->padfX[outVertex],objA->padfY[outVertex]);
        addVertex (objA->padfX[outVertex],objA->padfY[outVertex]);
      }
      /*Add to panPartStart*/
      addPart(pdfX.nused);
    }
  }
    if (verb_level >0) {int ii,jj;
    printf("iA OutVertex %d,%d nparts %d\n",panPartStart.arr[0],
         panPartStart.arr[panPartStart.nused-1],panPartStart.nused);
    fflush(stdout);
      if( panPartStart.nused>1) {
        ii = panPartStart.arr[panPartStart.nused-2];
        jj = panPartStart.arr[panPartStart.nused-1]-1;
        if (verb_level > 0) printf ("First %f,%f; Last %f,%f; Diff=%f,%f\n",
                pdfX.arr[ii],pdfY.arr[ii],
                pdfX.arr[jj],pdfY.arr[jj],
                pdfX.arr[ii]-pdfX.arr[jj],pdfY.arr[ii]-pdfY.arr[jj]);
      } else {
        printf("Few Parts\n");
      }
    }

  for (partno=0; partno < objB->nParts; partno++) {
    if (iB->arr[partno] == 0) {
    /*Add part partno of objB*/
      if (verb_level>0) printf("Drawing from objB\n");
      partShapeObj(objB, partno, &first, &last) ;
      for (outVertex=first; outVertex < last; outVertex++) {
        if (verb_level > 0) printf("Adding %d: %f,%f\n",
                       outVertex,objB->padfX[outVertex],objB->padfY[outVertex]);
        addVertex (objB->padfX[outVertex],objB->padfY[outVertex]);
      }
      /*Add to panPartStart*/
      addPart(pdfX.nused);
    }
  }
    if(verb_level > 0) {int ii,jj;
    printf("iB OutVertex %d,%d nparts %d\n",panPartStart.arr[0],
         panPartStart.arr[panPartStart.nused-1],panPartStart.nused);
      if( panPartStart.nused>1) {
        ii = panPartStart.arr[panPartStart.nused-2];
        jj = panPartStart.arr[panPartStart.nused-1]-1;
        printf ("First %f,%f; Last %f,%f Diff = %f,%f\n",
                pdfX.arr[ii],pdfY.arr[ii],
                pdfX.arr[jj],pdfY.arr[jj],
                pdfX.arr[ii]-pdfX.arr[jj],pdfY.arr[ii]-pdfY.arr[jj]);
      } else {
        printf("Few Parts\n");
      }
    }

  /*All parts drawn - create shape file object.*/
  *objOut = SHPCreateObject( SHPT_POLYGON, 0 /*id*/,
     panPartStart.nused-1,panPartStart.arr,NULL,
     panPartStart.arr[panPartStart.nused-1],pdfX.arr,pdfY.arr,NULL,NULL);
/*printf("Leaving drawUnion\n");fflush(stdout);*/
}

/* ------------------------ End drawUnion -------------------------- */

/* ------------------------- findIntersect ------------------------- */
int findIntersect(SHPObject * objA,SHPObject * objB,SHPObject ** objOut) {
/*For Polygon Type Objects objA and objB, If intersection of the objects
 * exists, return intersection as *objOut and return true.  If not,
 * set *objOut to NULL and return false.
 * the intersection of their interiors.  If in
 */
int isTable;
#ifdef DEBUGPR
fprintf(stderr,"Enter findIntersect\n");fflush(stderr);
#endif
  resetArrinfoIntersect(&aTable);
  isTable = mkIntersectTable(objA, objB,  &aTable, &aPartInt, &bPartInt);
  if (isTable != 0) {
    drawIntersect(&aTable,& bTable,objA,objB,objOut, &aPartInt, &bPartInt);
  } else {
    *objOut = NULL;
  }
  return isTable;
}
/* ------------------------End findIntersect ----------------------- */

/* ------------------------- drawIntersect -------------------------- */

void drawIntersect(arrinfoIntersect * a,arrinfoIntersect * b,
                    SHPObject * objA,SHPObject * objB,SHPObject ** objOut,
                    arrint * iA, arrint * iB) {
int jumpIndex=0,tempi;
abdraw drawab = drawa;
int partno;
static int first,last;
int outPart=0,outVertex=0;
int k,j;
#ifdef DEBUGPR
fprintf(stderr,"Enter drawIntersect\n");fflush(stderr);
#endif
/*Intersect table should have an even number of entries.*/
  if ((a->nused & 1) != 0) {
    if(verb_level>0) printf("Caution: Odd Count in Intersect Table\n");
    fprintf(stderr,"Caution: Odd Count in Intersect Table\n");
  }

/*Sort the intersect table, copy to b and resort it.*/
  qsort((void *) a->arr, (size_t) a->nused, sizeof(infoIntersect), infoIcompA);

/*Set up jump indices*/
  if (a->nused>0) {
    for (k = 0; k < a->nused; k++) a->arr[k].jumpIndex = k;
    for (k=0,tempi=SIGNOF(a->arr[a->nused-1].crossDir); k < a->nused;k++) {
      if (SIGNOF(a->arr[k].crossDir) == 0) {
        if(verb_level>0) printf("A area %d zero.\n",k);
      }
      if (tempi == SIGNOF(a->arr[k].crossDir)) {
        if(verb_level>0) printf("A areas %d and %d same sign: Intersect Table Bad.\n",
                ((k>0)?k-1:a->nused-1),k);
          fprintf(stderr,"A areas %d and %d same sign: Intersect Table Bad.\n",
                ((k>0)?k-1:a->nused-1),k);
      }
      tempi = SIGNOF(a->arr[k].crossDir);
    }
/*Set up jump back table.*/
    cpInfoIntersect(b, a);
    qsort((void *) b->arr, (size_t) b->nused, sizeof(infoIntersect), infoIcompB);
    for (k = 0; k < a->nused;k++) {
      j = b->arr[k].jumpIndex;
      a->arr[j].jumpIndex = k;
    }
    if (verb_level > 0) {printf ("\nA-table\n"); prInfoIntersect(a);}
    for (k=1,tempi=SIGNOF(b->arr[0].crossDir); k < b->nused;k++) {
      if (SIGNOF(b->arr[k].crossDir) == 0) {
        if(verb_level>0) printf("B area %d zero.\n",k);
      }
      if (tempi == SIGNOF(b->arr[k].crossDir)) {
        if(verb_level > 0) printf("B areas %d and %d same sign: Intersect Table Bad.\n",
              ((k>0)?k-1:b->nused-1),k);
        fprintf(stderr,"B areas %d and %d same sign: Intersect Table Bad.\n",
              ((k>0)?k-1:b->nused-1),k);
      }
      tempi = SIGNOF(b->arr[k].crossDir);
    }
    if (verb_level > 0) {printf ("\nB-table\n"); prInfoIntersect(b);}
  }

/*Reset coordinate arrays*/
  resetArrdouble(&pdfX);resetArrdouble(&pdfY);
  resetArrint(&panPartStart);
  addPart(0);

  for (outPart = 0; ; outPart++) {
/*Look for first vertex of current part.*/
    for (j = 0, drawab = drawa, jumpIndex = -1; j < a->nused;j++) {
      if (a->arr[j].jumpIndex < 0) continue;
      jumpIndex = j;break;
    }
    if (verb_level > 1) printf("outPart=%d jumpIndex: %d drawab:%d\n",
                                  outPart,jumpIndex,drawab);
    if (jumpIndex < 0) break; /*No more intersections; we're done.*/
    if (a->arr[jumpIndex].crossDir > 0.) {/*Switch to objB*/
      drawab = drawb;
      jumpIndex = a->arr[jumpIndex].jumpIndex;
    }

/*Trace rest of current part.*/
    while (jumpIndex >= 0) {
    int jpI2;
    if (verb_level > 1) printf("jumpIndex: %d drawab:%d\n",jumpIndex,drawab);
/*Draw next segment of current part.*/

    switch (drawab) {

    case drawa:  /*draw segment from objA*/
      /*If area > 0, cross over to B.*/
      if (a->arr[jumpIndex].crossDir > 0.) {/*Switch to objB*/
        drawab = drawb;
        j = a->arr[jumpIndex].jumpIndex;
        a->arr[jumpIndex].jumpIndex=-1;jpI2 = jumpIndex = j;
        break;
      } else if (a->arr[jumpIndex].crossDir == 0.) {
        /* Look ahead to next Intersection, cross if next <0. */
        jpI2 = nxtJump(a, jumpIndex, drawab);
        if (a->arr[jpI2].crossDir < 0.) {/*Switch to objB*/
          drawab = drawb;
          j = a->arr[jumpIndex].jumpIndex;
          a->arr[jumpIndex].jumpIndex=-1;jumpIndex=j;
        break;
        } else { /*Flag objB side done before continuing objA.*/
          b->arr[a->arr[jumpIndex].jumpIndex].jumpIndex=-1;
        }
      }


      addVertex(a->arr[jumpIndex].xPoint.x,a->arr[jumpIndex].xPoint.y);
      if (verb_level > 0) printf("x:%f,%f\n",
            a->arr[jumpIndex].xPoint.x,a->arr[jumpIndex].xPoint.y);
      if ((jpI2 = nxtJump(a, jumpIndex, drawab)) < 0) break ;
      partno = a->arr[jumpIndex].partA;
      partShapeObj(objA, partno, &first, &last) ;
      outVertex = a->arr[jumpIndex].vertexA;
      if (verb_level > 1) printf("Limits %d,%d %d,%d %d,%d\n",first,last,
            a->arr[jumpIndex].vertexA,a->arr[jpI2].vertexA,jumpIndex,jpI2);
      while (outVertex != a->arr[jpI2].vertexA) {
        outVertex = ((outVertex+1)<(last-1))?outVertex+1:first;
        addVertex (objA->padfX[outVertex],objA->padfY[outVertex]);
        if (verb_level > 0) printf("a %d: %f %f\n",outVertex,
            objA->padfX[outVertex],objA->padfY[outVertex]);
      }
/*Mark last index*/
      a->arr[jumpIndex].jumpIndex=-1;
      jumpIndex = jpI2;
      break;

    case drawb: /*draw segment from objB*/
      if (b->arr[jumpIndex].crossDir < 0.) {/*Switch to objA*/
        drawab = drawa;
        j = b->arr[jumpIndex].jumpIndex;
        b->arr[jumpIndex].jumpIndex=-1;jpI2 = jumpIndex = j;
        break;
      } else if (b->arr[jumpIndex].crossDir == 0.) {
        /* Look ahead to next Intersection, cross if next <0. */
        jpI2 = nxtJump(b, jumpIndex, drawab);
        if (b->arr[jpI2].crossDir > 0.) {/*Switch to objA*/
          drawab = drawa;
          j = b->arr[jumpIndex].jumpIndex;
          b->arr[jumpIndex].jumpIndex=-1;jumpIndex=j;
        break;
        } else { /*Flag objA side done before continuing objB.*/
          a->arr[b->arr[jumpIndex].jumpIndex].jumpIndex=-1;
        }

      }

      addVertex(b->arr[jumpIndex].xPoint.x,b->arr[jumpIndex].xPoint.y);
      if (verb_level > 0) printf("x:%f,%f\n",
            b->arr[jumpIndex].xPoint.x,b->arr[jumpIndex].xPoint.y);
      if ((jpI2 = nxtJump(b, jumpIndex, drawab)) < 0) break ;
      partno = b->arr[jumpIndex].partB;
      partShapeObj(objB, partno, &first, &last) ;
      outVertex = b->arr[jumpIndex].vertexB;
      if (verb_level > 1) printf("Limits %d,%d %d,%d %d,%d\n",first,last,
            b->arr[jumpIndex].vertexB,b->arr[jpI2].vertexB,jumpIndex,jpI2);
      while (outVertex != b->arr[jpI2].vertexB) {
        outVertex = ((outVertex+1)<(last-1))?outVertex+1:first;
        addVertex (objB->padfX[outVertex],objB->padfY[outVertex]);
        if (verb_level > 0) printf("b %d: %f %f\n", outVertex,
            objB->padfX[outVertex],objB->padfY[outVertex]);
      }

/*Mark last index*/
      b->arr[jumpIndex].jumpIndex=-1;
      jumpIndex = jpI2;
      break;
    default:
      fprintf(stderr,"Invalid value for drawab=%d\n",drawab);
      exit(1);
    } /* end switch drawab */
    if (jumpIndex <0 || jpI2 < 0 ) break; /*Done with current part.*/
  }

/*finish up current output part*/
/*Add to panPartStart*/
  addPart(pdfX.nused);
  if (verb_level > 0) {int ii,jj;
  printf("OutVertex %d,%d nparts %d\n",panPartStart.arr[0],
       panPartStart.arr[panPartStart.nused-1],panPartStart.nused);
    ii = panPartStart.arr[panPartStart.nused-2];
    jj = panPartStart.arr[panPartStart.nused-1]-1;
    printf ("First %f,%f; Last %f,%f; Diff=%f,%f\n",
              pdfX.arr[ii],pdfY.arr[ii],
              pdfX.arr[jj],pdfY.arr[jj],
              pdfX.arr[ii]-pdfX.arr[jj],pdfY.arr[ii]-pdfY.arr[jj]);
  }
}

/*Now add parts, if any, that are totally inside with no intersections:*/
  for (partno=0; partno < objA->nParts; partno++) {
    if (iA->arr[partno] == 2) {
    /*Add part partno of objA*/
      if (verb_level>0) printf("Drawing from objA\n");
      partShapeObj(objA, partno, &first, &last) ;
      for (outVertex=first; outVertex < last; outVertex++) {
        if (verb_level>0) printf("Adding %d: %f,%f\n",
                          outVertex,objA->padfX[outVertex],objA->padfY[outVertex]);
        addVertex (objA->padfX[outVertex],objA->padfY[outVertex]);
      }
      /*Add to panPartStart*/
      addPart(pdfX.nused);
    }
  }
    if (verb_level >0) {int ii,jj;
    printf("iA OutVertex %d,%d nparts %d\n",panPartStart.arr[0],
         panPartStart.arr[panPartStart.nused-1],panPartStart.nused);
    fflush(stdout);
      if( panPartStart.nused>1) {
        ii = panPartStart.arr[panPartStart.nused-2];
        jj = panPartStart.arr[panPartStart.nused-1]-1;
        if (verb_level > 0) printf ("First %f,%f; Last %f,%f; Diff=%f,%f\n",
                pdfX.arr[ii],pdfY.arr[ii],
                pdfX.arr[jj],pdfY.arr[jj],
                pdfX.arr[ii]-pdfX.arr[jj],pdfY.arr[ii]-pdfY.arr[jj]);
      } else {
        printf("Few Parts\n");
      }
    }

  for (partno=0; partno < objB->nParts; partno++) {
    if (iB->arr[partno] == 2) {
    /*Add part partno of objB*/
      if (verb_level>0) printf("Drawing from objB\n");
      partShapeObj(objB, partno, &first, &last) ;
      for (outVertex=first; outVertex < last; outVertex++) {
        if (verb_level > 0) printf("Adding %d: %f,%f\n",
                       outVertex,objB->padfX[outVertex],objB->padfY[outVertex]);
        addVertex (objB->padfX[outVertex],objB->padfY[outVertex]);
      }
      /*Add to panPartStart*/
      addPart(pdfX.nused);
    }
  }
    if(verb_level > 0) {int ii,jj;
    printf("iB OutVertex %d,%d nparts %d\n",panPartStart.arr[0],
         panPartStart.arr[panPartStart.nused-1],panPartStart.nused);
      if( panPartStart.nused>1) {
        ii = panPartStart.arr[panPartStart.nused-2];
        jj = panPartStart.arr[panPartStart.nused-1]-1;
        printf ("First %f,%f; Last %f,%f Diff = %f,%f\n",
                pdfX.arr[ii],pdfY.arr[ii],
                pdfX.arr[jj],pdfY.arr[jj],
                pdfX.arr[ii]-pdfX.arr[jj],pdfY.arr[ii]-pdfY.arr[jj]);
      } else {
        printf("Few Parts\n");
      }
    }

  /*All parts drawn - create shape file object.*/
  *objOut = SHPCreateObject( SHPT_POLYGON, 0 /*id*/,
     panPartStart.nused-1,panPartStart.arr,NULL,
     panPartStart.arr[panPartStart.nused-1],pdfX.arr,pdfY.arr,NULL,NULL);
/*printf("Leaving drawIntersect\n");fflush(stdout);*/
}

/* -------------------End of DrawIntersect -------------------------- */

/*Functions used by qsort to determine sequencing of intersections along
 * the "A" polygon and along the "B" polygon.
 */

static int infoIcompA (const void * a0, const void * b0) {
infoIntersect * a = (infoIntersect *) a0;
infoIntersect * b = (infoIntersect *) b0;
  if(a->partA > b->partA) return 3; if(a->partA < b->partA) return -3;
  if(a->vertexA > b->vertexA) return 2; if(a->vertexA < b->vertexA) return -2;
  if(a->fractA > b->fractA) return 1; if(a->fractA < b->fractA) return -1;
  return 0;
}

static int infoIcompB (const void * a0, const void * b0) {
infoIntersect * a = (infoIntersect *) a0;
infoIntersect * b = (infoIntersect *) b0;
  if(a->partB > b->partB) return 3; if(a->partB < b->partB) return -3;
  if(a->vertexB > b->vertexB) return 2; if(a->vertexB < b->vertexB) return -2;
  if(a->fractB > b->fractB) return 1; if(a->fractB < b->fractB) return -1;
  return 0;
}

/*==================================================================*/

#if NEWMKINT

static int vector2BoxOverlap(vector2 a0,vector2 a1, vector2 b0, vector2 b1) {
/* Returns TRUE if the box defined by amin and amax overlaps the box
 * defined by bmin and bmax
 */
  vector2 amax,amin,bmax,bmin;
  setVector2(a0.x>a1.x?a0.x:a1.x,a0.y>a1.y?a0.y:a1.y,&amax);
  setVector2(a0.x<a1.x?a0.x:a1.x,a0.y<a1.y?a0.y:a1.y,&amin);
  setVector2(b0.x>b1.x?b0.x:b1.x,b0.y>b1.y?b0.y:b1.y,&bmax);
  setVector2(b0.x<b1.x?b0.x:b1.x,b0.y<b1.y?b0.y:b1.y,&bmin);
  if ( (amax.x < bmin.x) || (bmax.x < amin.x)) return FALSE;
  if ( (amax.y < bmin.y) || (bmax.y < amin.y)) return FALSE;
  return TRUE;
}


/*-----------MKINTERSECT TABLE ---------------------------------------------*/

int mkIntersectTable(SHPObject * objA,SHPObject * objB,
                      arrinfoIntersect * arrInfoA,
                      arrint * arrIClassA,arrint * arrIClassB) {
/*objA and objB are shape file records of (possibly) multi-part polynomials*/
int aPart,bPart;
int firstAVertex,lastAVertex,firstBVertex,lastBVertex;
int aVertex,bVertex;
vector2 vAmax,vAmin;
int result=0;

  if ((objA->nParts <= 0)||(objB->nParts <= 0)) {
    fprintf(stderr,"Object Type Error in mkIntersectTable\n");
    exit(-1);
  }
  resetArrint(arrIClassA);resetArrint(arrIClassB);
  extendArrint(arrIClassA,objA->nParts - arrIClassA->navail);
  extendArrint(arrIClassB,objB->nParts - arrIClassB->navail);
  for(aPart = 0;aPart < objA->nParts; aPart++) arrIClassA->arr[aPart] = 0;
  arrIClassA->nused = objA->nParts;
  for(bPart = 0;bPart < objB->nParts;bPart++) arrIClassB->arr[bPart] = 0;
  arrIClassB->nused = objB->nParts;
  if ( shpBoxOverlap(objA,objB)) {
/* IF OBJECTS CAN OVERLAP, CHECKOUT OBJECT FURTHER--------------*/
    for (aPart = 0; aPart < objA->nParts; aPart++) {
      double aBoxXmin,aBoxYmin,aBoxXmax,aBoxYmax;
      firstAVertex = objA->panPartStart[aPart];
      if (aPart+1 <  objA->nParts) lastAVertex = objA->panPartStart[aPart+1];
        else lastAVertex = objA->nVertices;
/* Establish Bounds for current a Part:---------------------------*/
      for (aVertex = firstAVertex,
                     aBoxXmin=aBoxXmax=objA->padfX[firstAVertex],
                     aBoxYmin=aBoxYmax=objA->padfY[firstAVertex];
           aVertex < lastAVertex; aVertex++) {
        aBoxXmin = (aBoxXmin > objA->padfX[aVertex])?objA->padfX[aVertex]:aBoxXmin;
        aBoxYmin = (aBoxYmin > objA->padfY[aVertex])?objA->padfY[aVertex]:aBoxYmin;
        aBoxXmax = (aBoxXmax < objA->padfX[aVertex])?objA->padfX[aVertex]:aBoxXmax;
        aBoxYmax = (aBoxYmax < objA->padfY[aVertex])?objA->padfY[aVertex]:aBoxYmax;
      }
      setVector2(aBoxXmin,aBoxYmin,&vAmin);
      setVector2(aBoxXmax,aBoxYmax,&vAmax);
/*Bounding Box of A part aPart: vAmin to vAmax--------------------------*/

/*Look for elements of objB that intersect this part of A*/
      for (bPart = 0; bPart < objB->nParts; bPart++) {
        vector2 vB0,vB1;
        firstBVertex = objB->panPartStart[bPart];
        if (bPart+1 <  objB->nParts) lastBVertex = objB->panPartStart[bPart+1];
          else lastBVertex = objB->nVertices;
  /*Current part of Polygon Shapea between firstAVertex and lastAVertex-1 */
  /*Check for segments in current part of B that intersect current part of A*/
        for (bVertex = firstBVertex; bVertex < lastBVertex-1; bVertex++) {
          setVector2(objB->padfX[bVertex],objB->padfY[bVertex],&vB0);
          setVector2(objB->padfX[bVertex+1],objB->padfY[bVertex+1],&vB1);

/*If segment does not intersect bounding Box of this part of A, go to next.*/
          if (! vector2BoxOverlap(vAmin,vAmax,vB0,vB1)) continue;
        /*Otherwise, cycle through segments of this part of A to find any
          intersections*/
          for (aVertex = firstAVertex; aVertex < lastAVertex-1; aVertex++) {
            double sA,tB,crossArea;
            vector2 vA0,vA1,vJoin;
            infoIntersect * a;
            int sectret;
            setVector2(objA->padfX[aVertex],objA->padfY[aVertex],&vA0);
            setVector2(objA->padfX[aVertex+1],objA->padfY[aVertex+1],&vA1);
            if (! vector2BoxOverlap(vA0,vA1,vB0,vB1)) continue;
   /*Bounding Box of these two segments overlaps; do they intersect?*/
            if ((sectret = intersect(&vA0,&vA1,&vB0,&vB1,&sA,&tB,&crossArea,&vJoin))!=0) {
            int crossDir;
            crossDir = (crossArea>0.)?1:((crossArea<0.)?-1:0);
             vector2 tempA=vA0,tempB=vB0;
              if (sectret != 15) {
/*                printf("sectret=%d (0x%x)\n",sectret,sectret);*/
                if ((sectret & 5) != 5) continue;
                if ((sectret & 3) != 3) {
                  if (aVertex > firstAVertex) {
                    setVector2(objA->padfX[aVertex-1],objA->padfY[aVertex-1],&tempA);
                  } else {
                    setVector2(objA->padfX[lastAVertex-2],objA->padfY[lastAVertex-2],&tempA);
                  }
                }
                if ((sectret & 12) != 12) {
                  if (bVertex > firstBVertex) {
                    setVector2(objB->padfX[bVertex-1],objB->padfY[bVertex-1],&tempB);
                  } else {
                    setVector2(objB->padfX[lastBVertex-2],objB->padfY[lastBVertex-2],&tempB);
                  }
                }
                if ( (crossDir=checkCross(&vJoin, &tempA,&vA1,&tempB,&vB1)) == 0) continue ;
              }

   /*Intersection found. Add intersection entry to table.*/
              if ( ((sA==0) || (sA==1)) || ((tB==0)||(tB==1)) ) {
#ifdef DEBUGGER
    printf("Aflag retval=%d, avert=%d,%f, bvert=%d,%f\n",sectret,aVertex,sA,bVertex,tB);
      fflush(stdout);
    prInfoIntersect(arrInfoA);
#endif
              }
              if (arrInfoA->nused >= arrInfoA->navail)
                          extendArrinfoIntersect(arrInfoA,10);
              a = (arrInfoA->arr) + arrInfoA->nused;arrInfoA->nused++;
              a->partA = aPart; a->partB = bPart;
              a->vertexA = aVertex; a->vertexB = bVertex;
              a->fractA = sA; a->fractB = tB;
              a->crossDir= crossDir; a->xPoint = vJoin;
              arrIClassA->arr[aPart] = 1;
              arrIClassB->arr[bPart] = 1;
              result = 1;
            }
          }
        }
      }
    }
  }
  /*Find out how many parts of A inside B and vice-versa*/
  for (aPart=0; aPart < objA->nParts; aPart++) {
  vector2 point;
  int flag;
  int vertex = objA->panPartStart[aPart];
    if (arrIClassA->arr[aPart] == 0) {
      setVector2(objA->padfX[vertex],objA->padfY[vertex],&point);
      if ((flag=insideObj(1,&point,NULL,objB)) != 0) {
        arrIClassA->arr[aPart] = 2;
        result=1;
      }
      if (verb_level > 1) printf ("first point %d (%f,%f) inside objB? %d\n",
      aPart,point.x,point.y,flag );
    }
  }

  for (bPart=0; bPart < objB->nParts; bPart++) {
  vector2 point;
  int flag;
  int vertex = objB->panPartStart[bPart];
    if (arrIClassB->arr[bPart] == 0) {
      setVector2(objB->padfX[vertex],objB->padfY[vertex],&point);
      if ((flag=insideObj(1,&point,NULL,objA)) != 0) {
        arrIClassB->arr[bPart] = 2;
        result=1;
      }
      if (verb_level > 1) printf ("first point %d (%f,%f) inside objA? %d\n",
      bPart,point.x,point.y,flag );
    }
  }
  return result;
}
#endif /*NEWMKINT*/

void closeShop() {
  clearArrinfoIntersect(&aTable);
  clearArrinfoIntersect(&bTable);
  clearArrdouble(&pdfX);clearArrdouble(&pdfY);
  clearArrint(&panPartStart);
  clearArrint(&aPartInt); clearArrint(&bPartInt);
}
