#include <stdlib.h>
#include <stdio.h>
#define PROGNAME "splitObj"
#include "config.h"
#include <shapext.h>
#include "splitobj.h"


int objPartsInside(int j, int k, SHPObject * obj) {
/*returns -1 if part j inside part k, +1 if part k inside part j, 0 otherwise.*/
int venn;
vector2 partAmin,partAmax;
vector2 partBmin,partBmax;
vector2 point;
  limitsPartObj(j, obj, &partAmin.x,&partAmin.y, &partAmax.x,&partAmax.y);
  limitsPartObj(k, obj, &partBmin.x,&partBmin.y, &partBmax.x,&partBmax.y);
    venn = boxVenn(&partAmin, &partAmax,  &partBmin, &partBmax);
  switch (venn) {
  case 0:case 3:  break;   /*Neither a inside b nor b inside a*/
  case 1: /*Limits of j(A) inside limits of k(B).*/
    point.x = obj->padfX[obj->panPartStart[j]];
    point.y = obj->padfY[obj->panPartStart[j]];
    if ( insidePartObj(k, obj, 1, NULL, &point)) {
      return -1;
    }
    break;
  case 2:
    point.x = obj->padfX[obj->panPartStart[k]];
    point.y = obj->padfY[obj->panPartStart[k]];
    if ( insidePartObj(j, obj, 1, NULL, &point)) {
      return 1;
    }
    break;
  }
  return 0;
}

void dubPart(SHPObject * objIn,int k,double * XX,double * YY,
             int * panPartStart, int * partNo) {
int ncopy;
int k1,k2;
int m,xofset;
DEBUG_OUT3("Entering dubPart index=%d part=%d nparts=%d\n",k,* partNo,objIn->nParts);
  k1 = objIn->panPartStart[k];
DEBUG_OUT3(" part %d first vertex %d of %d\n",k,k1,objIn->nVertices);
  if (k < objIn->nParts-1) {
    k2 = objIn->panPartStart[k+1];
DEBUG_OUT1("Internal part last vertex + 1 =%d\n",k2);
  } else {
    k2 = objIn->nVertices;
DEBUG_OUT2("Final Part, last vertex + 1 = %d of %d\n",k2, objIn->nVertices);
  }
  ncopy = k2-k1;
  xofset = panPartStart[* partNo];
DEBUG_OUT3(" %d %d %d\n",k1,k2,xofset);
  panPartStart[(* partNo) + 1] = xofset + ncopy;
  for (m=0;m<ncopy;m++) {
    XX[xofset + m] = objIn->padfX[k1 + m];
    YY[xofset + m] = objIn->padfY[k1 + m];
  }
  (* partNo) ++;
}


typedef struct {
  int prev;
  int layer;
} link;

int splitObj(SHPObject * objIn,SHPObject ** objOut) {
int iObj = 0;
link * ptr=NULL;
int j,k,m,n;
double * XX=NULL, *YY=NULL;
int * panPartStart=NULL;
int partNo;
vector2 partMin,partMax;
  ptr = (link *) malloc( objIn->nParts * sizeof(link));
  for (k=0;k<objIn->nParts;k++) {
    ptr[k].prev = -1;
    ptr[k].layer = 0;
    limitsPartObj(k, objIn, &partMin.x,&partMin.y, &partMax.x,&partMax.y);
    DEBUG_OUT3("Limits box: %d: (%g,%g) ", k, partMin.x,partMin.y);
    DEBUG_OUT2("to (%g,%g)\n",partMax.x,partMax.y);
#ifdef DEBUGGER
      objPartIndices(objIn, k, &m, &n);
DEBUG_OUT3("object has %d vertices from %d to %d\n",n-m,m,n-1);
#endif
  }
/* Determine which parts are inside which.  A given part of object k is inside
 * ptr[k].layer other parts, and the innermost other part that it is inside is 
 * part number ptr[k].prev.
 */

  for(k=1; k<objIn->nParts; k++) {
    ptr[k].prev = -1;
    for (j=0; j<objIn->nParts; j++) {
      if (k == j) continue;
      if (objPartsInside( j, k, objIn) == 1) {
      /*Part k inside part j.*/
        if (ptr[k].prev < 0) {
          ptr[k].prev = j;
        } else {
          if (objPartsInside( ptr[k].prev, j, objIn) == 1) {
            ptr[k].prev = j;
          }
        }
      }
    }
  }
  for(k=1; k<objIn->nParts; k++) {
    for (j=k, ptr[k].layer = 0;ptr[j].prev>=0;j=ptr[j].prev) {
      ptr[k].layer ++;
    }
  }

#if DEBUGGER
  for (k=0;k<objIn->nParts;k++) {
    DEBUG_OUT3("Ptr %d: prev=%d, layer=%d\n",k,ptr[k].prev,ptr[k].layer);
  }
#endif
/*At this stage, inside/outside hierarchy of object parts should be recorded in ptr array.*/
  panPartStart = (int *) malloc((objIn->nParts+1)*sizeof(int));
  panPartStart[0] = 0;
  XX = (double *) malloc((objIn->nVertices) * sizeof(double ));
  YY = (double *) malloc((objIn->nVertices) * sizeof(double ));
  for (k=0;k<objIn->nParts;k++) {
DEBUG_OUT1(" Working on %d\n",k);
    if ((ptr[k].layer & 1) !=  0 ) continue;
    partNo=0;
    dubPart(objIn, k, XX, YY, panPartStart,& partNo);
DEBUG_OUT2(" initial dub %d %d\n",partNo,panPartStart[partNo]);
    for (m=0;m < objIn->nParts; m++) {
      if (m == k) continue;
      n=m;
      while (ptr[n].prev >= 0) {
        if (ptr[n].prev == k) {
          dubPart(objIn, m, XX, YY, panPartStart,& partNo);
DEBUG_OUT3("dubbed %d %d %d\n",iObj,partNo,panPartStart[partNo]);
        }
        n=ptr[n].prev;
      }
    }
    objOut[iObj] = SHPCreateObject(SHPT_POLYGON, objIn->nShapeId, partNo,
                    panPartStart,NULL, panPartStart[partNo],XX,YY,NULL,NULL);
    iObj++;
  }


  free(XX);XX=NULL;free(YY);YY=NULL;
  free(panPartStart);panPartStart=NULL;
  free(ptr);ptr = NULL;
  return iObj;
}
