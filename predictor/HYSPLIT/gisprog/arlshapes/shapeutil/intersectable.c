/*-------------------- intersectable.c -----------------------------------------
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
#undef MAX
#define MAX(A,B) (((A)>(B))?(A):(B))
#undef MIN
#define MIN(A,B) (((A)<(B))?(A):(B))

#define ERR_EXIT 0

static MKEXTENDARRTYPE(infoIntersect)

static void partShapeObj(SHPObject * a, int nPart, int * first, int * last) {
  *first =  a -> panPartStart[nPart];
  if (nPart+1 >= a-> nParts) {
    *last = a->nVertices;
  } else {
    *last = a->panPartStart[nPart+1];
  }
}

int realindex(SHPObject * obj, int part, int index)  {
int firstVertex,lastVertex;
  partShapeObj(obj, part, &firstVertex, &lastVertex) ;
  while (index > lastVertex-2) index += (firstVertex-lastVertex+1);
  while (index < firstVertex) index -= (firstVertex-lastVertex+1);
  return index;
}

int coordPart(SHPObject * obj, int part,int vertex,int count,vector2 * base) {
int firstVertex,lastVertex,vdex;
int index;
  partShapeObj(obj, part, &firstVertex, &lastVertex) ;
  for (index=0,vdex=realindex(obj, part, vertex); index<count; index++) {
    base[index].x = obj->padfX[vdex];
    base[index].y = obj->padfY[vdex];
    vdex++;
    if (vdex>=lastVertex-1) vdex += (firstVertex - lastVertex+1);
  }
  return 0;
}

void bboxPart(SHPObject * obj, int part, vector2 * vmin, vector2 * vmax){
int firstVertex,lastVertex,vertex;
  partShapeObj(obj, part, &firstVertex, &lastVertex) ;
  vmin->x = vmax->x = obj->padfX[firstVertex];
  vmin->y = vmax->y = obj->padfY[firstVertex];
  for (vertex=firstVertex;vertex<lastVertex;vertex++) {
    vmin->x = MIN(vmin->x,obj->padfX[vertex]);
    vmax->x = MAX(vmax->x,obj->padfX[vertex]);
    vmin->y = MIN(vmin->y,obj->padfY[vertex]);
    vmax->y = MAX(vmax->y,obj->padfY[vertex]);
  }
}

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

int mkIntersectTable(SHPObject * objA,SHPObject * objB,
                      arrinfoIntersect * arrInfoA,
                      arrint * arrIClassA,arrint * arrIClassB) {
/*objA and objB are shape file records of (possibly) multi-part polynomials*/
/* mkIntersectTable creates a table of intersections between object A
 * and object B.  Included in the table, for each intersection and for each
 * object, are the part and vertex of the intersection, and the proportional
 * distance from the given vertex to the next vertex that the intersection
 * takes place.  Also, the vector coordinates of the intersection.
 * In addition, a crossing direction flag crossDir is included, which
 * is plus one when objA crosses objB to the right (inside), -1 when
 * to the left (outside).  crossDir is zero at the point where the two objects
 * begin to overlap.
 *  The IClass (include class) tables indicate a status for the individual
 * parts of objA nad objB.  for a given part of either, it is zero if 
 * disjoint from all parts of the other, and neither enclosws nor is 
 * enclosed by, any part of the other object. IClass is 1 for that part if
 * it intersects with one or more parts of the other object.  IClass is 2
 * for a given part if it does not intersect the other object, but does
 * lie entirely inside some part of the other object.
 *
 * Returns 0 if there are no intersections, and all parts lie outside
 * the other object.  Returns 1 otherwise (some parts of one intersect or lie
 * inside some parts of the other).
 */
int result=0;
int aPart,bPart;

  if ((objA->nParts <= 0)||(objB->nParts <= 0)) {
    fprintf(stderr,"Object Type Error in mkIntersectTable\n");
    exit(ERR_EXIT);
  }
  resetArrint(arrIClassA);resetArrint(arrIClassB);
  extendArrint(arrIClassA,objA->nParts - arrIClassA->navail);
  extendArrint(arrIClassB,objB->nParts - arrIClassB->navail);

/*-------------------- Pre-set the IClass arrays -----------------------*/
  for(aPart = 0;aPart < objA->nParts; aPart++) arrIClassA->arr[aPart] = 0;
  arrIClassA->nused = objA->nParts;
  for(bPart = 0;bPart < objB->nParts;bPart++) arrIClassB->arr[bPart] = 0;
  arrIClassB->nused = objB->nParts;

/*------ If bounding boxes of the objects disjoint, no intersections */
  if ( ! shpBoxOverlap(objA,objB)) return 0;

/*------------------------------- Check Out Parts -----------------------*/
  for (aPart = 0; aPart < objA->nParts; aPart++) {
  vector2 vAmin,vAmax;
  int firstAvertex,lastAvertex,vertexA;
    partShapeObj(objA, aPart, &firstAvertex, &lastAvertex) ;
    bboxPart(objA,aPart,&vAmin,&vAmax);

    for (bPart = 0; bPart < objB->nParts; bPart++) {
    vector2 vB0[2];
    int firstBvertex,lastBvertex,vertexB;
/*--- If this part of objB does not overlap this part of objA, skip --*/
      bboxPart(objB,bPart,&vB0[0],&vB0[1]);
      if (! vector2BoxOverlap(vAmin,vAmax,vB0[0],vB0[1])) continue;
/*------- else check segment by segment -------------------------------*/
      partShapeObj(objB, bPart, &firstBvertex, &lastBvertex) ;
      for (vertexB=firstBvertex; vertexB<lastBvertex-1 ;vertexB++){
/*---- check the current segment for corssing bounding box object A ----*/
        coordPart(objB, bPart, vertexB, 2, vB0);
        if (! vector2BoxOverlap(vAmin,vAmax,vB0[0],vB0[1])) continue;
/*--- current B segment in A part Bounding Box; loop through A for crossings -*/
        for (vertexA = firstAvertex; vertexA < lastAvertex-1;vertexA++) {
        double sA,tB,crossArea;
        int sectret,crossDir;
        infoIntersect * a;
        vector2 vA0[2],vJoin,prevA,prevB;
          coordPart(objA, aPart, vertexA, 2, vA0);
          sectret = intersect(vA0,vA0+1,vB0,vB0+1,&sA,&tB,&crossArea,&vJoin);
          if (sectret == 0) continue; /*Return code 0; go to next segment*/

          crossDir = (crossArea>0.)?1:((crossArea<0.)?-1:0);

          if (crossDir == 0) {
          double ss[2],tt[2];
          vector2 vt[2];
            if ((colinear(vA0,vA0+1,vB0,vB0+1,ss,tt,vt) != 0)) continue;
/*code to handle overlap cases*/
/* ss is a pair of indices giving the relative position of b0 and b1 relative to a0 and a1;
 * tt is a pair of indices giving a0 and a1 relative to b0 and b1 (e.g. b0 = (1-ss[0])a0 + ss[0]a1)
 * vt[0] and vt[1] are the endpoints of the intersection of a0-a1 and b0-b1*/
            if ((ss[0] < ss[1]) /*two segments going in the same sense and overlap non-zero length*/
               );
            continue;
          }

          if ((sectret & 5) != 5) continue; /*If tail of either vector on other's line, skip*/
          switch (sectret) { /*At this stage, the 4- and 1- bit are on.
                             Hence, possibilities are 5,7,13,15*/
          case 15: /*standard crossing inside both segments*/
            break; /*go to end of switch loop and fill table entry*/
          case 5:
            coordPart(objA, aPart, vertexA-1, 1, &prevA);
            coordPart(objB, bPart, vertexB-1, 1, &prevB);
            if ( (crossDir=checkCross(&vJoin, &prevA, vA0+1,&prevB,vB0+1)) == 0)
                                               continue; /*go to next vertexA*/
            break;
          case 7:
            /* Base of B vector on interior of A vector*/
            coordPart(objB, bPart, vertexB-1, 1, &prevB);
            if ( (crossDir=checkCross(&vJoin, vA0, vA0+1,&prevB,vB0+1)) == 0)
                                               continue; /*go to next vertexA*/
            break;
          case 13:
            /* Base of A vector on interior of B vector*/
            coordPart(objA, aPart, vertexA-1, 1, &prevA);
            if ( (crossDir=checkCross(&vJoin, &prevA, vA0+1, vB0,vB0+1)) == 0)
                                               continue; /*go to next vertexA*/
            break;
          default:
            fprintf(stderr,"Unexplored case in mkIntersect %d\n",sectret);
            continue;
          }
          if (arrInfoA->nused >= arrInfoA->navail)
                       extendArrinfoIntersect(arrInfoA,10);
          a = (arrInfoA->arr) + arrInfoA->nused;arrInfoA->nused++;
          a->partA = aPart; a->partB = bPart;
          a->vertexA = vertexA; a->vertexB = vertexB;
          a->fractA = sA; a->fractB = tB;
          a->crossDir= crossDir; a->xPoint = vJoin;
          arrIClassA->arr[aPart] = 1;
          arrIClassB->arr[bPart] = 1;
          result = 1;
        } /* next vertexA*/
      } /* next vertexB*/
    } /* next bPart*/
  } /* next aPart*/


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

/*printf("New ICOUNT=%d, result=%d, table count=%d\n",ICOUNT++,result,arrInfoA->nused);*/

  return result;
}

