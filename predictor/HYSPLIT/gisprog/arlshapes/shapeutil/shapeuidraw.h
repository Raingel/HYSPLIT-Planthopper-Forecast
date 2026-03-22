#ifndef _SHAPE_UI_
#define _SHAPE_UI_

#include "vector2.h"

#include "arrayutil.h"

/* The following structure details an intersection between a polygon in
 * Shape file "A" and one in shape file "B".
 */

extern int verb_level;

typedef struct {
int partA,partB; /* The part number of the shape (individual polygon)*/
int vertexA,vertexB; /*The intersection takes place between vertex and vertex+1*/
double fractA,fractB; /*The intersection takes place fracto of the way between
                       * vertex and vertex+1  */
int crossDir; /*Direction of crossing. +1: A crosses B from left to right
               * while B crosses A from right to left.  -1: A crosses B from
               * right to left while B crosses A from left to right. 0:
               * paths coincide, no crossing yet.*/
vector2 xPoint;
int jumpIndex;
} infoIntersect;

ARRTYPE(infoIntersect);

void drawIntersect(arrinfoIntersect * a,arrinfoIntersect * b,
                    SHPObject * objA,SHPObject * objB,SHPObject ** objOut,
                    arrint * iA, arrint * iB) ;

void drawUnion(arrinfoIntersect * a,arrinfoIntersect * b,
                    SHPObject * objA,SHPObject * objB,SHPObject ** objOut,
                    arrint * iA, arrint * iB) ;
int mkIntersectTable(SHPObject * objA,SHPObject * objB,
                      arrinfoIntersect * arrInfoA,
                      arrint * arrIClassA,arrint * arrIClassB) ;
void findUnion(SHPObject * objA,SHPObject * objB,SHPObject ** objOut) ;
int findIntersect(SHPObject * objA,SHPObject * objB,SHPObject ** objOut) ;

void closeShop();

#endif
