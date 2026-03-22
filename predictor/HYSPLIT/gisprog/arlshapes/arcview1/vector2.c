#include <stdlib.h>
#include <stdio.h>
#include "vector2.h"
#include <math.h>

#ifndef MIN
#define MIN(A,B) ((A>B)?(B):(A))
#endif /*MIN*/
#ifndef MAX
#define MAX(A,B) ((A<B)?(B):(A))
#endif /*MAX*/
#define DEBUG_VECTOR2 1==0

#if DEBUG_VECTOR2
#define AV2(v) v->x,v->y
#define PCT %f,%f
  printf("a0 %f,%f b0 %f,%f mid %f,%f b1 %f,%f \n",AV2(a0),AV2(b0),AV2(mid),AV2(b1));
#endif

double dotDiff(vector2 * o, vector2 * a,vector2 * b) {
  return (a->y - o->y) * (b->y - o->y) + (a->x - o->x) * (b->x - o->x);
}

double triArea(vector2 * a,vector2 * b,vector2 * c) {
  return .5 * ((a->x - b->x)*(c->y - b->y) - (a->y - b->y)*(c->x - b->x));
}

double len2Vector(vector2 * a1,vector2 * a2) {
  return sqrt((a2->x - a1->x) * (a2->x - a1->x) +
              (a2->y - a1->y) * (a2->y - a1->y));
}

int bentline(vector2 * a,vector2 * mid,vector2 * b0,vector2 * b1,double * x,double *y) {
/* returns "bent line" coordinates x and y showing where, on the bent line
 * through b0  to mid, thence through b1, the vector a is located.
 * returns -2 if line undefined (b0==mid,b1==mid, or b0 and b1 co-linear
 * and on the same side of mid as each other), returns 1 if a to the right
 * of b0-mid-b1, -1 if to the left, and zero if on the bent line.
 * coordinate y is proportional to distance of a to the right of the bent line.
 */
vector2 perp;
double ab,xp,xa,xx,yy,xb;
double lena=len2Vector(mid,b0),lenb=len2Vector(mid,b1),lenp;
  if ((lena == 0.) || (lenb == 0.)) return -2;
  setVector2(mid->x - (b0->y - mid->y)/lena - (mid->y - b1->y)/lenb,
             mid->y + (b0->x - mid->x)/lena + (mid->x - b1->x)/lenb,&perp);
/* perp is a point on the angular bisector from mid, halfway from b0 to
 * b1, and to the right of b0-mid-b1.*/
  lenp = len2Vector(mid,&perp);
  ab = dotDiff(mid,b0,&perp)/lena/lenp;
  xp = dotDiff(mid,a,&perp)/lenp;
  xa = dotDiff(mid,a,b0)/lena;
  xx = - (xa - ab * xp)/(1. - ab*ab);
  yy = (xp - ab * xa)/(1. - ab*ab);
/*a = mid + xx*(mid-b0) + yy(perp-mid)*/
  if (xx > 0.) {
    xb = dotDiff(mid,a,b1)/lenb;
    xx = (xb - ab * xp)/(1. - ab*ab);
    yy = (xp - ab * xb)/(1. - ab*ab);
/*a = mid + xx*(b1-mid) + yy(perp-mid)*/
  }
  *x = xx;
  *y = yy;
  if (yy < 0) return -1;
  if (yy > 0) return 1;
  return 0;
}

int checkCross(vector2 * mid,vector2 * a0,vector2 * a1,vector2 * b0,vector2 * b1) {
/* retval == 0, no crossing.
 * retval == 1, a crosses b to the inside (i.e. to the right).
 * retval == -1, a crosses b to the outside (i.e. to the left)
 * retval == 2, two polylines overlap as they leave mid.
 * retval == -2, bad arrangement (point ax or bx == mid, or lines
 * a and b reverse oriented)
 **/
int i0,i1;
double x0,y0,x1,y1;
  i0 = bentline( a0, mid, b0, b1, &x0, &y0);
  i1 = bentline( a1, mid, b0, b1, &x1, &y1);
  if (i0 < -1 || i1 < -1) return -2;
  if (i0 < 0 && i1 > 0) return -1;
  if (i0 > 0 && i1 < 0) return 1;
  if (i0 < 0 && i1 < 0) return 0;
  if (i0 > 0 && i1 > 0) return 0;
  if (i1 == 0 ) {
    if( x1 > 0) {
      return 2; /*  polylines a and b continue in overlap without yet crossing*/
    } else {
      return -2; /* polyline overlap running opposite directions*/
    }
  }
/*At this point, i0 ==0 and either i1 == 1 or i1 == -1 */
  if ( x0 >= 0.) return -2;
  if (  i1 > 0) return -1;
  else return 1;
}

int checkCrossx(vector2 * mid,vector2 * a0,vector2 * a1,vector2 * b0,vector2 * b1) {
/*retval=0, no crossing.
 *retval  = -1, a crosses b to the inside (i.e. to the right).
 *retval  = 1, a crosses b to the outside (i.e. to the left)
 **/
  int d1 = sideOfLine(a0,b0,mid,b1);
  int d2 = sideOfLine(a1,b0,mid,b1);
  if ( (d1<0.) && (d2 > 0.) ) return 1;
  if ( (d1>0.) && (d2 < 0.) ) return -1;
  if ( (d1<0.) && (d2 < 0.) ) return 0;
  if ( (d1>0.) && (d2 > 0.) ) return 0;
/* At this stage, either d1==0 or d2==0 or both.
 */
  if (dotDiff(mid,a0,b0)>0) {
      if (d2 > 0.) return -1;
      if (d2 < 0.) return 1;
  }
  if ((d1 < -1) || (d2 < -1)) return 0;
  return 0;
}

int colinear(vector2 * a1,vector2 * a2, vector2 * b1, vector2 * b2,
         double s[2],double t[2], vector2 * join) {
double denA,denB;
  if ((triArea(a1,b1,a2) != 0) || (triArea(a1,b2,a2) != 0)) {
     fprintf(stderr,"vectors not co-linear\n");
     return -1;
  }
  denA = dotDiff(a1,a2,a2);
  denB = dotDiff(b1,b2,b2);
  if ( (denA == 0.) || (denB == 0.) ) {
    fprintf(stderr, "zero length vectors\n");
    return 4;
  }
  s[0] = dotDiff(a1,b1,a2)/denA;
  s[1] = dotDiff(a1,b2,a2)/denA;
  t[0] = dotDiff(b1,a1,b2)/denB;
  t[1] = dotDiff(b1,a2,b2)/denB;
  if ((s[1] <= s[0]) || (t[1] <= t[0]) ) {
    fprintf(stderr,"vectors opposite directions\n");
    return 3;
  }
  if ((s[0] > 1.) || (t[0] > 1.) || (s[1] < 0.) || (t[1] < 0.) ) {
    fprintf(stderr,"vectors do not overlap\n");
    return 2;
  }
  if (join != NULL) {
    if (s[0]>0) join[0] = *b1;
    else join[0] = *a1;
    if (s[1]<1.) join[1] = *b2;
    else join[1] = *a2;
  }
  return 0;
}

int intersect(vector2 * a1,vector2 * a2, vector2 * b1, vector2 * b2,
          double *s,double *t,double * tot_area, vector2 * join){
double area[4];
  area[0] = triArea(a1,b1,a2);
  area[1] = triArea(b1,a2,b2);
  area[2] = triArea(a2,b2,a1);
  area[3] = triArea(b2,a1,b1);
  * tot_area = .5 * ((area[0] + area[2]) + (area[1] + area[3]));
  if (*tot_area == 0.) {
/* If total area is zero, line segments a and b are parallel (or one of
 * is zero length).  If any of the component areas are non-zero, they
 * are not colinear.*/
    if ((area[0] != 0.) || (area[1] != 0.)) {
    /*Parallel, not colinear */
      *s = (area[1]<0.)?-10000.:10000;
      *t = (area[2]<0.)?-10000.:10000;
      return 0;
    } else {
   /*Co-linear */
/*a1->a2 and b1->b2 are co-linear or one of them is zero*/
     double alpha2,beta1,beta2,amax,amin,bmax,bmin,maxmin,minmax;
      alpha2 = len2Vector(a1,a2);
      beta1 = len2Vector(a1,b1); beta2 = len2Vector(a1,b2);
      amax=MAX(0.,alpha2);bmax=MAX(beta1,beta2);
      amin=MIN(0.,alpha2);bmin=MIN(beta1,beta2);
      if (amax<bmin || bmax < amin) return 0;
  /*Non-overlapping segments do not intersect.*/
      if (amax<=amin || bmax<=bmin) return 0;
  /*If either segment is zero length, segments do not intersect.*/
      maxmin=MAX(amin,bmin);minmax=MIN(amax,bmax);
      *s = .5*(maxmin + minmax)/alpha2;
      *t = (.5*(maxmin + minmax)-beta1)/(beta2-beta1);
      join->x = .5 * ( (*s * a1->x + (1.- *s) * a2->x) +
          (*t * b1->x + (1. - *t) * b2->x) );
      join->y = .5 * ( (*s * a1->y + (1.- *s) * a2->y) +
          (*t * b1->y + (1. - *t) * b2->y) );
     /*Need analysis of joined, non-overlaping lines*/
#if DEBUG_VECTOR2
      fprintf(stderr,"Co-linear segment case needs developing\n");
      fprintf(stderr,"%f,%f %f,%f %f,%f %f,%f",a1->x,a1->y,a2->x,a2->y,
               b1->x,b1->y,b2->x,b2->y);
      exit(-1);
#endif
      return 15;
    }
  } else {
    int retval=0;
    /*Not parallel nor co-linear.*/
    *s = area[3]/ *tot_area;
    *t = area[0]/ *tot_area;
    join->x = .5 * ( (*s * a2->x + (1.- *s) * a1->x) + (*t * b2->x + (1. - *t) * b1->x) );
    join->y = .5 * ( (*s * a2->y + (1.- *s) * a1->y) + (*t * b2->y + (1. - *t) * b1->y) );
    if ( *s < 0.) ;
    else if (*s == 0.) retval = 1; /* head of a on line b */
    else if (*s == 1.) retval = 2; /* tail of a on line b */
    else if (*s < 1.) retval = 3; /* interior of a crosses line b */
    if (retval != 0) {
      if (*t < 0.) retval = 0;
      else if (*t == 0.) retval |= 4; /* head of b on line a */
      else if (*t == 1.) retval |= 8; /* tail of b on line a */
      else if (*t < 1.) retval |= 12; /* interior of b crosses line a */
      else retval = 0;
    }
#if 1==0
    if ( (*s < 0.) || (*t < 0.) ) return 0;
    if ( (*s > 1.) || (*t > 1.) ) return 0;
    /*In above cases, segments do not intersect.*/
    if ( (*s > 0.) && (*s < 1.) &&(*t > 0.) && (*t < 1.)) return 1;
    if ( (*s >= 1.) || (*t >= 1.) ) return 0;
    return 1;
    if ((retval!=0) && (retval !=15) ) {
/*      printf("retval= %d (a:%d,b:%d)\n",retval,retval&3,((retval&12)/4));*/
/*      printf("a1=%f,%f a2=%f,%f b1=%f,%f b2=%f,%f\n",a1->x,a1->y,a2->x,a2->y,
               b1->x,b1->y,b2->x,b2->y);*/

    }
#endif
#if DEBUG_VECTOR2
printf("Vector2 retval = %x, s=%f, t=%f\n",retval,*s,*t);
printf("a1=%f,%f a2=%f,%f b1=%f,%f b2=%f,%f\n",a1->x,a1->y,a2->x,a2->y,
               b1->x,b1->y,b2->x,b2->y);
#endif
    return retval;
  }
  return 0;
}

vector2 * setVector2 (double x,double y,vector2 * result) {
  result->x = x;
  result->y = y;
  return result;
}

int sideOfLine(vector2 * a, vector2 * b0, vector2 * b1, vector2 * b2) {
double d1=distPtLine(a,b0,b1),d2=distPtLine(a,b1,b2),dc;
  if ( (maxsep(b0,b1) == 0.) || (maxsep(b1,b2)==0.)) return -2;
  if ((d1 < 0.) && (d2 < 0.)) return -1; /*a is to left of both lines.*/
  if ((d1 > 0.) && (d2 > 0.)) return  1; /*a is to right of both lines.*/
  dc = distPtLine(b2,b0,b1);
  if (dc == 0.) return 0; /* If dc == 0, the b's are co-linear.  If we came
                           * to this point, a is colinear with all.*/
  if (dc < 0.) { /*Point b2 is to left of b0-b1*/
    if ((d2 > 0.) || (d1 > 0.)) return 1; /*In either case, a is to right.*/
    return 0;
  }
  if (dc > 0.) { /*Point b2 is to right of b0-b1*/
    if ((d2 < 0.) || (d1 < 0.)) return -1; /*In either case, a is to left.*/
    return 0;
  }
  return 0;
}

double distPtLine(vector2 * a, vector2 * b0, vector2 * b1) {
/*Returns distance point a is to the left (negative, right) of the line
 * from b0 to b1.  Assumes b0 != b1. */
return ( (a->y - b0->y)*(b1->x - b0->x) - (a->x - b0->x)*(b1->y - b0->y) ) /
       len2Vector(b0,b1);
}

double maxsep(vector2 * a, vector2 * b) {
  return MAX( MAX(a->x - b->x,b->x - a->x), MAX(a->y - b->y,b->y - a->y) );
}


int overlap (vector2 * amin,vector2 * amax, vector2 * bmin, vector2 * bmax) {
  if( MAX(amin->x,amax->x) <= MIN(bmin->x,bmax->x)) return 0;
  if( MAX(bmin->x,bmax->x) <= MIN(amin->x,amax->x)) return 0;
  return 1;
}

#ifndef MAX
#define MAX(A,B) ((A)>(B)?(A):(B))
#endif
#ifndef MIN
#define MIN(A,B) ((A)<(B)?(A):(B))
#endif
int boxVenn(vector2 * amin,vector2 * amax, vector2 * bmin, vector2 * bmax) {
int retval=0; /*0 - no intersection 1 - no (A-B) 2 - no (B-A) 3 - A == B */
vector2 minmax,maxmin;
  minmax.x = MIN(amax->x,bmax->x);
  minmax.y = MIN(amax->y,bmax->y);
  maxmin.x = MAX(amin->x,bmin->x);
  maxmin.y = MAX(amin->y,bmin->y);
  if( minmax.x < maxmin.x) return 0; /* No overlap */
  if( minmax.y < maxmin.y) return 0;
  if( maxmin.x > maxmin.x) return 0;
  if( maxmin.y > maxmin.y) return 0; /* No overlap */
  if ((amax->x <= minmax.x) && (amax->y <= minmax.y) && (amin->x >= maxmin.x) &&
                 (amin->y >= maxmin.y)) retval = 1;
  if ((bmax->x <= minmax.x) && (bmax->y <= minmax.y) && (bmin->x >= maxmin.x) &&
                 (bmin->y >= maxmin.y)) retval += 2;
  return retval;
/* retval = 0; boxes a and b do not overlap. =3; boxes a and b are coincident.
 * retval = 1; box a inside box b.  2; box b inside box a.  */
}
