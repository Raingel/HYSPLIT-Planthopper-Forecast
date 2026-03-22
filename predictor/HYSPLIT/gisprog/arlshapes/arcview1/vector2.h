#ifndef _VECTOR_2_
#define _VECTOR_2_
typedef struct {
double x,y;
} vector2;
typedef struct {
vector2 a,b;
} segment;

int sideOfLine(vector2 * a, vector2 * b0, vector2 * b1, vector2 * b2) ;
/* Determines which side a is relative to the directed bent line from infinity
 * through b0 to b1, thence at an angle through b2 to infinity. Function
 * returns -2 if b0 == b1 or b1 == b2, so the bent line is undefined,
 * returns 0 if a is on the bent line, -1 if a is to the left of the bent
 * line, and 1 if a is to the right of the line.
 */

double len2Vector(vector2 * a1,vector2 * a2) ;
/* Returns length of the vector from a1 to a2.
 */

double distPtLine(vector2 * a, vector2 * b0, vector2 * b1) ;
/* Returns distance point a is to the left (negative, right) of the directed
 * linethrough b0 toward b1.  Assumes b0 != b1.
 */

double dotDiff(vector2 * o, vector2 * a,vector2 * b) ;
/* returns dot product between vectors a-o and b-o
 */ 


double triArea(vector2 * a,vector2 * b, vector2 *c);
double maxsep(vector2 * a, vector2 * b) ;
vector2 * setVector2 (double x,double y,vector2 * result) ;

int intersect(vector2 * a1,vector2 * a2, vector2 * b1, vector2 * b2,
          double *s,double *t,double * tot_area,vector2 * join);
int boxVenn(vector2 * amin,vector2 * amax, vector2 * bmin, vector2 * bmax) ;
int overlap (vector2 * amin,vector2 * amax, vector2 * bmin, vector2 * bmax) ;

int checkCross(vector2 * mid,vector2 * a0,vector2 * a1,vector2 * b0,vector2 * b1) ;
/* Determines whether bent line a0-mid-a1 crosses bent line b0-mid-b1.
 */
int colinear(vector2 * a1,vector2 * a2, vector2 * b1, vector2 * b2,
         double s[2],double t[2], vector2 * join) ;


#endif /*_VECTOR_2_*/
