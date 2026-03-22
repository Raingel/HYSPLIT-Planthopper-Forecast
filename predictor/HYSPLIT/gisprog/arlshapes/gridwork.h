#ifndef _GRIDWORK_H_
#define  _GRIDWORK_H_

typedef struct {
int nx,ny;
void * params;
 void (* xy2XY)(void * params,double x,double y,double * X,double * Y);
double * arr;
} grid;

#define GRID(g,i,j) (g)->arr[(i)*((g)->ny)+(j)]

void newGrid (grid * g,int nx,int ny,void * params,
      void (*xy2XY)(void * params,double x,double y,double * X,double *Y) ) ;

void freeGrid(grid * g) ;


#endif
