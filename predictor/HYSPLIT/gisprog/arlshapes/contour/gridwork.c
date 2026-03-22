#include <stdlib.h>
#include "gridwork.h"

  #ifdef MEMWATCH
    #include <memwatch.h>
  #endif

void newGrid (grid * g,int nx,int ny,void * params,
      void (*xy2XY)(void * params,double x,double y,double * X,double *Y)) {
  g->nx=nx;g->ny=ny;
  g->arr = (double *) malloc(nx*ny*sizeof(double));
  g->params = params;
  g->xy2XY = xy2XY;
}

void freeGrid(grid * g) {
  free(g->arr);
  g->nx=g->ny=0;
  g->arr=NULL;
  g->params = NULL;
  g->xy2XY = NULL;
}
