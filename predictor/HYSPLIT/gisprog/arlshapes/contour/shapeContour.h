#ifndef _SHAPECONTOUR_H_
#define  _SHAPECONTOUR_H_
#include <shapext.h>
#include "gridwork.h"

void freeChainLinks(void) ;
int objContour(aShape * fileout, grid * g, double V, fieldData *fd);
int dewhiskerChain(int j) ;
void dropChain (int j) ;
void freePdfXY(void);


#endif
