#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int nChInStr(const char * string,int ch) {
int k=0;
const char * pt = string;
  while (pt != NULL)   {
    if ((pt = strchr(pt,ch)) != NULL) k++;
  }
  return k;
}
