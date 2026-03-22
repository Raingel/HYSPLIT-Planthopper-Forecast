
/* swap function, call by reference (FORTRAN system)
   will reverse, msb to lsb, every word of *framep bytes
   length, in a buffer of length *count words.
*/
#include "fcsubs.h"

void SWAP_F77(void * buffer,INT4 *framep,INT4 *count)
{
 char temp;
 INT4 bufsize,k,l,l1,frame;
 frame=*framep;
 bufsize=frame*(*count);
 for (l=0,l1=frame-1;l<l1;l++,l1--)
 {
  for (k=0;k<bufsize;k+=frame)
  {
   temp = ((char *)buffer)[k+l];
   ((char *)buffer)[k+l]=((char *)buffer)[k+l1];
   ((char *)buffer)[k+l1]=temp;
  }
 }
}

void swap_c(void * buffer,INT4 frame,INT4 count)
/* swap function, call by value (C system)
   will reverse, msb to lsb, every word of frame bytes
   length, in a buffer of length count words.
*/
{
 char temp;
 INT4 bufsize,k,l,l1;
 bufsize=frame*count;
 for (l=0,l1=frame-1;l<l1;l++,l1--)
 {
  for (k=0;k<bufsize;k+=frame)
  {
   temp = ((char *)buffer)[k+l];
   ((char *)buffer)[k+l]=((char *)buffer)[k+l1];
   ((char *)buffer)[k+l1]=temp;
  }
 }
}
