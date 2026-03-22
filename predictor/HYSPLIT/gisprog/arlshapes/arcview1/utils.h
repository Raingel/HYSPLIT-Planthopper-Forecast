#ifndef __utils__
#define __utils__

#include "../config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
 
/* getStripLine: reads a line (delimited by \n) from fp into buffer,
 * (size n).  Returns buffer with \n removed, and preceding \r, if any.
 * buffer will be null-terminated.  Returns number of characters in
 * buffer, not including terminating null character, or -1 if EOF
 * reached, or -2 if buffer length exceeded.                           */
ssize_t getStripLine(char * buffer, size_t n, FILE * fp) ;

#endif /*__utils__*/

