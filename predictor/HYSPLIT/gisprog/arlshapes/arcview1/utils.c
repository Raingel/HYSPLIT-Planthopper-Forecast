#include "../config.h"
#include "utils.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
 
/* getStripLine: reads a line (delimited by \n) from fp into buffer,
 * (size n).  Returns buffer with \n removed, and preceding \r, if any.
 * buffer will be null-terminated.  Returns number of characters in
 * buffer, not including terminating null character, or -1 if EOF
 * reached, or -2 if buffer length exceeded.                           */

ssize_t getStripLine(char * buffer, size_t n, FILE * fp) {
int c;
ssize_t i;

#if 1==0
  i=0;
  while ( (c = getc(fp)) != EOF && i<n ) {
    buffer[i]=c;
    if (c == '\n') break;
    i++;
  }
  if (i == n) return -2;
  buffer[i] = '\0';
  if (c == EOF) return -1;
#else
  for (i=0;i<n;i++) {
    c = getc(fp);
    if (c == EOF ) {
      buffer[i]='\0';
      return (i==0?-1:i);
    }
    if (c == '\n') {
      buffer[i]='\0';
      break;
    }
    buffer[i] = c;
  }
  if (i==n) {
    buffer[i-1]='\0';
    return -2;
  }
  if (i>0 && buffer[i-1] == '\r') {
    i--;
    buffer[i]='\0';
  }
  return i;
#endif
}
