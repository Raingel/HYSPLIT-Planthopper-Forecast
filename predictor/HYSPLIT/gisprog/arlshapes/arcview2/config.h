/* arcview2/config.h.  Generated from config.h.in by configure.  */
#ifndef _CONFIG_H_
#define _CONFIG_H_
/* #undef HAVE_INT32 */
/* #undef WORDS_BIGENDIAN */
/* #undef HAVE_UCHAR */
#define SIZEOF_DOUBLE 4
 #ifndef HAVE_UCHAR
  typedef unsigned char uchar;
 #endif /* HAVE_UCHAR */
 #ifndef HAVE_INT32
  #if UINT_MAX == 65535
   typedef long          int32;
  #else
   typedef int           int32;
  #endif
 #endif /* HAVE_INT32 */
#endif /*_CONFIG_H_*/
