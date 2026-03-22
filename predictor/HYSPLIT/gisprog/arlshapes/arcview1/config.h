/* arcview1/config.h.  Generated from config.h.in by configure.  */
#ifndef _CONFIG_H_1
#define _CONFIG_H_1
/* #undef USE_STRICMP */
#define HAVE_STRCHR 1
#define HAVE_STRRCHR 1
#define HAVE_STRCASECMP 1
/* #undef HAVE_INDEX */
/* #undef HAVE_RINDEX */
#ifdef HAVE_STRCASECMP
  #define STRCASECMP strcasecmp
#else 
  #ifdef USE_STRICMP
    #define STRCASECMP stricmp
  #endif
#endif
#ifndef FALSE
#define FALSE (1==0)
#endif
#ifndef TRUE
#define TRUE (1==1)
#endif
#ifndef XSUB
  #define XSUB(V) #V
  #define SUB(V) XSUB(V)
#endif

#ifndef DEBUG_OUT
  #ifdef DEBUGGER
    #define DEBUG_OUT(str) fprintf(stderr, PROGNAME " debug: " str);fflush(stderr)
    #define DEBUG_OUT1(str,v) fprintf(stderr, PROGNAME " debug: " str, v);fflush(stderr)
    #define DEBUG_OUT2(str,v,w) fprintf(stderr, PROGNAME " debug: " str, v, w);fflush(stderr)
    #define DEBUG_OUT3(str,v,w,x) fprintf(stderr, PROGNAME " debug: " str, v, w, x);fflush(stderr)
  #else
    #define DEBUG_OUT(str)
    #define DEBUG_OUT1(str,v)
    #define DEBUG_OUT2(str,v,w)
    #define DEBUG_OUT3(str,v,w,x)
  #endif
#endif

#endif /*_CONFIG_H_1*/
