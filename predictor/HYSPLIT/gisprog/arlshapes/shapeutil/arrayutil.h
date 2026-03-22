#ifndef _ARRAY_UTIL_H
#define  _ARRAY_UTIL_H
#define ARRTYPE(atype) typedef struct {int nused,navail;/*@null@*/atype * arr;}arr##atype

#define CLEARARRTYPE(atype) void clearArr##atype (arr##atype * a)

#define RESETARRTYPE(atype) void resetArr##atype (arr##atype * a)

#define EXTENDARRTYPE(atype) void extendArr##atype(arr##atype * a,int nterms)

#define MKCLEARARRTYPE(atype) CLEARARRTYPE(atype) {\
  if (a->arr != NULL) {\
    free(a->arr);\
    a->arr = NULL;\
  }\
  a->nused = a->navail = 0;\
}

#define MKRESETARRTYPE(atype) RESETARRTYPE(atype) {\
  a->nused = 0;\
}

#define MKEXTENDARRTYPE(atype) EXTENDARRTYPE(atype) {\
  if (a->arr == NULL) {\
    a->arr = (atype *) malloc(nterms * sizeof(atype));\
    a->nused=0;a->navail=nterms;\
    return;\
  }\
  a->arr = (atype *) realloc(a->arr,\
                          (a->navail + nterms) * sizeof(atype));\
  a->navail += nterms;\
}

ARRTYPE(int);
RESETARRTYPE(int);
CLEARARRTYPE(int);
EXTENDARRTYPE(int);

ARRTYPE(double);
RESETARRTYPE(double);
CLEARARRTYPE(double);
EXTENDARRTYPE(double);

#endif /* _ARRAY_UTIL_H*/
