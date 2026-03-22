#include <stdlib.h>
#include <stdio.h>
#include  "shapext.h"
#include <string.h>
#include <float.h>
#include <math.h>
#include <stdarg.h>
#include "config.h"
#include <shapefil.h>

/*#include "utils.h"*/
#include "vector2.h"


#ifndef REARTH
#define REARTH 6367.47
#endif /*REARTH : radius of Earth in km */

#ifdef USE_STRICMP
#define CASE_INSENSITIVE_STR_CMP	stricmp
#else
#define CASE_INSENSITIVE_STR_CMP	strcasecmp
#endif

#define PROGNAME "shapext"
#define LV(value) {value,#value}
typedef struct {int id;char * sid;} Lv;
Lv SHPTYPES[] = {LV(SHPT_NULL),LV(SHPT_POINT),LV(SHPT_ARC),LV(SHPT_POLYGON),
                 LV(SHPT_MULTIPOINT),LV(SHPT_POINTZ),LV(SHPT_ARCZ),
                 LV(SHPT_POLYGONZ),LV(SHPT_MULTIPOINTZ),LV(SHPT_POINTM),
                 LV(SHPT_ARCM),LV(SHPT_POLYGONM),LV(SHPT_MULTIPOINTM),
                 LV(SHPT_MULTIPATCH)};

char * SHPLABEL(int shpType){
int k;
static char buf[40];
  for(k=0;k<sizeof(SHPTYPES)/sizeof(SHPTYPES[0]);k++){
    if (shpType == SHPTYPES[k].id) return SHPTYPES[k].sid ;
  }
  sprintf(buf,"Unknown Shape Type %d ",shpType);
  return buf;
}

Lv FTTypes[] = {LV(FTString), LV(FTInteger), LV(FTDouble), LV(FTLogical), LV(FTInvalid)};

char * FTLabel( DBFFieldType ft) {
int k;
static char buf[40];
  for (k=0; k<sizeof(FTTypes)/sizeof(FTTypes[0]);k++) {
    if (ft == FTTypes[k].id) return FTTypes[k].sid ;
  }
  sprintf(buf,"Unknown Field Type %d ",ft);
  return buf;
}

fieldType * fillDBFTypes(DBFHandle hDBF) {
fieldType * ft;
int k;
int count =  DBFGetFieldCount(hDBF);
char pszFieldName[12];

  ft = (fieldType *) malloc(count * sizeof(fieldType) );

  for(k=0;k<count;k++) {
    ft[k].Type = DBFGetFieldInfo( hDBF, k, pszFieldName,
                                  &ft[k].width,&ft[k].decimals);
    ft[k].fieldName = (char * ) malloc((strlen(pszFieldName)+1) * sizeof(char));
    strcpy(ft[k].fieldName,pszFieldName);
  }
  return ft;
}

void emptyDBFTypes(DBFHandle hDBF,fieldType * ft) {
int count =  DBFGetFieldCount(hDBF);
int k;
  for(k=0;k<count;k++) {
    free(ft[k].fieldName);
  }
  free(ft);
}

int matchDBFTypes(fieldType * ft1, fieldType * ft2,int count) {
int k;
  for(k=0;k<count;k++) {
    if (ft1[k].Type != ft2[k].Type) return 0;
    if (strcmp(ft1[k].fieldName,ft2[k].fieldName) != 0) return 0;
  }
  return 1;
}

fieldData * newFieldData(aShape * shape) {
fieldData * retval=NULL;
int k;
  retval = (fieldData * ) malloc(shape->nFields * sizeof (fieldData));
  for (k=0;k<shape->nFields;k++) {
    switch (shape->f[k].Type) {
    case FTDouble:
      retval[k].value.d = 0.;
      break;
    case FTInteger:
      retval[k].value.i=0;
      break;
    case FTString:
      retval[k].value.s = NULL;
      break;
    }
  }
  return retval;
}

void destroyFieldData(aShape * shape,fieldData * fD) {
int k;
  for (k=0;k<shape->nFields;k++) {
    switch (shape->f[k].Type) {
    case FTString:
      if (fD[k].value.s != NULL) {
        free(fD[k].value.s);
        fD[k].value.s = NULL;
      }
      break;
    default:
      break;
    }
  }
  free(fD);
}

void rdFieldData (aShape * shapeFile,int recNum, fieldData * fD) {
int k;
const char * string;
  for (k=0;k<shapeFile->nFields;k++) {
    switch (shapeFile->f[k].Type) {
    case FTDouble:
      fD[k].value.d = DBFReadDoubleAttribute(shapeFile->hDBF, recNum, k);
      break;
    case FTInteger:
      fD[k].value.i = DBFReadIntegerAttribute(shapeFile->hDBF, recNum, k);
      break;
    case FTString:
      string = DBFReadStringAttribute(shapeFile->hDBF, recNum, k);
#if 1==0
      if (fD[k].value.s != NULL) {
        free(fD[k].value.s);
        fD[k].value.s = NULL;
      }
      fD[k].value.s = (char *) malloc( (strlen(string)+1) * sizeof(char));
#endif
      fD[k].value.s = (char *) realloc(fD[k].value.s,
                                (strlen(string)+1) * sizeof(char));
      strcpy(fD[k].value.s,string);
      break;
    }
  }
}

void fieldDataPrint(FILE * file, aShape * shape,fieldData * fD) {
int k;
  for (k=0;k<shape->nFields;k++) {
    switch (shape->f[k].Type) {
    case FTDouble:
      fprintf(file,"%*.*f",shape->f[k].width, shape->f[k].decimals, fD[k].value.d);
      /*DBFWriteDoubleAttribute(shapeFile->hDBF, recNum, k, fD[k].value.d);*/
      break;
    case FTInteger:
      fprintf(file,"%*d",shape->f[k].width, fD[k].value.i);
      /*DBFWriteIntegerAttribute(shapeFile->hDBF, recNum, k, fD[k].value.i);*/
      break;
    case FTString:
      fprintf(file,"%*s",shape->f[k].width, fD[k].value.s);
      /*DBFWriteStringAttribute( shapeFile->hDBF, recNum, k, fD[k].value.s);*/
      break;
    }
    if (k < shape->nFields-1) {
      fprintf(file,",");
    } else {
      fprintf(file,"\n");
    }
  }
}

void fieldHeaderPrint(FILE * file, aShape * shape) {
int k;
  for (k=0;k<shape->nFields;k++) {
    fprintf(file,"%*s",shape->f[k].width, shape->f[k].fieldName);
    if (k < shape->nFields-1) {
      fprintf(file,",");
    } else {
      fprintf(file,"\n");
    }
  }
}

void wrtFieldData (aShape * shapeFile,int recNum, fieldData * fD) {
int k;
  for (k=0;k<shapeFile->nFields;k++) {
    switch (shapeFile->f[k].Type) {
    case FTDouble:
      DBFWriteDoubleAttribute(shapeFile->hDBF, recNum, k, fD[k].value.d);
      break;
    case FTInteger:
      DBFWriteIntegerAttribute(shapeFile->hDBF, recNum, k, fD[k].value.i);
      break;
    case FTString:
      DBFWriteStringAttribute( shapeFile->hDBF, recNum, k, fD[k].value.s);
      break;
    }
  }
}

void copyFieldData(aShape * shapeFile,  fieldData * dest,  fieldData * src) {
int k;
  for (k=0;k<shapeFile->nFields;k++) {
    if (shapeFile->f[k].Type == FTString) {
      dest[k].value.s = (char *) realloc(dest[k].value.s,
                             (strlen(src[k].value.s)+1) * sizeof(char));
      strcpy(dest[k].value.s, src[k].value.s);
    } else {
      dest[k] = src[k];
    }
  }
}

int compareFieldData(aShape * shapeFile,  fieldData * fD1,  fieldData * fD2,int * mask) {
int k,retval;
  for (k=0;k<shapeFile->nFields;k++) {
    if ((mask != NULL) && (! mask[k])) continue;
    switch (shapeFile->f[k].Type) {
    case FTDouble:
      if (fD1[k].value.d == fD2[k].value.d) continue;
      if (fD1[k].value.d < fD2[k].value.d) return -1;
      return 1;
    case FTInteger:
      if (fD1[k].value.i == fD2[k].value.i) continue;
      if (fD1[k].value.i < fD2[k].value.i) return -1;
      return 1;
    case FTString:
      if ((retval = strcmp(fD1[k].value.s , fD2[k].value.s)) == 0) continue;
      return retval;
    }
  }
  return 0;
}

void delFieldData(int nFlds,fieldType * ft,fieldData * fd) {
int k;
  for (k=0;k<nFlds;k++) {
    if ((ft[k].Type ==  FTString) && (fd[k].value.s != NULL)) {
      free (fd[k].value.s);
      fd[k].value.s = NULL;
    }
  }
  free (fd);
}

void fieldDataFill(aShape * templat, fieldData * fd, ... ) {
int k;
va_list ap;
va_start(ap,fd);
  for (k=0;k<templat->nFields;k++) {
    switch(templat->f[k].Type) {
    case FTInteger:
      fd[k].value.i = va_arg(ap,int);
      break;
    case FTDouble:
      fd[k].value.d = va_arg(ap,double);
      break;
    case FTString:
      fd[k].value.s = va_arg(ap,char *);
      break;
    }
  }
  va_end(ap);
}

void shapeFieldToData(DBFHandle hDBF,int nRecd,
                         int nFlds,fieldType * ft,fieldData * fd ) {
int k;
const char * string;
  for (k=0;k<nFlds;k++) {
    switch (ft[k].Type) {
    case FTDouble:
      fd[k].value.d = DBFReadDoubleAttribute( hDBF, nRecd, k);
      break;
    case FTInteger:
      fd[k].value.i = DBFReadIntegerAttribute( hDBF, nRecd, k);
      break;
    case FTString:
      string = DBFReadStringAttribute( hDBF, nRecd, k);
      if (fd[k].value.s != NULL) {
        free(fd[k].value.s);
        fd[k].value.s = NULL;
      }
      fd[k].value.s = (char *) malloc( (strlen(string)+1) * sizeof(char));
      strcpy(fd[k].value.s,string);
      break;
    }
  }
}


void shapeDataToField(DBFHandle hDBF,int nRecd,
                         int nFlds,fieldType * ft,fieldData * fd ) {
int k;
  for (k=0;k<nFlds;k++) {
    switch (ft[k].Type) {
    case FTDouble:
      DBFWriteDoubleAttribute( hDBF, nRecd, k, fd[k].value.d);
      break;
    case FTInteger:
      DBFWriteIntegerAttribute( hDBF, nRecd, k, fd[k].value.i);
      break;
    case FTString:
      DBFWriteStringAttribute( hDBF, nRecd, k, fd[k].value.s);
      break;
    }
  }
}

void cpShapeData(DBFHandle hDBFDest,int nRecdDest,
                 DBFHandle hDBFSrc,int nRecdSrc,
                         int nFlds,fieldType * ft) {
int k;
  for (k=0;k<nFlds;k++) {
    switch (ft[k].Type) {
    case FTDouble:
      DBFWriteDoubleAttribute( hDBFDest, nRecdDest, k, 
         DBFReadDoubleAttribute(hDBFSrc, nRecdSrc, k) );
      break;
    case FTInteger:
      DBFWriteIntegerAttribute( hDBFDest, nRecdDest, k, 
         DBFReadIntegerAttribute(hDBFSrc, nRecdSrc, k) );
      break;
    case FTString:
      DBFWriteStringAttribute( hDBFDest, nRecdDest, k, 
         DBFReadStringAttribute(hDBFSrc, nRecdSrc, k) );
      break;
    }
  }
}



void shapeFieldSet(aShape * shape,int recNo,int fieldNo,void * value) {
  switch (shape->f[fieldNo].Type) {
  case FTInteger:
    DBFWriteIntegerAttribute( shape->hDBF, recNo, fieldNo, *(int *)value);
    /*shape->f[fieldNo].value.i = *(int *)value;*/
    break;
  case FTDouble:
    DBFWriteDoubleAttribute( shape->hDBF, recNo,fieldNo, *(double *)value);
    /*shape->f[fieldNo].value.d = *(double *)value;*/
    break;
  case FTString:
    DBFWriteStringAttribute( shape->hDBF, recNo, fieldNo, (char *)value);
    /*shape->f[fieldNo].value.s = (char *) malloc(strlen(value));
    strcpy(shape->f[fieldNo].value.s,(char *)value);*/
    break;
  }
}

int newShapeFile(aShape * shape,const char * ShapeName,int shapeType,int nFields,fieldType * t) {
char buffer[128];
int k;
FILE * prjFile;
char prjString[] = "GEOGCS[\"GCS_North_American_1983\","
                   "DATUM[\"D_North_American_1983\","
                   "SPHEROID[\"GRS_1980\",6378137,298.257222101]],"
                   "PRIMEM[\"Greenwich\",0],"
                   "UNIT[\"Degree\",0.0174532925199433]]"
  ;
  shape->hDBF = NULL;
  shape->hSHP = NULL;
  shape->nFields = 0;
  shape->f = NULL;
  sprintf(buffer,"%s.dbf",ShapeName);
  if ((shape->hDBF = DBFCreate(buffer)) == NULL) {
    fprintf(stderr,"Unable to create Database File %s\n",buffer);
  }
  shape->nFields = nFields;
  shape->f = (fieldType *) malloc(nFields * sizeof(fieldType));
  for (k = 0; k < shape->nFields; k++) {
    DBFAddField(shape->hDBF,t[k].fieldName,t[k].Type,t[k].width,t[k].decimals);
    shape->f[k] = t[k];
     shape->f[k].fieldName = (char *) malloc(strlen(t[k].fieldName)+1);
     strcpy(shape->f[k].fieldName,t[k].fieldName);
  }
#if 1==0
/*Close and open to force write of header terms*/
    DBFClose( shape->hDBF );
  shape->hDBF = DBFOpen( buffer, "r+b" );
#endif

  shape->nShapeType = shapeType;
  setVector2(0.,0.,& shape->min);
  setVector2(0.,0.,& shape->max);
  shape->nRecds = 0.;
  if ((shape->hSHP = SHPCreate(ShapeName,shape->nShapeType)) == NULL) {
    fprintf(stderr,"Unable to Create Shape File %s\n",ShapeName);
  }
  sprintf(buffer,"%s.prj",ShapeName);
  if ((prjFile = fopen(buffer,"w")) == NULL) {
    fprintf(stderr,"Unable to Create Prj File %s\n",buffer);
  }
  fprintf(prjFile,"%s",prjString);
  fclose(prjFile);
  return 0;
}

int ShapeOpen(aShape * shape,char * ShapeName) {
int nEntities,nShpRecords;
int k;
char fieldName[12];
double 	adfMinBound[4], adfMaxBound[4];
  shape->hDBF = NULL;
  shape->hSHP = NULL;
  shape->nFields = 0;
  shape->f = NULL;

   if ((shape->hDBF = DBFOpen( ShapeName, "r"))==NULL) {
     DEBUG_OUT1("Failure to Open %s.dbf\n",ShapeName);
    /* exit(EXIT_FAILURE);*/
     return -1;
   }
   if ((shape->hSHP = SHPOpen(ShapeName, "rb"))==NULL) {
     DEBUG_OUT1("Failure to Open %s.shx\n",ShapeName);
     DBFClose(shape->hDBF);shape->hDBF=NULL;
     /*exit(EXIT_FAILURE);*/
     return -1;
   };
   SHPGetInfo( shape->hSHP, &nEntities, & shape->nShapeType,
       adfMinBound,adfMaxBound);
   shape->nFields = DBFGetFieldCount(shape->hDBF);
   shape->f = (fieldType *) malloc(shape->nFields * sizeof(fieldType));
   for (k=0;k<shape->nFields;k++) {
     shape->f[k].Type = DBFGetFieldInfo( shape->hDBF, k,
           fieldName/*NULL char * pszFieldName*/,
           & shape->f[k].width,  & shape->f[k].decimals);
     shape->f[k].fieldName = (char *) malloc(strlen(fieldName)+1);
     strcpy(shape->f[k].fieldName,fieldName);
   }
   shape->nRecds = nShpRecords = DBFGetRecordCount(shape->hDBF);
   shape->min.x = adfMinBound[0];
   shape->min.y = adfMinBound[1];
   shape->max.x = adfMaxBound[0];
   shape->max.y = adfMaxBound[1];
   if (nShpRecords != nEntities) {
     fprintf(stderr,
        "Error - %s.dbf record count %d not equal to %s.shx record count %d\n",
         ShapeName, nShpRecords, ShapeName, nEntities);
     shape->nRecds = (nShpRecords>nEntities)?nEntities:nShpRecords;
     nEntities = nShpRecords;
     fprintf(stderr,"Using Record Count%d\n",nShpRecords);
   }
   return shape->nRecds;
}

void ShapeClose(aShape * shape) {
int k,nFields = shape->nFields ;
DEBUG_OUT("Closing shapefile\n");
  if (shape->hSHP != NULL) {
    SHPClose( shape->hSHP );
    shape->hSHP = NULL;
  }
  if (shape->f != NULL) {
    for (k=0;k<nFields;k++) {
      if (shape->f[k].fieldName != NULL) {
        free(shape->f[k].fieldName);
        shape->f[k].fieldName = NULL;
      }
    }
    free (shape->f);
    shape->f = NULL;
  }
  if (shape->hDBF != NULL) {
    DBFClose(shape->hDBF);
    shape->hDBF = NULL;
  }
}

void printDbfHeader(aShape * shape) {
int i;
  for( i = 0; i < shape->nFields; i++ ) {
    printf ("Field %d: Type:%10s(%d,%d), %10s\n",i,
          FTLabel(shape->f[i].Type),
          shape->f[i].width,
          shape->f[i].decimals,
          shape->f[i].fieldName);
  } 
}

void copyDbfRecord(aShape * in, int recIn,aShape * out,int recOut,int offset) {
int field, ival;
double dval;
const char * pString;
  for( field = 0; field < in->nFields; field++ ) {
    if (in->f[field].Type != out->f[field+offset].Type) {
      fprintf(stderr,"Error: copyDbfRecord mismatch");
    }
    switch(in->f[field].Type) {
    case FTString:
      pString = DBFReadStringAttribute(in->hDBF,recIn, field);
      DBFWriteStringAttribute(out->hDBF, recOut, field+offset ,pString);
      break;
    case FTInteger:
      ival = DBFReadIntegerAttribute(in->hDBF, recIn, field);
      DBFWriteIntegerAttribute(out->hDBF, recOut, field+offset ,ival);
      break;
    case FTDouble:
      dval = DBFReadDoubleAttribute(in->hDBF, recIn, field);
      DBFWriteDoubleAttribute(out->hDBF, recOut, field+offset ,dval);
      break;
    }
  }
}

int checkObj(SHPObject * obj) {
int parta,partb;
int firsta,lasta,firstb,lastb,vertexa,vertexb;
#define nearEQ(A,B) fabs((A)-(B)) <= DBL_EPSILON * (fabs(A)+fabs(B))
#define nearSAME(OBJ,J,K) (nearEQ(OBJ->padfX[J],OBJ->padfX[K]) && nearEQ(OBJ->padfY[J],OBJ->padfY[K]))
  if (obj->dfXMin > 180. || obj->dfXMin > obj->dfXMax || -180. > obj->dfXMax)
                   return -1;/*Limits out of Range*/
  if (obj->dfYMin > 90. || obj->dfYMin > obj->dfYMax || -90. > obj->dfYMin)
                   return -1;/*Limits out of Range*/
  if (obj->dfXMax > 180. || -180. > obj->dfXMin)
                   return -2; /* Limits outside standard range.*/
  if (obj->dfYMax > 90. || -90. > obj->dfYMin)
                   return -2; /* Limits outside standard range.*/
  for (parta = 0;parta < obj->nParts; parta++) {
    objPartIndices(obj, parta,& firsta, & lasta);
    if (!nearSAME(obj,firsta,lasta-1)) return -3;/*not closed polygon*/
    while(nearSAME(obj,firsta,lasta-1) && (lasta>firsta)) lasta--;
    while(nearSAME(obj,firsta,firsta+1) && (lasta>firsta)) firsta++;
    for (vertexa=firsta;vertexa < lasta ;vertexa++) {
      for (partb = 0; partb <= parta; partb++) {
        int inseq=0;
        objPartIndices(obj, partb,& firstb, & lastb);
        for (vertexb=firstb;vertexb < lastb ;vertexb++) {
          if (vertexb>=vertexa) break;
    /*      if ( nearEQ(obj->padfX[vertexa], obj->padfX[vertexb]) &&
               nearEQ(obj->padfY[vertexa], obj->padfY[vertexb])) {*/
          if ( nearSAME(obj,vertexa,vertexb)) {
              if (parta != partb) return -3; /*Self-Intersecting*/
            inseq = 1;
          } else {
            if (inseq != 0 ) return -3; /*Self-Intersecting*/
          }
        }
      }
    }
  }
  return 0;
}

void printObj(FILE * file,SHPObject * obj) {
int k,l,first,last;
  if (obj->nSHPType != SHPT_POLYGON) return;
  fprintf(file,"%d parts\n",obj->nParts);
  for (k=0;k<obj->nParts;k++) {
    first = obj->panPartStart[k];
    if (k<obj->nParts-1) {
      last = obj->panPartStart[k+1];
    } else {
      last = obj->nVertices;
    }
    fprintf(file,"Part %d, from %d to %d\n",k,first,last-1);
    for (l=first;l<last;l++) {
      fprintf(file," %d,%f,%f\n",l,obj->padfX[l],obj->padfY[l]);
    }
    fprintf(file,"\n");
  }
  fprintf(file,"\n");
}

int objPartIndices(SHPObject * obj,int part,int * first,int* last) {
  if (obj->nSHPType != SHPT_POLYGON) return FALSE;
  if (obj->nParts <= 1) {
    *first = 0;
    *last = obj->nVertices;
    return TRUE;
  }
  if (part >= obj->nParts) return FALSE;
  * first = obj->panPartStart[part];
  if (part == obj->nParts - 1) {
    *last = obj->nVertices;
  } else {
    *last = obj->panPartStart[part+1];
  }
  return TRUE;
}

int shapeGetObj(aShape * shape,int jRecd,SHPObject ** obj) {
int nv,j,k,nParts,redo;
SHPObject * obj2;
  * obj = SHPReadObject( shape->hSHP, jRecd );
  SHPRewindObject( shape->hSHP, *obj );
  if ((*obj)->nSHPType == SHPT_POLYGON) {
  double *XX=NULL,*YY=NULL;
  int * panPartStart;
  int first,last;
    nParts = (*obj)->nParts;
    for (k=0,redo=FALSE;k<nParts;k++) {
      objPartIndices(*obj,k,&first,&last);
      redo = redo || ((*obj)->padfX[first] != (*obj)->padfX[last-1]) ||
                     ((*obj)->padfY[first] != (*obj)->padfY[last-1]);
    }
    if (redo) {
  /*Invalid form; not all parts are closed loops.*/
      XX=(double *) malloc(((*obj)->nVertices + (*obj)->nParts) * sizeof(double));
      YY=(double *) malloc(((*obj)->nVertices + (*obj)->nParts) * sizeof(double));
      panPartStart = (int *) malloc(((*obj)->nParts+1) * sizeof(int));
      panPartStart[0] = nv = 0;
      for (k=0;k<nParts;k++) {
        objPartIndices(*obj,k,&first,&last);
        for (j=first;j<last;j++,nv++) {
          XX[nv] = (*obj)->padfX[j];
          YY[nv] = (*obj)->padfY[j];
        }
        if (  ((*obj)->padfX[first] != (*obj)->padfX[last-1]) ||
                     ((*obj)->padfY[first] != (*obj)->padfY[last-1])) {
           XX[nv] = (*obj)->padfX[first];
           YY[nv] = (*obj)->padfY[first];
           nv++;
        }
        panPartStart[k+1]=nv;
      }
      obj2 = SHPCreateObject( SHPT_POLYGON, (*obj)->nShapeId, nParts, panPartStart, NULL,
             nv, XX, YY, NULL, NULL);
      SHPDestroyObject(*obj);
      *obj = obj2;
      free(XX);free(YY);free(panPartStart);
    }
  }
  SHPComputeExtents( * obj ); /*Make sure bounds are correct.*/
  return TRUE;
}

int shapePutObj(aShape * shape,SHPObject * obj) {
  int recno;
  SHPRewindObject( shape->hSHP, obj );
  recno = SHPWriteObject( shape->hSHP, -1, obj);
  return recno;
  
}

void objGetInfo(SHPObject * obj,vector2 * max, vector2 * min) {
  if (max != NULL) {
    max -> x = obj -> dfXMax;
    max -> y = obj -> dfYMax;
  }
  if (max != NULL) {
    min -> x = obj -> dfXMin;
    min -> y = obj -> dfYMin;
  }
}

static double sinxox(double x) {
double xsq=x*x;
  if ( xsq > 1.e-10) {
    return sin(x)/x;
  } else {
    return (1.  - xsq/6.*(1. - xsq/20.*(1. - xsq/42.)));
  }
}

#ifndef RADPDEG
#define RADPDEG (M_PI/180.)
#endif
#define XXP(K) (RADPDEG * (obj -> padfX[(K)]))
#define YYP(K) (RADPDEG * (obj -> padfY[(K)]))
double areaObjSph(SHPObject * obj, vector2 * centroid) {
double area=0.,area2=0.;
double sumSlat=0.,sumClon=0.,sumSlon=0.;
int iPart,i;
  if (obj -> nParts > 0) {
    for (iPart = 0;iPart < obj -> nParts; iPart++) {
    int first,next;
    double cLatn,sLatn,cLatp,sLatp;
      objPartIndices(obj,iPart,&first,&next);
      cLatn=cos(YYP(first));
      sLatn=sin(YYP(first));
      for(i=first+1;i<next;i++) {
        cLatp=cLatn;sLatp=sLatn;
        cLatn=cos(YYP(i));
        sLatn=sin(YYP(i));
        area += (XXP(i)-XXP(i-1)) * sin((YYP(i) + YYP(i-1))*.5) * 
                        sinxox((YYP(i)-YYP(i-1))*.5);
        area2 -= ((XXP(i)*sLatn - XXP(i-1)*sLatp)) -
               (XXP(i)-XXP(i-1)) * sin((YYP(i) + YYP(i-1))*.5) * 
                      sinxox((YYP(i)-YYP(i-1))*.5);
        if (centroid != NULL) {
          sumSlat += ( (XXP(i) * cos(2.*YYP(i)) - XXP(i-1) * cos(2.*YYP(i-1)) ) -
                 cos(YYP(i)+YYP(i-1)) * sinxox(YYP(i)-YYP(i-1)) *(XXP(i)-XXP(i-1)) ) /4. ;
          sumSlon -=  -.5 * (YYP(i) - YYP(i-1)) * (
                 sinxox( .5*(XXP(i)-XXP(i-1)) ) * cos( .5*(XXP(i)+XXP(i-1)) )+
            .5* cos( YYP(i)+YYP(i-1) + .5*(XXP(i) + XXP(i-1)) ) *
                  sinxox( YYP(i)-YYP(i-1) + .5*(XXP(i) - XXP(i-1)) ) +
            .5* cos( YYP(i)+YYP(i-1) - .5*(XXP(i) + XXP(i-1)) )*
                  sinxox( YYP(i)-YYP(i-1) - .5*(XXP(i) - XXP(i-1)) )
                );
          sumClon -= .5 * (YYP(i) - YYP(i-1)) * (
                 sinxox( .5*(XXP(i)-XXP(i-1)) ) * sin( .5*(XXP(i)+XXP(i-1)) )+
            .5* sin( YYP(i)+YYP(i-1) + .5*(XXP(i) + XXP(i-1)) ) *
                  sinxox( YYP(i)-YYP(i-1) + .5*(XXP(i) - XXP(i-1)) ) +
            .5* sin(.5*(XXP(i) + XXP(i-1))-(YYP(i)+YYP(i-1)) ) *
                  sinxox( YYP(i)-YYP(i-1) - .5*(XXP(i) - XXP(i-1)) )
                );
        }
      }
     /* printf("area=%f,%f xc=%f,yc=%f zc=%f,angle=%f,%f\n",area,area2,sumClon,sumSlon,sumSlat,atan2(sumSlon,sumClon)/RADPDEG, atan2(sumSlat,sqrt(sumSlon*sumSlon+sumClon*sumClon))/RADPDEG );*/
    }
  }
  if (centroid != NULL) {
     setVector2(atan2(sumSlon,sumClon)/RADPDEG,
                atan2(sumSlat,sqrt(sumSlon*sumSlon+sumClon*sumClon))/RADPDEG,
                centroid);
  }
  return REARTH * REARTH * area;
}
#undef XXP
#undef YYP

double areaObj(SHPObject * obj, vector2 * centroid) {
double area=0.,xcent=0.,ycent=0.,area2=0.;
double sumx,sumy,delx,dely;
int iPart,i;
  if (obj -> nParts > 0) {
    for (iPart = 0;iPart < obj -> nParts; iPart++) {
    int first,next;
      objPartIndices(obj,iPart,&first,&next);
      for(i=first+1;i<next;i++) {
        sumx = (obj -> padfX[i] + obj -> padfX[i-1]);
        sumy = (obj -> padfY[i] + obj -> padfY[i-1]);
        dely = (obj -> padfY[i] - obj -> padfY[i-1]);
        delx = (obj -> padfX[i] - obj -> padfX[i-1]);
        area -= .5 * sumx * dely;
        area2 += .5 * sumy * delx;
        xcent += delx * ((sumx * sumy)/4. + delx*dely/12.);
        ycent -= dely * ((sumx * sumy)/4. + delx*dely/12.);
      }
    }
  } else {
    for(i=1;i < obj -> nVertices;i++) {
      sumx = (obj -> padfX[i] + obj -> padfX[i-1]);
      sumy = (obj -> padfY[i] + obj -> padfY[i-1]);
      dely = (obj -> padfY[i] - obj -> padfY[i-1]);
      delx = (obj -> padfX[i] - obj -> padfX[i-1]);
      area -= .5 * sumx * dely;
      xcent += delx * ((sumx * sumy)/4. + delx*dely/12.);
      ycent -= dely * ((sumx * sumy)/4. + delx*dely/12.);
    }
  }
  if ((area != 0.) && (centroid != NULL)) {
    centroid->x = xcent/area;
    centroid->y = ycent/area;
  }
  return area;
}

int shpBoxOverlap(SHPObject * a,SHPObject * b) {
  if (a->dfXMax < b->dfXMin) return FALSE;
  if (b->dfXMax < a->dfXMin) return FALSE;
  if (a->dfYMax < b->dfYMin) return FALSE;
  if (b->dfYMax < a->dfYMin) return FALSE;
  return TRUE;
}

int limitsPartObj(int partno,SHPObject * obj,double * xmin,double * ymin,
   double * xmax, double * ymax){
#define PX(k) obj->padfX[k]
#define PY(k) obj->padfY[k]
  int first,last,k;
  if (partno<0 || partno >= obj->nParts) return -1;
  objPartIndices(obj,partno,&first,&last);
  *xmin=*xmax=PX(first);*ymin=*ymax=PY(first);
  for (k=first+1;k<last;k++) {
    *xmin = (*xmin<PX(k)) ? (*xmin):PX(k);
    *xmax = (*xmax>PX(k)) ? (*xmax):PX(k);
    *ymin = (*ymin<PY(k)) ? (*ymin):PY(k);
    *ymax = (*ymax>PY(k)) ? (*ymax):PY(k);
  }
#undef PX
#undef PY
  return 0;
}

int insidePartObj(int partno,SHPObject * obj,int count,int * flag, vector2 * point) {
#define PVX(K) obj->padfX[K]
#define PVY(K) obj->padfY[K]
#define PTX(K) point[K].x
#define PTY(K) point[K].y
int firstV,lastV,vertex,k;
int * winding,result,first;
double xintercept;
  if (obj->nSHPType != SHPT_POLYGON) return -1;
  if (count <= 0) return -1;
  if (flag == NULL) {
    first = count-1;
    winding = &result;
  } else {
    first = 0;
    winding = flag;
  }
  if (partno<0 || partno >= obj->nParts) return -2;
  for (k=first;k<count;k++) winding[k]=0;
  objPartIndices(obj,partno,&firstV,&lastV);
  for (vertex=firstV;vertex<lastV-1;vertex++) {
    for (k=first;k<count;k++) {
      if ((PVY(vertex) <= PTY(k)) && (PVY(vertex+1) <  PTY(k)) ) continue;
      if ((PVY(vertex) >  PTY(k)) && (PVY(vertex+1) >  PTY(k)) ) continue;
      if ((PVY(vertex) <  PTY(k)) && (PVY(vertex+1) == PTY(k)) ) continue;
      if ((PVY(vertex) == PTY(k)) && (PVY(vertex+1) == PTY(k)) ) {
        if ((PVX(vertex) >= PTX(k)) && (PVX(vertex+1) < PTX(k))) winding[k-first]++;
        continue;
      }
      xintercept = ( PVX(vertex) * (PVY(vertex+1) - PTY(k))  +
                    PVX(vertex+1) * (PTY(k) - PVY(vertex) ) ) /
                   ( PVY(vertex+1) - PVY(vertex) );
      if (xintercept < PTX(k)) continue;

      if (PVY(vertex) <= PTY(k) && PVY(vertex+1) >  PTY(k)) {
        winding[k-first]--; continue;
      }
      if (PVY(vertex) >  PTY(k) && PVY(vertex+1) <= PTY(k)) {
        if (xintercept > PTX(k)) winding[k-first]++; continue;
      }
    }
  }
  return winding[count-1-first];

#undef PVX
#undef PVY
#undef PTX
#undef PTY
}

/* code copied from splitobj.c --------------------------------------*/

int objPartsInside(int j, int k, SHPObject * obj) {
/*returns -1 if part j inside part k, +1 if part k inside part j, 0 otherwise.*/
int venn;
vector2 partAmin,partAmax;
vector2 partBmin,partBmax;
vector2 point;
  limitsPartObj(j, obj, &partAmin.x,&partAmin.y, &partAmax.x,&partAmax.y);
  limitsPartObj(k, obj, &partBmin.x,&partBmin.y, &partBmax.x,&partBmax.y);
    venn = boxVenn(&partAmin, &partAmax,  &partBmin, &partBmax);
  if ((venn == 0) || (venn == 3) ) return 0;   /*Neither a inside b nor b inside a*/
  switch (venn) {
  case 0:case 3:  return 0;   /*Neither a inside b nor b inside a*/
  case 1: /*Limits of j(A) inside limits of k(B).*/
    point.x = obj->padfX[obj->panPartStart[j]];
    point.y = obj->padfY[obj->panPartStart[j]];
    if ( insidePartObj(k, obj, 1, NULL, &point)) {
      return -1;
    }
    return 0;
  case 2:
    point.x = obj->padfX[obj->panPartStart[k]];
    point.y = obj->padfY[obj->panPartStart[k]];
    if ( insidePartObj(j, obj, 1, NULL, &point)) {
      return 1;
    }
    return 0;
  }
  return 0;
}

preObject * createPreObject(void) {
preObject * retval = (preObject *) malloc(sizeof(preObject));
  retval->nParts = retval->nVertices = 0;
  retval->panPartStart = NULL;
  retval->XX = retval->YY = NULL;
  return retval;
}

void destroyPreObject(preObject * a) {
  free(a->XX); a->XX = NULL;
  free(a->YY); a->YY = NULL;
  free(a->panPartStart);a->panPartStart = NULL;
  free(a);
}

SHPObject * createObjectFromPre(preObject * obj) {
SHPObject * a;
  a = SHPCreateObject(SHPT_POLYGON,-1,obj->nParts,obj->panPartStart,
              NULL,obj->nVertices,obj->XX,obj->YY,NULL,NULL);
  return a;
}

void dubPreObject(SHPObject * objIn,int partk,preObject * objOut) {
int first,last,m,k;
int firstP,lastP,iP,totParts,totVertices;
  if ( partk >= objIn->nParts) return;
  m=objOut->nVertices;
  if (partk < 0) {
    firstP=0;lastP=objIn->nParts;
    first = 0;last = objIn->nVertices;
    totVertices = objOut->nVertices + objIn->nVertices;
  } else {
    firstP=partk;lastP=partk+1;
    objPartIndices(objIn, partk, &first, &last) ;
    totVertices = objOut->nVertices + last-first;
  }
  totParts=lastP-firstP+objOut->nParts;
  objOut->panPartStart = (int *)realloc(objOut->panPartStart,
                  totParts * sizeof(int));
  objOut->XX = (double *) realloc( objOut->XX,
                             totVertices * sizeof(double));
  objOut->YY = (double *) realloc( objOut->YY,
                             totVertices * sizeof(double));
  for (k=first;k<last;k++) {
    objOut->XX[k+m-first] = objIn->padfX[k];
    objOut->YY[k+m-first] = objIn->padfY[k];
  }
  for (iP=firstP;iP<lastP;iP++) {
    objOut->panPartStart[iP-firstP+objOut->nParts] =
            objOut->nVertices +  objIn->panPartStart[iP];
  }
  objOut->nVertices = totVertices;
  objOut->nParts = totParts;
}

preObject * catPreObject(preObject * a,preObject * b) {
int k,m;
  if (b->nParts == 0) return a;
  a->panPartStart = (int *) realloc(a->panPartStart,
                             (a->nParts+b->nParts) * sizeof(int));
  a->XX = (double *) realloc(a->XX,
                             (a->nVertices+b->nVertices) * sizeof(double));
  a->YY = (double *) realloc(a->YY,
                             (a->nVertices+b->nVertices) * sizeof(double));
  for (k=0,m=a->nParts; k<b->nParts; k++) {
    a->panPartStart[m+k] = a->nVertices + b->panPartStart[k];
  }
  a->nParts += b->nParts;
  for (k=0,m=a->nVertices;k<b->nVertices;k++) {
    a->XX[m+k] = b->XX[k];
    a->YY[m+k] = b->YY[k];
  }
  a->nVertices += b->nVertices;
  return a;
}

void dubPart(SHPObject * objIn,int k,double * XX,double * YY,
             int * panPartStart, int * partNo) {
int ncopy;
int k1,k2;
int m,xofset;
  k1 = objIn->panPartStart[k];
  if (k < objIn->nParts-1) {
    k2 = objIn->panPartStart[k+1];
  } else {
    k2 = objIn->nVertices;
  }
  ncopy = k2-k1;
  xofset = panPartStart[* partNo];
  panPartStart[(* partNo) + 1] = xofset + ncopy;
  for (m=0;m<ncopy;m++) {
    XX[xofset + m] = objIn->padfX[k1 + m];
    YY[xofset + m] = objIn->padfY[k1 + m];
  }
  (* partNo) ++;
}


int splitObj(SHPObject * objIn,SHPObject ** objOut) {
int iObj = 0;

typedef struct {
  int prev;
  int layer;
} link;

link * ptr=NULL;
int j,k,m,n;
double * XX=NULL, *YY=NULL;
int * panPartStart=NULL;
int partNo;
vector2 partMin,partMax;
  ptr = (link *) malloc( objIn->nParts * sizeof(link));
  for (k=0;k<objIn->nParts;k++) {
    ptr[k].prev = -1;
    ptr[k].layer = 0;
    limitsPartObj(k, objIn, &partMin.x,&partMin.y, &partMax.x,&partMax.y);
  }
/* Determine which parts are inside which.  A given part of object k is inside
 * ptr[k].layer other parts, and the innermost other part that it is inside is
 * part number ptr[k].prev.
 */

  for(k=0; k<objIn->nParts; k++) {
    ptr[k].prev = -1;
    for (j=0; j<objIn->nParts; j++) {
      if (k == j) continue;
      if (objPartsInside( j, k, objIn) == 1) {
      /*Part k inside part j.*/
        if (ptr[k].prev < 0) {
          ptr[k].prev = j;
        } else {
          if (objPartsInside( ptr[k].prev, j, objIn) == 1) {
            ptr[k].prev = j;
          }
        }
      }
    }
  }
  for(k=0; k<objIn->nParts; k++) {
    for (j=k, ptr[k].layer = 0;ptr[j].prev>=0;j=ptr[j].prev) {
      ptr[k].layer ++;
    }
  }
/*At this stage, inside/outside hierarchy of object parts should be recorded in ptr array.*/
  panPartStart = (int *) malloc((objIn->nParts+1)*sizeof(int));
  panPartStart[0] = 0;
  XX = (double *) malloc((objIn->nVertices) * sizeof(double ));
  YY = (double *) malloc((objIn->nVertices) * sizeof(double ));
  for (k=0;k<objIn->nParts;k++) {
    if ((ptr[k].layer & 1) !=  0 ) continue;
    partNo=0;
    dubPart(objIn, k, XX, YY, panPartStart,& partNo);
    for (m=0;m < objIn->nParts; m++) {
      if (m == k) continue;
      n=m;
      while (ptr[n].prev >= 0) {
        if (ptr[n].prev == k) {
          dubPart(objIn, m, XX, YY, panPartStart,& partNo);
        }
        n=ptr[n].prev;
      }
    }
    objOut[iObj] = SHPCreateObject(SHPT_POLYGON, objIn->nShapeId, partNo,
                    panPartStart,NULL, panPartStart[partNo],XX,YY,NULL,NULL);
    iObj++;
  }


  free(XX);XX=NULL;free(YY);YY=NULL;
  free(panPartStart);panPartStart=NULL;
  free(ptr);ptr = NULL;
  return iObj;
}

/* end code copied from splitobj.c --------------------------------------*/

int insideObj(int count,vector2 * point,int * flag,SHPObject * obj) {
int k,first=0,result;
int partno;
int vertex,firstV,lastV;
int * winding;
double xintercept;
  if (obj->nSHPType != SHPT_POLYGON) return -1;
  if (count <= 0) return -1;
  if (flag == NULL) {
    first = count-1;
    winding = &result;
  } else {
    first = 0;
    winding = flag;
  }
#define PTX point[k].x
#define PTY point[k].y
#define PVX(K) obj->padfX[K]
#define PVY(K) obj->padfY[K]
  for (k=first;k<count;k++) winding[k-first]=0;
  for (partno = 0; partno < obj->nParts; partno++) {
    firstV = obj->panPartStart[partno];
    lastV = (partno < obj->nParts -1)?(obj->panPartStart[partno+1]):(obj->nVertices);
    for (vertex=firstV;vertex<lastV-1;vertex++) {
      for (k=first;k<count;k++) {
        if ((PVY(vertex) <= PTY) && (PVY(vertex+1) <  PTY) ) continue;
        if ((PVY(vertex) >  PTY) && (PVY(vertex+1) >  PTY) ) continue;
        if ((PVY(vertex) <  PTY) && (PVY(vertex+1) == PTY) ) continue;
        if ((PVY(vertex) == PTY) && (PVY(vertex+1) == PTY) ) {
          if ((PVX(vertex) >= PTX) && (PVX(vertex+1) < PTX)) winding[k-first]++;
          continue;
        }
        xintercept = ( PVX(vertex) * (PVY(vertex+1) - PTY)  +
                      PVX(vertex+1) * (PTY - PVY(vertex) ) ) /
                     ( PVY(vertex+1) - PVY(vertex) );
        if (xintercept < PTX) continue;

        if (PVY(vertex) <= PTY && PVY(vertex+1) >  PTY) {
          winding[k-first]--; continue;
        }
        if (PVY(vertex) >  PTY && PVY(vertex+1) <= PTY) {
          if (xintercept > PTX) winding[k-first]++; continue;
        }
      }
    }
  }
  return winding[count-1-first];
}
#undef PTX
#undef PTY
#undef PVX
#undef PVY

int insidePolygons(int npts,vector2 pts[],int flag[] ,aShape * shape) {
/* if shape is a polygon, determines which of the npts vectors pts[]
 * lie inside any part of shape.  Returns in the npts integers flag[k]
 * a +1 if pts[k] is inside, and 0 if not.
 */
SHPObject * objA=NULL;
int kRecd,kpt,result=0,* tFlag;
  if (flag != NULL) {
    tFlag = (int *) malloc(npts * sizeof(int));
    for (kpt = 0;kpt < npts;kpt++) flag[kpt] = 0;
  } else {
    tFlag = NULL;
  }
  for (kRecd=0; kRecd<shape->nRecds; kRecd++) {
    shapeGetObj(shape ,kRecd,&objA);
    result += insideObj( npts, pts , tFlag, objA);
    if (flag != NULL) {
      for(kpt = 0;kpt < npts;kpt++) flag[kpt] += tFlag[kpt];
    }
    SHPDestroyObject(objA);
  }
  if (tFlag != NULL) {free(tFlag);tFlag = NULL;}
  return result;
}

void insideShape(SHPObject * obj,int nPoints,vector2 * pts,int * isInside) {
int iPoint,iPart,iVert,first,last;
double dx1,dy1,dx2,dy2,area;
  for (iPoint=0;iPoint<nPoints;iPoint++) isInside[iPoint] = 0;
  if(obj -> nParts <= 0) return;
  for (iPart=0;iPart < obj->nParts;iPart++) {
    first = obj->panPartStart[iPart];
    last = (iPart <obj->nParts-1)? obj->panPartStart[iPart+1]:obj->nVertices;
    for (iVert = first+1; iVert < last; iVert++) {
      for (iPoint=0;iPoint<nPoints;iPoint++) {
        if ( (obj->padfY[iVert-1] <= pts[iPoint].y) &&
                                         (obj->padfY[iVert] > pts[iPoint].y)) {
          dx1 = obj->padfX[iVert-1] -pts[iPoint].x;
          dx2 = obj->padfX[iVert] -pts[iPoint].x;
          dy1 = obj->padfY[iVert-1] -pts[iPoint].y;
          dy2 = obj->padfY[iVert] -pts[iPoint].y;
          area =  ( dx1*dy2 - dy1*dx2);
          if ( area  > 0. ) isInside[iPoint] --;
          else if (area == 0.) {
            isInside[iPoint] = -2;
            break;
          }
        } else if  ( (obj->padfY[iVert-1] > pts[iPoint].y) &&
                                        (obj->padfY[iVert] <= pts[iPoint].y)) {
          dx1 = obj->padfX[iVert-1] -pts[iPoint].x;
          dx2 = obj->padfX[iVert] -pts[iPoint].x;
          dy1 = obj->padfY[iVert-1] -pts[iPoint].y;
          dy2 = obj->padfY[iVert] -pts[iPoint].y;
          area =  ( dx1*dy2 - dy1*dx2);
          if ( area < 0. ) isInside[iPoint] ++;
          else if (area == 0.) {
            isInside[iPoint] = -2;
            break;
          }
        } else if ( (obj->padfY[iVert-1] == pts[iPoint].y) &&
                                         (obj->padfY[iVert] == pts[iPoint].y)) {
          if ( ((obj->padfX[iVert-1] <= pts[iPoint].x) &&
                                       (obj->padfX[iVert] >= pts[iPoint].x)) ||
               ((obj->padfX[iVert-1] >= pts[iPoint].x) &&
                                       (obj->padfX[iVert] <= pts[iPoint].x)) ){
            isInside[iPoint] = -2;
            break;
          }
        }
      }
    }
  }
}
