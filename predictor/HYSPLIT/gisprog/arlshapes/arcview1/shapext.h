#ifndef _SHAPEEXT_H_
#define _SHAPEEXT_H_
#include <shapefil.h>
#include "vector2.h"

#ifndef REARTH
#define REARTH 6367.47
#endif /*REARTH : radius of Earth in km */

typedef struct {
DBFFieldType Type;
char * fieldName;
int width,decimals;
} fieldType;

typedef struct {
int nParts,nVertices;
int * panPartStart;
double *XX, *YY;
} preObject;

typedef struct {
union {
double d;int i;char * s;} value;
} fieldData;

typedef struct {
DBFHandle hDBF;
SHPHandle hSHP;
int nRecds,nFields,nShapeType;
fieldType * f;
vector2 min,max;
}aShape;

char * SHPLABEL(int shpType);

char * FTLabel( DBFFieldType ft) ;

fieldType * fillDBFTypes(DBFHandle hDBF) ;
void emptyDBFTypes(DBFHandle hDBF,fieldType * ft) ;

int matchDBFTypes(fieldType * ft1, fieldType * ft2,int count) ;

fieldData * newFieldData(aShape * shape ) ;
void destroyFieldData(aShape * shape,fieldData * fD) ;
void rdFieldData (aShape * shapeFile,int recNum, fieldData * fD) ;
int compareFieldData(aShape * shapeFile,  fieldData * fD1,  fieldData * fD2,
                       int * mask) ;
void fieldHeaderPrint(FILE * file, aShape * shape) ;
void fieldDataPrint(FILE * file, aShape * shape,fieldData * fD) ;
void wrtFieldData (aShape * shapeFile,int recNum, fieldData * fD) ;
void copyFieldData(aShape * shapeFile,  fieldData * dest,  fieldData * src) ;
void delFieldData(int nFlds,fieldType * ft,fieldData * fd) ;

void cpShapeData(DBFHandle hDBFDest,int nRecdDest,
                 DBFHandle hDBFSrc,int nRecdSrc,
                 int nFlds,fieldType * ft) ;

void fieldDataFill(aShape * templat, fieldData * fd, ... ) ;

void shapeDataToField(DBFHandle hDBF,int nRecd,
                 int nFlds,fieldType * ft,fieldData * fd ) ;

void shapeFieldToData(DBFHandle hDBF,int nRecd,
                         int nFlds,fieldType * ft,fieldData * fd ) ;


int newShapeFile(aShape * shape,const char * ShapeName,int shapeType,int nFields,fieldType * t) ;

int ShapeOpen(aShape * shape,char * ShapeName) ;
void ShapeClose(aShape * shape) ;

void printDbfHeader(aShape * shape) ;
void copyDbfRecord(aShape * in, int recIn,aShape * out,int recOut,int offset) ;

int checkObj(SHPObject * obj) ;

int objPartIndices(SHPObject * obj,int part,int * first,int* last) ;

void shapeFieldSet(aShape * shape,int recNo,int fieldNo,void * value) ;

void printObj(FILE * file,SHPObject * obj) ;

int shapeGetObj(aShape * shape,int jRecd,SHPObject ** obj) ;
int shapePutObj(aShape * shape,SHPObject * obj) ;

void objGetInfo(SHPObject * obj,vector2 * max, vector2 * min) ;
double areaObj(SHPObject * obj, vector2 * centroid) ;
double areaObjSph(SHPObject * obj, /*@null@*/ vector2 * centroid) ;

preObject * createPreObject(void) ;
void destroyPreObject(preObject * a) ;
void dubPreObject(SHPObject * objIn,int partk,preObject * objOut) ;
preObject * catPreObject(preObject * a,preObject * b) ;
SHPObject * createObjectFromPre(preObject * obj) ;

int limitsPartObj(int partno,SHPObject * obj,double * xmin,double * ymin,
   double * xmax, double * ymax);
int insidePartObj(int partno,SHPObject * obj,int count,int * flag, vector2 * point) ;

int insideObj(int count,vector2 * point,int * flag,SHPObject * obj) ;
int insidePolygons(int npts,vector2 pts[],int flag[] ,aShape * shape) ;
void insideShape(SHPObject * obj,int nPoints,vector2 * pts,int * isInside) ;
int splitObj (SHPObject * objIn, SHPObject ** objOut) ;

int shpBoxOverlap(SHPObject * a,SHPObject * b) ;

#endif /*_SHAPEEXT_H_*/
