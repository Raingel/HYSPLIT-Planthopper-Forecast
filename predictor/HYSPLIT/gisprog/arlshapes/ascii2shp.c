/*      
 * $Id: ascii2shp.c,v 1.8 2000/06/12 13:23:35 jan Exp $
 *
 * Copyright (C) 1999 by Jan-Oliver Wagner <jan@intevation.de>
 * 
 *    This program is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU General Public License
 *   as published by the Free Software Foundation; either version 2
 *   of the License, or (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#define PROGNAME "ascii2shp"
#define MAJORV 1
#define MINORV 1
#define PATCHLEVEL 10
#define LVERSION MAJORV.MINORV.PATCHLEVEL
#define XSUB(V) #V
#define SUB(V) XSUB(V)

#define UnxpTyp(VAL,LINE) fprintf(stderr,"Unexpected type %d in line %d of file %s\n",VAL,LINE,__FILE__);fflush(stderr);exit(1);

#include "config.h"
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#ifndef HAVE_STRCHR
#define INDEX index
#else
#define INDEX strchr
#endif /*HAVE_STRCHR*/

#ifndef HAVE_STRRCHR
#define RINDEX rindex
#else
#define RINDEX strrchr
#endif /*HAVE_STRRCHR*/

#include <shapefil.h>

#include "utils.h"
/*#include "shapext.h"*/

#define VERSION "0.3.0"

/* Error codes for exit() routine: */

enum {ERR_USAGE=1,ERR_TYPE,ERR_FORMAT,ERR_OBJECTTYPE,ERR_ALLOC};
enum {ERR_DBFCREATE=10,ERR_DBFADDFIELD,ERR_DBFOPEN,ERR_DBFWRITEINTEGERATTRIBUTE,
   ERR_DBFWRITEDOUBLEATTRIBUTE};

#define ERR_SHPOPEN  20

/* Object Type codes used in main(): */
#define OBJECTTYPE_NONE    0
#define OBJECTTYPE_POINT  1
#define OBJECTTYPE_LINE    2
#define OBJECTTYPE_POLYGON  3

/* minimum number of coordinates allocated blockwise */
#define COORDS_BLOCKSIZE  100

/* maximum length for read strings,
 * if input lines with more characters appear,
 * errors are likely to occur */
#define STR_BUFFER_SIZE    300

#ifdef USE_STRICMP
#define CASE_INSENSITIVE_STR_CMP  stricmp
#else
#define CASE_INSENSITIVE_STR_CMP  strcasecmp
#endif
DBFFieldType fType=FTInteger;

#if 1==0
int getStripLine(FILE * fp,char * buffer) {
/*Added code to get rid of cr in crlf of DOS format files.*/
char * ptr;
int p;
DEBUG_OUT("calling getline\n");
  p = getline(fp,buffer);
DEBUG_OUT1("size of line = %d\n",p);
  if (p == EOF) return p;
DEBUG_OUT1("p not eof (%d)\n",EOF);
  if ((ptr = strchr(buffer,'\r')) != NULL) {
    *ptr = 0;
    p = (int) (* (ptr-1));
  }
DEBUG_OUT("strchr called\n");
  return p;
}
#endif


void print_version(FILE *file)
{
  fprintf(file,"ascii2shp version " VERSION "\n"); 
  fprintf(file,"Copyright (C) 1999 by Jan-Oliver Wagner.\n"
    "The GNU GENERAL PUBLIC LICENSE applies. "
    "Absolutly No Warranty!\n");
#ifdef DEBUG
  fprintf(file,"compiled with option: DEBUG\n"); 
#endif
}

static DBFHandle LaunchDbf (  const char *fname ) {
  DBFHandle  hDBF;
  char    dbffname[STR_BUFFER_SIZE];
  char    fieldname[STR_BUFFER_SIZE];

  sprintf(dbffname, "%s.dbf", fname);
  /*sprintf(fieldname, "%s-id", fname);*/
  sprintf(fieldname, "id");

  hDBF = DBFCreate( dbffname );
  if( hDBF == NULL ) {
    fprintf(stderr, "DBFCreate(%s) failed.\n", fname );
    exit(ERR_DBFCREATE);
  }
   switch (fType) {
   case FTInteger:
     if (DBFAddField( hDBF, fieldname, FTInteger, 11, 0 ) == -1) {
      fprintf(stderr, "DBFAddField(hDBF,%s,FTInteger,11,0) failed.\n", fieldname);
       exit(ERR_DBFADDFIELD);
     }
     break;
   case FTDouble:
     if (DBFAddField( hDBF, fieldname, FTDouble, 12, 4 ) == -1) {
       fprintf(stderr, "DBFAddField(hDBF,%s,FTDouble,12,4) failed.\n", fieldname);
       exit(ERR_DBFADDFIELD);
     }
     break;
   default:
     fprintf(stderr, "Field Type Unknown\n");fflush(stderr);
      exit(ERR_DBFADDFIELD);
   }

  DBFClose( hDBF );

  hDBF = DBFOpen( dbffname, "r+b" );
  if( hDBF == NULL ) {
    fprintf(stderr, "DBFOpen(%s,\"r+b\") failed.\n", dbffname );
    exit(ERR_DBFOPEN);
  }

  return hDBF;
}

static SHPHandle LaunchShp(  const char *fname,
        int ObjectType ) {
  SHPHandle  hSHP;
  char    shpfname[STR_BUFFER_SIZE];

  sprintf(shpfname, "%s.shp", fname);

  switch (ObjectType) {
    case OBJECTTYPE_POINT:
      hSHP = SHPCreate( shpfname, SHPT_POINT );
      break;
    case OBJECTTYPE_LINE:
      hSHP = SHPCreate( shpfname, SHPT_ARC );
      break;
    case OBJECTTYPE_POLYGON:
      hSHP = SHPCreate( shpfname, SHPT_POLYGON );
      break;
    default:
      fprintf(stderr, "internal error: "
        "unknown ObjectType=%d\n", ObjectType);
      exit(ERR_OBJECTTYPE);
  }

  if( hSHP == NULL ) {
    fprintf(stderr, "SHPOpen(%s, shape_type) failed.\n", shpfname );
    exit(ERR_SHPOPEN);
  }

  return hSHP;
}

static int LaunchPrj(  const char *fname ) {
FILE * prjFile;
char shpfname[STR_BUFFER_SIZE];
char prjString[] = "GEOGCS[\"GCS_North_American_1983\","
                   "DATUM[\"D_North_American_1983\","
                   "SPHEROID[\"GRS_1980\",6378137,298.257222101]],"
                   "PRIMEM[\"Greenwich\",0],"
                   "UNIT[\"Degree\",0.0174532925199433]]"
  ;

  sprintf(shpfname, "%s.prj", fname);
  prjFile = fopen(shpfname,"w");
  fprintf(prjFile,"%s",prjString);
  fclose(prjFile);
  return 0;
}

static void WriteDoubleDbf (  DBFHandle hDBF,
      int rec,
      double id ) {
  if (! DBFWriteDoubleAttribute(hDBF, rec, 0, id)) {
    fprintf(stderr, "DBFWriteDoubleAttribute(hDBFs,%d,1,%f) failed.\n", rec, id );
    exit(ERR_DBFWRITEDOUBLEATTRIBUTE);
  }
}
static void WriteIntegerDbf (  DBFHandle hDBF,
      int rec,
      int id ) {
  if (! DBFWriteIntegerAttribute(hDBF, rec, 0, id)) {
    fprintf(stderr, "DBFWriteIntegerAttribute(hDBFs,%d,1,%d) failed.\n", rec, id );
    exit(ERR_DBFWRITEINTEGERATTRIBUTE);
  }
}


/*--------------------------- WritePoint to SHP file---------------------------
 *
 */

static void WritePoint(  SHPHandle hSHP,
      int rec,
      double x,
      double y ) {
  SHPObject  *psShape;

  psShape = SHPCreateObject( SHPT_POINT, rec, 0, NULL, NULL,
                               1, &x, &y, NULL, NULL );
  SHPWriteObject( hSHP, -1, psShape );
  SHPDestroyObject( psShape );
}

/*--------------------------- WriteLine to SHP file---------------------------
 *
 */

static void WriteLine(  SHPHandle hSHP,
      int rec,
      int coords,
      double * x,
      double * y ) {
  SHPObject  *psShape;

  psShape = SHPCreateObject( SHPT_ARC, rec, 0, NULL, NULL,
    coords, x, y, NULL, NULL );
  SHPWriteObject( hSHP, -1, psShape );
  SHPDestroyObject( psShape );
}

/*-------------------------- WritePolygon to SHP file---------------------------
 *
 */

static void WritePolygon(  SHPHandle hSHP,
        int rec,
        int coords,
        double * x,
        double * y,
        int nparts,
        int * partstarts) {
  SHPObject  *psShape;
  double * xx = (double *) malloc((coords+nparts+1)*sizeof(double));
  double * yy = (double *) malloc((coords+nparts+1)*sizeof(double));
  int * ppstarts=NULL;
  int k,kv,l,first,last;
/*Code modified 5/10/05 to allow for incomplete polynomials being offerred.*/
  if (nparts > 0) {
    ppstarts = (int *) malloc(nparts * sizeof(int));
    for (k=0,kv=0;k<nparts;k++) {
      first = partstarts[k];
      if (k == nparts-1) {
        last = coords;
      } else {
        last = partstarts[k+1];
      }
      ppstarts[k]=kv;
      for (l=first;l<last;l++) {
        xx[kv]=x[l];yy[kv]=y[l];
        kv++;
      }
      if ( (x[last-1] != x[first]) || (y[last-1] != y[first]) ) {
        xx[kv]=x[first];yy[kv]=y[first];
        kv++;
      }
    }
  } else {
    first = 0;last = coords;
    for (l=first,kv=0;l<last;l++) {
      xx[kv]=x[l];yy[kv]=y[l];
      kv++;
    }
    if ( (x[last-1] != x[first]) || (y[last-1] != y[first]) ) {
      xx[kv]=x[first];yy[kv]=y[first];
      kv++;
    }
  }

DEBUG_OUT1("WritePolygon: rec = %d\n", rec);
DEBUG_OUT1("WritePolygon: nparts = %d\n", nparts);
DEBUG_OUT1("WritePolygon: coords = %d\n", coords);

  psShape = SHPCreateObject( SHPT_POLYGON, rec, nparts, ppstarts, NULL,
    kv, xx, yy, NULL, NULL );
/*Introduced 10/18/05 by A. D. Taylor to correct for shape orientation*/

  SHPRewindObject(hSHP, psShape);


  SHPWriteObject( hSHP, -1, psShape );
  SHPDestroyObject( psShape );
  free (xx);free(yy);if (ppstarts != NULL) {free(ppstarts);ppstarts=NULL;}
}

/* read from fp and generate point shapefile to hDBF/hSHP */
static void GeneratePoints (  FILE *fp,
        DBFHandle hDBF,
        SHPHandle hSHP ) {
char linebuf[STR_BUFFER_SIZE];  /* buffer for line-wise reading from file */
union {
  double d;
  int i;
} id;          /* ID of point */
double x, y;    /* coordinates of point */
char * str;    /* tmp variable needed for assertions */
char * dstr;    /* tmp variable needed to find out substrings */
int rec = 0;    /* Counter for records */

DEBUG_OUT("Begin Generate Points\n");

  while (getStripLine(linebuf, sizeof(linebuf), fp) >0 ) {
DEBUG_OUT("getStripLine\n");
DEBUG_OUT2("Detoken0 %d \"%s\"\n",strlen(linebuf),linebuf);
    if (CASE_INSENSITIVE_STR_CMP(linebuf, "end") == 0) {
      DEBUG_OUT("'end' detected\n");
      break;
    }
    if ((str = strtok(linebuf, " ,")) == NULL) {
DEBUG_OUT2("Detoken1 %d \"%s\"\n",strlen(linebuf),linebuf);
      fprintf(stderr, "format error in line %d\n", rec + 1);
      exit(ERR_FORMAT);
    }
    if (fType == FTDouble) {
      id.d = atof((const char *)str);
    } else if (fType == FTInteger) {
      id.i = atoi((const char *)str);
    }

    if ((str = strtok(NULL, " ,")) == NULL) {
DEBUG_OUT2("Detoken2 %d \"%s\"\n",strlen(linebuf),linebuf);
      fprintf(stderr, "format error in line %d\n", rec + 1);
      exit(ERR_FORMAT);
    }
    dstr = (char *)strchr((const char *)str, (char)'D');
    if (dstr) *dstr = 'E';
    x = atof((const char *)str);

    if ((str = strtok(NULL, " ,")) == NULL) {
DEBUG_OUT2("Detoken3 %d \"%s\"\n",strlen(linebuf),linebuf);
      fprintf(stderr, "format error in line %d\n", rec + 1);
      exit(ERR_FORMAT);
    }
    dstr = (char *)strchr((const char *)str, (char)'D');
    if (dstr) *dstr = 'E';
    y = atof((const char *)str);
    if (fType == FTDouble) {
      DEBUG_OUT3("id=%f, x=%f, y=%f\n", id.d, x, y);

      WriteDoubleDbf(hDBF, rec, id.d);
    } else if (fType == FTInteger) {
      DEBUG_OUT3("id=%d, x=%f, y=%f\n", id.i, x, y);

      WriteIntegerDbf(hDBF, rec, id.i);
    }
    WritePoint(hSHP, rec, x, y);
    rec ++;
  }
}

/* read from fp and generate line/arc shapefile to hDBF/hSHP */
static void GenerateLines (  FILE *fp,
        DBFHandle hDBF,
        SHPHandle hSHP ) {
  char linebuf[STR_BUFFER_SIZE];  /* buffer for line-wise reading from file */
union {
  double d;
  int i;
} id;
   
double  * x = NULL, * y = NULL;  /* coordinates arrays */
int vector_size = 0;  /* current size of the vectors x and y */
char * str;    /* tmp variable needed for assertions */
char * dstr;    /* tmp variable needed to find out substrings */
int rec = 0;    /* Counter for records */
int coord = 0;    /* Counter for coordinates */

  /* loop lines */
  while (getStripLine(linebuf, sizeof(linebuf), fp) >0 ) {
    if (CASE_INSENSITIVE_STR_CMP(linebuf, "end") == 0) {
      DEBUG_OUT("final 'end' detected\n");
      break;
    }

    /* IDs are in single lines */
      if (fType == FTDouble) {
        id.d = atof((const char *)linebuf);
      DEBUG_OUT1("id=%f\n", id.d);
      } else if (fType == FTInteger) {
      id.i = atoi((const char *)linebuf);
      DEBUG_OUT1("id=%d\n", id.i);
      }

    coord = 0;

    /* loop coordinates of line 'id' */
    while (getStripLine(linebuf, sizeof(linebuf), fp) >0 ) {
      if (CASE_INSENSITIVE_STR_CMP(linebuf, "end") == 0) {
        DEBUG_OUT("a lines 'end' detected\n");
        break;
      }

      /* allocate coordinate vectors if to small */
      if (vector_size <= coord) {
        vector_size += COORDS_BLOCKSIZE;
        x = realloc(x, vector_size * sizeof(double));
        y = realloc(y, vector_size * sizeof(double));
        if (x == NULL || y == NULL) {
          fprintf(stderr, "memory allocation failed\n");
          exit(ERR_ALLOC);
        }
      }

      if ((str = strtok(linebuf, " ,")) == NULL) {
           if (fType == FTDouble) {
          fprintf(stderr, "format error for line with "
          "id=%f\n", id.d);
           } else if (fType == FTInteger) {
             fprintf(stderr, "format error for line with "
          "id=%d\n", id.i);
           }
        exit(ERR_FORMAT);
      }
      dstr = (char *)strchr((const char *)str, (char)'D');
      if (dstr) *dstr = 'E';
      x[coord] = atof((const char *)str);

      if ((str = strtok(NULL, " ,")) == NULL) {
           if (fType == FTDouble) {
             fprintf(stderr, "format error for line with "
          "id=%f\n", id.d);
           } else if (fType == FTInteger) {
             fprintf(stderr, "format error for line with "
          "id=%d\n", id.i);
           }
        exit(ERR_FORMAT);
      }
      dstr = (char *)strchr((const char *)str, (char)'D');
      if (dstr) *dstr = 'E';
      y[coord] = atof((const char *)str);

      DEBUG_OUT2("x=%f, y=%f\n", x[coord], y[coord]);

      coord ++;
    }
      switch (fType) {
      case FTDouble:
      WriteDoubleDbf(hDBF, rec, id.d);
        break;
      case FTInteger:
        WriteIntegerDbf(hDBF, rec, id.i);
        break;
      default:
        UnxpTyp(fType,__LINE__);
      }
    WriteLine(hSHP, rec, coord, x, y);
    rec ++;
  }

  free(x);
  free(y);
}                                                             

static int GeneratePolygons (  FILE *fp,
        DBFHandle hDBF,
        SHPHandle hSHP ) {
char linebuf[STR_BUFFER_SIZE];  /* buffer for line-wise reading from file */
char tmpbuf[STR_BUFFER_SIZE];  /* buffer for line-wise reading from file */
char *tmpptr1;
char *tmpptr2;
union {
  double d;
  int i;
} id;
double  * x = NULL, * y = NULL;  /* coordinates arrays */
int vector_size = 0;  /* current size of the vectors x and y */
int nparts = 0; /* number of parts */
int * partstarts = NULL; /* indices where new parts start in x[],y[] */
char * str;    /* tmp variable needed for assertions */
char * dstr;    /* tmp variable needed to find out substrings */
int rec = 0;    /* Counter for records */
int coord = 0;    /* Counter for coordinates */
  switch (fType) {
  case FTDouble:
    id.d = -1.0;
    break;
  case FTInteger:
    id.i = -1;
    break;
  default:
    UnxpTyp(fType,__LINE__);
  }

  while (getStripLine(tmpbuf, sizeof(tmpbuf), fp) >0 ) {
    if (CASE_INSENSITIVE_STR_CMP(tmpbuf, "end") == 0) {
      DEBUG_OUT("final 'end' detected\n");
      break;
    }
    tmpptr1=INDEX(tmpbuf,',');
    tmpptr2=RINDEX(tmpbuf,',');
    if (tmpptr1 == NULL) { /*No comma on line; presumed id*/
      DEBUG_OUT1("Solitary id.  coords=%d\n",coord);
      switch (fType) {
        case FTDouble:
          id.d = atof((const char *)tmpbuf);
          DEBUG_OUT1("id=%f\n", id.d);
          break;
        case FTInteger:
          id.i = atoi((const char *)tmpbuf);
          DEBUG_OUT1("id=%d\n", id.i);
          break;
        default:
          UnxpTyp(fType,__LINE__);
      }
      continue;
    } else if (tmpptr1 != tmpptr2) { /*More than one comma on line;Firct value id*/
      strcpy(linebuf,tmpptr1+1);
      switch (fType) {
        case FTDouble:
          id.d = atof((const char *)tmpbuf);
          DEBUG_OUT1("id=%f\n", id.d);
          break;
        case FTInteger:
          id.i = atoi((const char *)tmpbuf);
          DEBUG_OUT1("id=%d\n", id.i);
          break;
        default:
          UnxpTyp(fType,__LINE__);
      }
    } else {
      strcpy(linebuf,tmpbuf);
    }
    coord = 0;
    nparts = 0;
    while (CASE_INSENSITIVE_STR_CMP(linebuf,"end") != 0) {
      while (vector_size <= coord) { /* Extend vectors if needed*/
        vector_size += COORDS_BLOCKSIZE;
        x = realloc(x, vector_size * sizeof(double));
        y = realloc(y, vector_size * sizeof(double));
        if ((x==NULL) || (y == NULL)) {
          fprintf(stderr,"Memory allocation failed in GeneratePolygon\n");
          exit(0);
        }
      }
      if ((str = strtok(linebuf,",")) == NULL) {
        switch (fType) {
        case FTDouble:
          fprintf(stderr,"Format error for polynomial rec %d, id=%f\n",rec,id.d);
          break;
        case FTInteger:
          fprintf(stderr,"Format error for polynomial rec %d, id=%d\n",rec,id.i);
          break;
        default:
          UnxpTyp(fType,__LINE__);
        }
        DEBUG_OUT("format error\n");
      }
      dstr = (char *) strchr((const char *)str,(char)'D');
      if (dstr != NULL) *dstr = 'E';
      x[coord]=atof((const char *) str);
      if ((str = strtok(NULL,",")) == NULL) {
        switch (fType) {
        case FTDouble:
          fprintf(stderr,"Format error for polynomial rec %d, id=%f\n",rec,id.d);
          break;
        case FTInteger:
          fprintf(stderr,"Format error for polynomial rec %d, id=%d\n",rec,id.i);
          break;
        default:
          UnxpTyp(fType,__LINE__);
        }
        DEBUG_OUT("format error\n");
      }
      dstr = (char *) strchr((const char *)str,(char)'D');
      if (dstr != NULL) *dstr = 'E';
      y[coord]=atof((const char *) str);
      DEBUG_OUT2("vertex: x = %f, y=%f\n",x[coord],y[coord]);
      coord++;
      if (getStripLine(linebuf, sizeof(linebuf), fp) <0 ) break;
    }
    DEBUG_OUT("An end detected\n");
    WritePolygon(hSHP, rec, coord,x, y, nparts, partstarts);
    switch (fType) {
      case FTDouble:
        WriteDoubleDbf(hDBF, rec, id.d);
        break;
      case FTInteger:
        WriteIntegerDbf(hDBF, rec, id.i);
        break;
      default:
        UnxpTyp(fType,__LINE__);
    }
    rec++;
  }
  if (rec == 0) {
    printf("Warning: Shape file has no polygons!\n");
  }
  if(x != NULL) {free(x);x=NULL;}
  if(y != NULL) {free(y);y=NULL;}
  return 0;
}

#if 1==0
/* read from fp and generate polgon shapefile to hDBF/hSHP */
static int GeneratePolygonsOld (  FILE *fp,
        DBFHandle hDBF,
        SHPHandle hSHP ) {
int rc=0;
char linebuf[STR_BUFFER_SIZE];  /* buffer for line-wise reading from file */
char tmpbuf[STR_BUFFER_SIZE];  /* buffer for line-wise reading from file */
char *tmpptr1;
char *tmpptr2;
union {
  double d;
  int i;
} id;
int tokcnt;
double  * x = NULL, * y = NULL;  /* coordinates arrays */
int vector_size = 0;  /* current size of the vectors x and y */
int nparts = 0; /* number of parts */
int * partstarts = NULL; /* indices where new parts start in x[],y[] */
char * str;    /* tmp variable needed for assertions */
char * dstr;    /* tmp variable needed to find out substrings */
int rec = 0;    /* Counter for records */
int coord = 0;    /* Counter for coordinates */

  switch (fType) {
  case FTDouble:
    id.d = -1.0;
    break;
  case FTInteger:
    id.i = -1;
    break;
  default:
    UnxpTyp(fType,__LINE__);
  }
  /* loop polygons */
  while (getStripLine(tmpbuf, sizeof(tmpbuf), fp) >0 ) {
    if (CASE_INSENSITIVE_STR_CMP(tmpbuf, "end") == 0) {
      DEBUG_OUT("final 'end' detected\n");
      break;
    }

    tmpptr1=INDEX(tmpbuf,',');
    tmpptr2=RINDEX(tmpbuf,',');

    if(tmpptr1 != tmpptr2){ /*More than one comma; id presumed on line.*/
      tmpptr1++;
      strcpy(linebuf,tmpptr1);
      /* we assume we found an id */
      if (((fType==FTInteger) && (id.i != -1))||((fType==FTDouble) && (id.d != -1.))) {
        /* now its time to create the last read object */
        switch (fType) {
          case FTDouble:
            WriteDoubleDbf(hDBF, rec, id.d);
            break;
          case FTInteger:
            WriteIntegerDbf(hDBF, rec, id.i);
            break;
          default:
            UnxpTyp(fType,__LINE__);
        }
        if (partstarts != NULL) partstarts[0] = 0;
        WritePolygon(hSHP, rec, coord, x, y, (nparts > 0 ? nparts+1 : 0), partstarts);
        free(partstarts); partstarts = NULL;
        rec ++;
      }
      /* IDs are on a single line */
      tmpptr2=strtok(tmpbuf,",");
      switch (fType) {
        case FTDouble:
          id.d = atof((const char *)tmpptr2);
          coord = 0;
          nparts = 0;
          DEBUG_OUT1("id=%f\n", id.d);
          break;
        case FTInteger:
          id.i = atoi((const char *)tmpptr2);
          coord = 0;
          nparts = 0;
          DEBUG_OUT1("id=%d\n", id.i);
          break;
        default:
          UnxpTyp(fType,__LINE__);
      }
    } else { /* First comma on line same as last: coordinates only*/
      strcpy(linebuf,tmpbuf);
      if (coord == 0) { /*Case of coordinates without an id:*/
        DEBUG_OUT("no id for coordinates!");
        exit(ERR_FORMAT);
      }
      nparts ++;  /* a new part starts */
      partstarts = realloc(partstarts, sizeof(int) * (nparts+1));
      if (partstarts == NULL) {
        fprintf(stderr, "memory allocation failed\n");
        exit(ERR_ALLOC);
      }
      partstarts[nparts] = coord;
      DEBUG_OUT1("newpart at %d\n", coord);
    }

    /* the following block is just a copy from the while
     * construct below. Should find a more elegant solution!
     * ---------
     */
    /* allocate coordinate vectors if to small */
    if (vector_size <= coord) {
      vector_size += COORDS_BLOCKSIZE;
      x = realloc(x, vector_size * sizeof(double));
      y = realloc(y, vector_size * sizeof(double));
      if (x == NULL || y == NULL) {
        fprintf(stderr, "memory allocation failed\n");
        exit(ERR_ALLOC);
      }
    }
    if ((str = strtok(linebuf, " ,")) == NULL) {
      if (fType == FTDouble) {
        fprintf(stderr, "format error for polygon with "
          "id=%f\n:\t\"%s\"\n", id.d,linebuf);
      } else if (fType == FTInteger) {
        fprintf(stderr, "format error for polygon with "
          "id=%d\n", id.i);
      }
      exit(ERR_FORMAT);
    }
    dstr = (char *)strchr((const char *)str, (char)'D');
    if (dstr != NULL) *dstr = 'E';
      x[coord] = atof((const char *)str);
      if ((str = strtok(NULL, " ,")) == NULL) {
        switch (fType) {
          case FTDouble:
            fprintf(stderr, "format error AD for polygon with "
              "id=%f\n", id.d);
            break;
          case FTInteger:
            fprintf(stderr, "format error AI for polygon with "
             "id=%d\n", id.i);
            break;
          default:
            UnxpTyp(fType,__LINE__);
        }
        exit(ERR_FORMAT);
      }
      dstr = (char *)strchr((const char *)str, (char)'D');
      if (dstr) *dstr = 'E';
      y[coord] = atof((const char *)str);

      DEBUG_OUT2("x=%f, y=%f\n", x[coord], y[coord]);

      coord ++;
      /* ---------- end of copy */
    /*}*/

    /* loop coordinates of polygon 'id' */
      while (getStripLine(linebuf, sizeof(linebuf), fp) >0 ) {
        if (CASE_INSENSITIVE_STR_CMP(linebuf, "end") == 0) {
          DEBUG_OUT("an 'end' detected\n");
          break;
        }

      /* allocate coordinate vectors if to small */
        if (vector_size <= coord) {
          vector_size += COORDS_BLOCKSIZE;
          x = realloc(x, vector_size * sizeof(double));
          y = realloc(y, vector_size * sizeof(double));
          if (x == NULL || y == NULL) {
            fprintf(stderr, "memory allocation failed\n");
            exit(ERR_ALLOC);
          }
        }

        if ((str = strtok(linebuf, " ,")) == NULL) {
          switch (fType) {
            case FTDouble:
              fprintf(stderr, "format error BD for polygon with "
                "id=%f\n", id.d);
              break;
            case FTInteger:
              fprintf(stderr, "format error BI for polygon with "
               "id=%d\n", id.i);
              break;
            default:
              UnxpTyp(fType,__LINE__);
          }
          exit(ERR_FORMAT);
        }
        dstr = (char *)strchr((const char *)str, (char)'D');
        if (dstr != NULL) *dstr = 'E';
        x[coord] = atof((const char *)str);
        if ((str = strtok(NULL, " ,")) == NULL) {
          switch (fType) {
            case FTDouble:
              fprintf(stderr, "format error CD for polygon with "
                "id=%.1f\n", id.d);
              break;
            case FTInteger:
              fprintf(stderr, "format error CI for polygon with "
                "id=%d\n", id.i);
              break;
            default:
              UnxpTyp(fType,__LINE__);
          }
          exit(ERR_FORMAT);
        }
        dstr = (char *)strchr((const char *)str, (char)'D');
        if (dstr != NULL) *dstr = 'E';
        y[coord] = atof((const char *)str);
        DEBUG_OUT2("polygon: x=%f, y=%f\n", x[coord], y[coord]);
        coord ++;
      }
    }

  /* now its time to create the last object */
    switch (fType) {
      case FTDouble:
        WriteDoubleDbf(hDBF, rec, id.d);
        break;
      case FTInteger:
        WriteIntegerDbf(hDBF, rec, id.i);
        break;
      default:
        UnxpTyp(fType,__LINE__);
    }
    DEBUG_OUT("Exited from loop\n");
    if (partstarts != NULL) {
      partstarts[0] = 0;
      DEBUG_OUT("Exited from loop 2\n");
      WritePolygon(hSHP, rec, coord, x, y, (nparts > 0 ? nparts+1 : 0), partstarts);
      DEBUG_OUT2("Polygon Written: %d %d\n",rec,coord);

      free(partstarts);partstarts=NULL;
    } else {
      rc=2;
      printf("Warning - Output file has no polygons\n");
      DEBUG_OUT2("no polygons in file %d %d\n",rec,coord);
    }
    if(x != NULL) {free(x);x=NULL;}
    if(y != NULL) {free(y);y=NULL;}
    return rc;
}
#endif /*GeneratePolygonsOld*/

void Usage(char * progname) {
    print_version(stderr);
    fprintf(stderr, "ARL ascii2shp version " SUB(LVERSION) "\n\n");
    fprintf(stderr, "Usage: %s [options] outfile type < infile\n"
       "\treads stdin and creates outfile.shp, "
      "outfile.shx and outfile.dbf\n"
      "\ttype must be one of these: points lines polygons\n"
      "\tinfile must be in 'generate' format\n"
         "\t\tOptions:\n"
         "\t\t-i Place integer value id in .dbf file (default)\n"
         "\t\t-d Place double precision id in .dbf file\n",
         progname);
    exit(ERR_USAGE);
  }


int main(  int argc, char ** argv ) {
  DBFHandle hDBF;    /* handle for dBase file */
  SHPHandle hSHP;    /* handle for shape files .shx and .shp */
  int ObjectType = OBJECTTYPE_NONE;

extern int optind;
int rc=0;
int ch;

  while ((ch = getopt(argc, argv, "di")) != EOF) {
    switch (ch) {
    case 'd':
      fType = FTDouble;
      break;
    case 'i':
      fType = FTInteger;
      break;
    }
  }

  if (argc != optind+2) Usage(argv[0]);

  /* determine Object Type: */
  if (strcmp(argv[optind+1], "points") == 0) ObjectType = OBJECTTYPE_POINT;
  if (strcmp(argv[optind+1], "lines") == 0) ObjectType = OBJECTTYPE_LINE;
  if (strcmp(argv[optind+1], "polygons") == 0) ObjectType = OBJECTTYPE_POLYGON;
  if (ObjectType == OBJECTTYPE_NONE) {
    fprintf(stderr, "type '%s' unknown, use one of these: "
      "points lines polygons.", argv[optind+1]);
    exit(ERR_TYPE);
  }

  DEBUG_OUT1("outfile=%s\n", argv[optind]);
  DEBUG_OUT1("type=%s\n", argv[optind+1]);

  /* Open and prepare output files */
  hDBF = LaunchDbf(argv[optind]);
DEBUG_OUT("LaunchDbf\n");
  hSHP = LaunchShp(argv[optind], ObjectType);
DEBUG_OUT("LaunchShp\n");
  LaunchPrj(argv[optind]);
DEBUG_OUT("LaunchPrj\n");
  /* Call generate function */
  switch (ObjectType) {
    case OBJECTTYPE_POINT:
DEBUG_OUT("GeneratePoints\n");
      GeneratePoints(stdin, hDBF, hSHP);
DEBUG_OUT("End GeneratePoints\n");
      break;
    case OBJECTTYPE_LINE:
      GenerateLines(stdin, hDBF, hSHP);
      break;
    case OBJECTTYPE_POLYGON:
      rc = GeneratePolygons(stdin, hDBF, hSHP);
      break;
    default:
      fprintf(stderr, "internal error: "
        "unknown ObjectType=%d\n", ObjectType);
      exit(ERR_OBJECTTYPE);
  }

  /* Finish output files */
  DBFClose( hDBF );
  SHPClose( hSHP );

  /* success */
  exit(0);
}
