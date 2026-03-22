#include "config.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <shapefil.h>
#include <shapext.h>
  #ifdef MEMWATCH
    #include <memwatch.h>
  #endif
#define IGNORE_COLUMNS TRUE

/* # include "utils.h"*/
#include "vector2.h"

#define PROGNAME "shapesplit"
#define ERR_USAGE       1

#define MAJORV 1
#define MINORV 1
#define PATCHLEVEL 0
#define LVERSION MAJORV.MINORV.PATCHLEVEL
#define XSUB(V) #V
#define SUB(V) XSUB(V)


/* Object Type codes used in main(): */
#define	ERR_USAGE	1

char * pullWs (char * inString) {
/* Removes White Space from the string inString. */
int k=0,l=0;
   while (inString[l] != '\0') {
    if (strchr(" \t\r",inString[k])==NULL) {
      inString[l] = inString[k];
      l++;
    }
    k++;
  }
  return inString;
}

typedef struct {
char * frag;
fieldData * fD;
aShape * shape;
} fileOut;

static int verb_level;

static void Usage(char * progname) {
  fprintf(stderr, "ARL " PROGNAME " version " SUB(LVERSION) "\n\n");

  fprintf(stderr,"Usage: %s [options] shapeFile basename\n\n"
    "\twhere the shapefile will be split into shapefiles basename001\n"
    "\tbasename002, etc, all of which have matching field values in a\n"
    "\ta specified range.  Index file basename.idx is created to\n"
    "\trecord field values that match.\n\n",progname);
  fprintf(stderr,
    "\toptions: -v verbosity level.\n"
    "\t         -h print header line in basename.idx file\n"
    "\t         -m range collect objects whose fields are identical\n"
    "\t            in the given range.  range may be a field number,\n"
    "\t            or a range of field numbers joined by a hyphen '-'.\n\n");
  fprintf(stderr,
    "\te.g. A \"smoke\" file has 3 fields: an \"id\" field which increases\n"
    "\tsequentially, followed by a \"Start\" and \"End\" time field.  To\n"
    "\tsplit into shape files with common start and end times, use\n"
    "\t  %s -m 2-3  smokefilename basename\n"
    "\t to break into files as determined by the second and third fields.\n"
    ,progname);
  exit(-1);
}

#if IGNORE_COLUMNS
void parseColumns(char * spec,int * columns,int nColumns) {
char * token, * flag;
int begin,end,k;
  if (spec == NULL) {
    for (k=0;k<nColumns;k++) {
      columns[k] = TRUE;
    }
    return;
  } else {
    for (k=0;k<nColumns;k++) columns[k] = FALSE;
    for( token = strtok( spec," ,");token != NULL;token = strtok( NULL," ,")) {
      begin = 1;end = nColumns+1;
      if ((flag = strchr(token,'-')) == NULL) {
        sscanf(token,"%d",&end);
        if (end <= nColumns) {
          columns[end-1] = TRUE;
        }
      } else if (flag == token) {
        sscanf(token+1,"%d",&end);
        if (end > nColumns) end = nColumns;
        for (k=1;k<=end;k++) columns[k-1] = TRUE;
      } else {
        if (sscanf(token,"%d-%d",&begin,&end) == 2) {
          if (end > nColumns) end = nColumns;
          if (begin < 1) begin = 1;
          for (k=begin;k<= end;k++) columns[k-1]=TRUE;
        } else {
          if (begin < 1) begin = 1;
          end = nColumns;
          for (k=begin;k<= end;k++) columns[k-1]=TRUE;
        }
      }
DEBUG_OUT3("token %s, begin=%d, end=%d\n",token,begin,end);
    }
  }
#ifdef DEBUGGER
  for (k=0;k<nColumns;k++) {
    DEBUG_OUT2("Field %d is %d\n",k+1,columns[k]);
  }
#endif
}
#endif


int main(int argc, char ** argv ) {
aShape shapeFileIn;
fileOut * ofile=NULL;int kOFile,nOFiles=0;
FILE * indexfile;
SHPObject * obj;
char frag[25];
char buf[257];
int DO_Header = FALSE;
enum shapeFileType {SMOKE,HYSPLIT,FIRES,OTHER} thistype;
int kRecd,recno;
fieldData * mfield ;
#if IGNORE_COLUMNS
int * columns=NULL;
char * columnSelect=NULL;
#endif
extern int optind;
extern char *optarg;
int ch;
  verb_level=0;
#if IGNORE_COLUMNS
  while ((ch = getopt(argc, argv, "v:hm:")) != EOF) {
#else
  while ((ch = getopt(argc, argv, "v:h")) != EOF) {
#endif
    switch (ch) {
    case 'v' :
      if(sscanf(optarg,"%d",&verb_level) != 1) Usage(argv[0]);
      break;
    case 'h' :
      DO_Header = TRUE;
      break;
#if IGNORE_COLUMNS
    case 'm':
      if ( columnSelect == NULL) {
         columnSelect = (char *) malloc((strlen(optarg)+1)*sizeof(char));
        strcpy(columnSelect,optarg);
      } else {
        columnSelect=(char *)realloc(columnSelect,
                             (strlen(columnSelect)+strlen(optarg)+2)*sizeof(char));
        strcat(columnSelect,",");
        strcat(columnSelect,optarg);
      }
      /*parseColumns(optarg,&columns,&nColumns);*/
DEBUG_OUT1("Col Data %s\n",columnSelect);
      break;
#endif
    default:
      Usage(argv[0]);
    }
  }

  if (argc-optind < 2) Usage(argv[0]);

  #ifdef MEMWATCH
    mwInit();
  #endif

  ShapeOpen(&shapeFileIn,argv[optind]);

  mfield = newFieldData(&shapeFileIn);

  if (verb_level > 0) {
    printf( "Shapefile Type: %s   # of Shapes: %d\n\n",
           SHPTypeName( shapeFileIn.nShapeType ), shapeFileIn.nRecds );
    printf("Bounds: %f,%f to %f,%f\n",shapeFileIn.min.x,shapeFileIn.min.y,
              shapeFileIn.max.x,shapeFileIn.max.y);

    printf("Field count = %d, nEntities=%d\n",shapeFileIn.nFields,
              shapeFileIn.nRecds);

    printDbfHeader(& shapeFileIn);
  }
#if IGNORE_COLUMNS
   columns = (int *) malloc(shapeFileIn.nFields*sizeof(int)); 
      parseColumns(columnSelect,columns,shapeFileIn.nFields);
#endif
  thistype=OTHER;
  if (shapeFileIn.nFields == 3) {
    if ((strcmp(shapeFileIn.f[1].fieldName,"Start")==0) &&
              (strcmp(shapeFileIn.f[2].fieldName,"End")==0) ) {
      thistype = SMOKE;
    }
  } else if (shapeFileIn.nFields == 1) {
    if ((shapeFileIn.f[0].Type == FTDouble) &&
        (strcmp(shapeFileIn.f[0].fieldName,"id")==0) ) {
      thistype = HYSPLIT;
    }
  }
DEBUG_OUT("Spot 002\n");
#if 1==0
  if (thistype == OTHER) {
    ShapeClose(& shapeFileIn);
    fprintf(stderr,"Shape Type not valid\n");
    exit(-1);
  }
#endif
  strcpy(buf,argv[optind+1]);strcat(buf,".idx");
  indexfile = fopen(buf,"w");

  if (DO_Header) {
    fprintf(indexfile,"%10s,","FileName");
    fieldHeaderPrint(indexfile, & shapeFileIn);
  }
DEBUG_OUT("Spot 003\n");
  for (kRecd=0;kRecd<shapeFileIn.nRecds;kRecd++) {
/*Loop through records of infile ---------------------------------------------*/
DEBUG_OUT1("Record %d\n",kRecd);
    rdFieldData (& shapeFileIn, kRecd, mfield);
DEBUG_OUT("FieldData Read into mfield\n");
    for (kOFile=0;kOFile<nOFiles;kOFile++) {
      if (compareFieldData(& shapeFileIn, mfield, ofile[kOFile].fD,columns) != 0)
                                                                continue;
      break; /*On break, kOFile is the matching file number*/
    }
DEBUG_OUT2("kOFile=%d, nOFiles=%d\n",kOFile,nOFiles);
    if (kOFile == nOFiles) {
/*Need to create new output file ---------------------------------------------*/
      switch (thistype) {
        case SMOKE:
          strcpy(frag,DBFReadStringAttribute(shapeFileIn.hDBF,kRecd,1));
          strcat(frag,"-");
          strcat(frag,DBFReadStringAttribute(shapeFileIn.hDBF,kRecd,2));
          break;
        case HYSPLIT:
          strcpy(frag,DBFReadStringAttribute(shapeFileIn.hDBF,kRecd,0));
          break;
        case OTHER:
        case FIRES:
          sprintf(frag,"%03d",kOFile+1);
      }
DEBUG_OUT2("%d:%s\n",kRecd,frag);

      ofile = (fileOut *) realloc(ofile,(nOFiles+1)*sizeof(fileOut));
      
      nOFiles++;
      ofile[kOFile].frag = (char *) malloc((strlen(frag)+1)*sizeof(char));
      ofile[kOFile].fD = newFieldData(&shapeFileIn);
      copyFieldData(&shapeFileIn,  ofile[kOFile].fD, mfield) ;

      sprintf(buf,"%s%03d",argv[optind+1],kOFile+1);
      strcpy(ofile[kOFile].frag,frag);
      fprintf(indexfile,"%s,",buf);
      fieldDataPrint(indexfile, &shapeFileIn, ofile[kOFile].fD);
      /*printf("%s\n",buf);*/
      ofile[kOFile].shape = (aShape *) malloc(sizeof(aShape));
      newShapeFile( ofile[kOFile].shape,buf, shapeFileIn.nShapeType,
      shapeFileIn.nFields, shapeFileIn.f) ;
    }
    shapeGetObj(&shapeFileIn, kRecd,&obj);    
 
    if (checkObj(obj)!=0) {
      SHPDestroyObject( obj);obj = NULL;
      fprintf(stderr,"Object %d of input shapefile not valid.\n",kRecd);
      continue;
    }

    recno  = shapePutObj( ofile[kOFile].shape, obj);

    copyDbfRecord(& shapeFileIn, kRecd, ofile[kOFile].shape, recno, 0) ;

    SHPDestroyObject( obj);obj = NULL;
  }
  if (verb_level > 0) {
    for (kOFile = 0;kOFile<nOFiles;kOFile++) {
      fprintf(stdout,"%s\n",ofile[kOFile].frag);
    }
  }
/*free up allocated space*/
#if IGNORE_COLUMNS
  if (columns != NULL) {free(columns);columns = NULL;}
  if (columnSelect != NULL) {free(columnSelect);columnSelect = NULL;}
#endif

  for (kOFile = 0;kOFile<nOFiles;kOFile++) {
    free(ofile[kOFile].frag);ofile[kOFile].frag = NULL;
    ShapeClose( ofile[kOFile].shape);
    destroyFieldData(& shapeFileIn,ofile[kOFile].fD);
    ofile[kOFile].fD = NULL;
    free( ofile[kOFile].shape);
    ofile[kOFile].shape=NULL;
  }
DEBUG_OUT("Cleaned Ofiles\n");
  free(ofile);ofile=NULL;nOFiles=0;
  destroyFieldData(& shapeFileIn,mfield);
  ShapeClose(& shapeFileIn);
DEBUG_OUT("Cleaned shapeFileIn\n");

  #ifdef MEMWATCH
    mwTerm();
  #endif
  return 0;
}
