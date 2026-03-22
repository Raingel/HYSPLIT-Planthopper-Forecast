#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>
#include <time.h>
#include <getopt.h>
#define PROGNAME "shapeHysplitOut"
#include "config.h"
#include <unformio.h>
#include "shapefil.h"
#include "shapext.h"
#include "gridwork.h"
#include <shapeContour.h>
  #ifdef MEMWATCH
    #include <memwatch.h>
  #endif

int doSplit = TRUE;

int verb_level=0;
int strcnt(char * string,int pchar) ;

typedef struct {
  time_t begin,end;
} timeslot;

typedef struct {
int byr,bmo,bda,bhr;
double blat,blon,bheight;
} startLocRcd;

typedef struct {
int begin,end;
} intranges;

static grid g;

/* unfRecord record = {0,0,0,NULL};*/

void parseIntRanges(char * list,int * count, intranges ** t) {
int nc;
int k;
char * c,*flag;
  if (list == NULL) return;
  nc=(strcnt(list,',')+1);
  * count += nc;
  *t = (intranges *) realloc((*t),(* count)*sizeof(intranges));
  for (k=0,c=strtok(list,", ");c !=NULL ;c=strtok(NULL,", "),k++) {
    if ((flag = strchr(c,'-')) == NULL) { 
      sscanf(c,"%d",&(*t)[k].begin);
      (*t)[k].end = (*t)[k].begin;
    } else if (flag == c) {
      sscanf(c+1,"%d",&(*t)[k].end);
      (*t)[k].begin = -1;
    } else {
      if (sscanf(c,"%d-%d",&(*t)[k].begin,&(*t)[k].end) < 2) {
        (*t)[k].end = -1;
      } 
    }   
DEBUG_OUT3("c %s, begin=%d, end=%d\n",c,(*t)[k].begin,(*t)[k].end);
  }     
}

int strcnt(char * string,int pchar) {
char * ptr;
int retval;
  for (retval=0,ptr=string;ptr != NULL;) {
    ptr = strchr(ptr,pchar);
    if (ptr != NULL) {
      retval++;
      ptr++;
    }
  }
  return retval;
}

void rdTimes(struct tm * base_t,char * list,int * ntimes,timeslot ** t) {
intranges * tt=NULL;
int k,ntranges=0;
struct tm begin_t,end_t;
time_t base,begin,end;
  begin_t = * base_t;
  base = mktime(& begin_t);
DEBUG_OUT1("in rdTimes. list = %s\n",list);
  parseIntRanges(list, &ntranges, & tt) ;
  (*t) = (timeslot *) malloc(ntranges * sizeof(timeslot));
  *ntimes = ntranges;
DEBUG_OUT1("ntranges = %d\n",ntranges);
  for (k=0;k<ntranges;k++) {
    begin_t = * base_t;
    if ( tt[k].begin >= 0 ) {
      begin_t.tm_mday = tt[k].begin / 100;
      begin_t.tm_hour = tt[k].begin % 100;
      begin_t.tm_isdst = -1;
    }
    if (difftime((begin = mktime(& begin_t)),base)<0) {
      begin_t.tm_mon ++;
      begin = mktime(& begin_t);
    }
    (*t)[k].begin = begin;
    end_t = * base_t;
    if ( tt[k].end >= 0 ) {
      end_t.tm_mday = tt[k].end / 100;
      end_t.tm_hour = tt[k].end % 100;
      end_t.tm_isdst = -1;
    } else {
      end_t.tm_mon ++;
      end_t.tm_isdst = -1;
    }
    if (difftime((end = mktime(& end_t)),base)<0) {
      end_t.tm_mon ++;
      end = mktime(& end_t);
    }
    (*t)[k].end = end;
DEBUG_OUT3("Time %d, %s to %s\n",k,asctime(localtime(&((*t)[k].begin))),asctime(localtime(&(*t)[k].end)));
  }
  if (tt != NULL) {
    free(tt);tt=NULL;
  }
}

void rdIntRanges(char * list,int * ntimes,intranges ** t) {
int k;
  parseIntRanges(list, ntimes, t) ;
  for (k=0;k < *ntimes;k++) {
    if ((*t)[k].begin < 0) (*t)[k].begin = 0;
    if ((*t)[k].end < 0) (*t)[k].end = INT_MAX ;
  }
}


typedef struct {
double olat,dlat;
double olon,dlon;
} llgrid;


static void ij2XY(void * a,double xi,double yj,double * xlon,double * ylat) {
llgrid *p = (llgrid *) a;
  *xlon = p->olon + xi * p->dlon;
  *ylat = p->olat + yj * p->dlat;
}

void fillGridWValues(int doMissing,int offset,double * maxVal,double * minVal,
     int * nzCellCount,int * missVals) {
double Val,mnVal=2.,mxVal=0.;
int nzCellKount,knownVals;
int idx,jdx;
int nlon=g.nx,nlat=g.ny;
int noNZ;
/* For doMissing == TRUE, make Grid g = 2.0 except where values exist; there
 * grid values will be zero.
 * For doMissing == FALSE (standard), grid values are set to 0.0 except where
 * values can be scanned; there they will be set as given.
 */
/* Establish background grid: --------------------------------------------*/
DEBUG_OUT3("XXX doMissing=%d, nlat=%d,nlon=%d\n",doMissing,nlat,nlon);
  if (doMissing) {
    for (idx = 0; idx < nlat;idx ++) {
      for (jdx = 0; jdx < nlon; jdx++) {
        GRID(&g,idx,jdx) = 2.0;
      }
    }
  } else {
    for (idx = 0; idx < nlat;idx ++) {
      for (jdx = 0; jdx < nlon; jdx++) {
        GRID(&g,idx,jdx) = 0.0;
      }
    }
  }
DEBUG_OUT("YYY\n");
/* End set background grid ----------------------------------------------*/
/*Begin Fill Grid with values ------------------------------------------------*/
  for (nzCellKount=0,knownVals=0,noNZ=TRUE;offset >=0;knownVals++) {
    offset = unfscanf("s,s,f",offset,&idx,&jdx,&Val);

    if(idx>nlon || jdx>nlat) fprintf(stderr,"Error i=%d,j=%d\n",idx,jdx);
                                  fflush(stderr);
    if (verb_level>2) printf("i:%d, j:%d, Val:%lf\n",idx,jdx,Val);
    if (verb_level==2&&Val!=0.) printf("i:%d, j:%d, Val:%le\n",idx,jdx,Val);
      if (doMissing) {
        GRID(&g,idx-1,jdx-1) = 0.0;
      } else {
        GRID(&g,idx-1,jdx-1) = Val;
      }
    if (Val > 0.) {
      if (noNZ) {
        mxVal=mnVal=Val;
        noNZ=FALSE;
      }
      mnVal = (mnVal<Val)?mnVal:Val;
      mxVal = (mxVal>Val)?mxVal:Val;
      nzCellKount++;
    }
    *maxVal = mxVal;
    *minVal = mnVal;
  }
  * nzCellCount = nzCellKount;
  * missVals = (nlat * nlon) - knownVals;
  if (verb_level>0) printf("Missing Values: %d, Non-zero values:%d, max value:%g,"
                       " min value %g\n",* missVals, nzCellKount,mxVal,mnVal);
/*End Fill Grid with values ------------------------------------------------*/
}


void Usage(char * pgmName) {
  fprintf(stderr,
          "Usage: %s [options] infile [outShapefile]\n\n"
          "Options:\n"
          "\t-v value giving verbosity level of output (defauly 0)\n"
          "\t-u do not split contours into separate polygons.  Let all\n"
          "\t\tpolygons of contours at a given level be a single, multi-part\n"
          "\t\tpolygon object.  Default: split all non-contiguous polygons into\n"
          "\t\tunique objects.\n"
          "\t-s value Separation of grid contours, in case of uniform Separation\n"
          "\t\t\t(e.g. 10 gives contours for 230, 240, 250, ... degrees)\n"
          "\t-r rcd[-rcd][...] List of records and ranges of record numbers to\n"
          "\t\toutput shape files.  Thus, -r 25,30-40,50- will process the 25th\n"
          "\t\trecord, the 30th through 40th record, and record 50 on.\n"
          "\t-t ddhh[-ddhh][,...] List of times or ranges of times to output\n"
          "\t\tshape files.  Thus -t3120-3123,0110,0113 will output everything\n"
          "\t\tfrom 8PM to 11PM on the 31st, and 10AM and 1PM on the first.\n"
          "\t-f hour[-hour][,...] Forecast hours and ranges of forecast hours to\n"
          "\t\toutput shape files.  Thus -h 12,24-36 would output contour files\n"
          "\t\tfor forecast hours 12 and all hours between 24 and 36.\n"
          "\t-C contours Comma-separated list of contours desired (no internal\n"
          "\t\tspaces)\n"
          "\t\t\te.g. -C 2.67,2.73,3 produces contours at those values.\n\n"
          "\t-n num draw num equally spaced contours\n"
          "\t If both -C and -s are provided, both sets are drawn; if only one\n"
          "\t\t is provided, only that set is drawn.  If neither is provided,\n"
          "\t\t a default interval is selected.\n\n"
          "\t-z Produces contours surrounding missing values.\n\n"
          "\t outShapefile is the name of the output shape files (default shape).\n"
          "\t\tA separate shape file will be created for each grid in infile.\n"
          "\tthe shape name will have the valid date and time appended to\n"
          "\toutShapefile.\n"
                  ,pgmName);
  exit(1);
}

static int __compar(const void * a,const void * b) {
  const double *c = a;
  const double * d = b;
  if (*c > *d) return 1;
  if (*c < *d) return -1;
  return 0;
}

int main(int argc, char ** argv) {
char fInName[240]="";
char shapename[240]="",baseshapename[240]="";
fieldType t[]={
               {FTDouble,"Value",10,2},
               {FTInteger,"kexp",10,0},
               {FTString,"Type",10,0},
               {FTInteger,"Level",10,0}};
const int nFields=sizeof(t)/sizeof(t[0]);
fieldData fd[sizeof(t)/sizeof(t[0])];
FILE * infile;
aShape outfil;
int k,nObj;
int iyr,imo,ida,ifc,ihr,imn,nloc,cpck;

char * timeRanges = NULL,* hourRanges = NULL,* rcdRanges=NULL;
timeslot * slots=NULL;int ntimes=0;
intranges * fcstHours=NULL;int nFcstHours=0;
intranges * doRcd =NULL;int nDoRcd =0;
struct tm base;time_t base_t;
int do_process;
int krecd;
startLocRcd * startLocs;
char model[5],ptype[5],ident[5];
int nlat,nlon,nlvl,ntyp,kntr,height;
double dlat,dlon,clat,clon;
llgrid params;
int offset;
int idx,jdx,nzCellCount,missVals;
double Val,minVal,maxVal,mFactor;
int kexpFactor;
char * Contours=NULL;
double * mandLevels=NULL;
int nMandLevels=0;

int numCons=0; /*Number of contours to fit between max and min Val*/
int doMissing=FALSE;
double sepval=0.;
double * levels = NULL;
int nLevs,nLevsAv;

extern int optind;
extern char *optarg;
int ch;
  #ifdef MEMWATCH
    mwInit();
  #endif
/*--------- getopt loop ------------------------------------*/
  while ((ch = getopt(argc, argv, "v:s:c:C:n:N:t:T:f:F:R:r:hHuUzZ")) != EOF) {
    switch (ch) {
    case 'c':case 'C':
      for (Contours = strtok(optarg,", ");Contours != NULL;
                               Contours = strtok(NULL,", ")) {
        mandLevels = (double *) realloc (mandLevels,
                                (nMandLevels+1) * sizeof(double));
        sscanf(Contours,"%lf",&mandLevels[nMandLevels]);
        nMandLevels++;
      }
      break;
    case 'n': case 'N':
      if(sscanf(optarg,"%d",&numCons) != 1) Usage(argv[0]);
      break;
    case 'r': case 'R':
DEBUG_OUT1("R optarg %s\n",optarg);
      if (  rcdRanges == NULL) {
        rcdRanges = (char *) malloc((strlen(optarg)+1)*sizeof(char));
        strcpy(rcdRanges, optarg);
      } else {
        rcdRanges = (char *)realloc(rcdRanges,
                             (strlen(rcdRanges)+strlen(optarg)+2)*sizeof(char));
        strcat(rcdRanges,",");
        strcat(rcdRanges,optarg);
      }
      break;
    case 'f': case 'F':
DEBUG_OUT1("F optarg %s\n",optarg);
      if ( hourRanges == NULL) {
        hourRanges = (char *) malloc((strlen(optarg)+1)*sizeof(char));
        strcpy(hourRanges,optarg);
      } else {
        hourRanges = (char *)realloc(hourRanges,
                             (strlen(hourRanges)+strlen(optarg)+2)*sizeof(char));
        strcat(hourRanges,",");
        strcat(hourRanges,optarg);
      }
      break;
    case 't': case 'T':
DEBUG_OUT1("T optarg %s\n",optarg);
      if ( timeRanges == NULL) {
        timeRanges = (char *) malloc((strlen(optarg)+1)*sizeof(char));
        strcpy(timeRanges,optarg);
      } else {
        timeRanges = (char *)realloc(timeRanges,
                             (strlen(timeRanges)+strlen(optarg)+2)*sizeof(char));
        strcat(timeRanges,",");
        strcat(timeRanges,optarg);
      }
DEBUG_OUT3("ntimes = %d, slots = %x, line=%d\n",ntimes,slots,__LINE__);
      break;
    case 's': case 'S':
DEBUG_OUT1("S optarg %s\n",optarg);
      if (sscanf(optarg,"%lf",&sepval) == 0) Usage(argv[0]);
DEBUG_OUT2("sepval = %f, optarg = %s\n",sepval,optarg);
      break;
    case 'v' :
      if(sscanf(optarg,"%d",&verb_level) != 1) Usage(argv[0]);
      if (verb_level>0) printf("Verb level = %d\n",verb_level);
      break;
    case 'u' :case 'U':
      doSplit = FALSE;
      break;
    case 'z':case 'Z':
      doMissing = TRUE;
      break;
    case 'h': case 'H': default:
      Usage(argv[0]);
    }
  }
/*--------- end getopt loop: get standard parameters ---------------*/
DEBUG_OUT("Cmd Line Read\n");
  switch (argc-optind) {
  case 2:
    strcpy(baseshapename,argv[optind+1]);
  case 1:
    strcpy(fInName,argv[optind]);
    break;
  default:
    Usage(argv[0]);
  }

  if (nMandLevels > 0) {
    levels = (double *) malloc(nMandLevels * sizeof(double));
    nLevsAv = nMandLevels;
      if (verb_level > 0) {
      printf("Specified contour levels:\n");
      for (k=0;k<nMandLevels;k++) {
        printf("\t%g\n",mandLevels[k]);
      }
    }
  }

/*Open input file and deal with header records --------------------------------*/
  if ((infile = fopen(fInName,"r")) == NULL) {
    fprintf(stderr,"Unable to open file %s.\n",fInName);fflush(stderr);
    exit (0);
  }

  if (readUnfRecord (infile)<0) {
   fprintf(stderr,"Not valid file\n");fflush(stderr);
   exit(0);
  }
  /*if (verb_level>0) printf(" %4s: ",record.data);*/
  
  unfscanf("c4,i,i,i,i,i,i,i",0,
                     model,&iyr,&imo,&ida,&ihr,&ifc,&nloc,&cpck);
  if (verb_level>0) printf("model:%s yr%d mo%d da%d hr%d fc%d nloc%d cpack%d\n",
                            model,iyr,imo,ida,ihr,ifc,nloc,cpck);

  base.tm_year = 100+iyr; base.tm_mon=imo-1;base.tm_mday=ida;base.tm_hour=ihr-1;
  base.tm_min = base.tm_sec = 0;
  base.tm_isdst = -1;
  base_t = mktime(& base);

  startLocs = (startLocRcd *) malloc(nloc * sizeof(startLocRcd));

  for (k=0;k<nloc;k++) {
    readUnfRecord (infile);
    unfscanf("i,i,i,i,f,f,f",0,
                      &startLocs[k].byr,&startLocs[k].bmo,&startLocs[k].bda,
                            &startLocs[k].bhr,&startLocs[k].blat,&startLocs[k].blon,
                            &startLocs[k].bheight);

    if (verb_level>0) printf("yr:%d mo:%d da:%d hr:%d olat:%f olon:%f olvl:%f\n",
                           startLocs[k].byr,startLocs[k].bmo,startLocs[k].bda,
                           startLocs[k].bhr,startLocs[k].blat,startLocs[k].blon,
                           startLocs[k].bheight);
  }

  readUnfRecord (infile);
  unfscanf("i,i,f,f,f,f",0,
                            &nlat,&nlon,&dlat,&dlon,&clat,&clon);

  if (verb_level>0) printf("nlat:%d nlon:%d dlat:%f dlon:%f clat:%f clon:%f\n",
                            nlat,nlon,dlat,dlon,clat,clon);

  readUnfRecord (infile);
  unfscanf("i,i",0, &nlvl,&height);
  if (verb_level>0) printf("nlvl:%d height:%d\n",nlvl,height);

  if (nlvl != 1) {
    fprintf(stderr,"Current code handles only one level, not %d\n",nlvl);
    exit(0);
  }

/* Set do_process filters from hourRanges, rcdRanges and timeRanges*/
  rdIntRanges(hourRanges, & nFcstHours, & fcstHours) ;
  rdIntRanges(rcdRanges, & nDoRcd, & doRcd) ;
  rdTimes(& base, timeRanges, & ntimes, & slots) ;
DEBUG_OUT3("hourRanges %d, rcdRanges %d, ntimes %d\n",nFcstHours,nDoRcd,ntimes);
#ifdef DEBUGGER
DEBUG_OUT1("forecast hour ranges number %d\n",nFcstHours);
{
 int k;
  for (k=0;k<nFcstHours;k++) {
DEBUG_OUT3("range # %d, begin %d, end %d\n",k, fcstHours[k].begin, fcstHours[k].end);
  }
}
#endif


/* End header records, Begin case records -------------------------------------------*/
DEBUG_OUT("Begin Case Records\n");
  readUnfRecord (infile);
  unfscanf("i,c4",0, &ntyp,ident);
  if (verb_level>0) printf("ntyp:%d ident:%s\n",ntyp,ident);

  if (ntyp != 1) {
    fprintf(stderr,"Current code handles only one pollutant type, not %d\n",ntyp);
    exit(0);
  }
/*Create grid from info in header records.*/
  params.olon=clon;params.dlon=dlon;params.olat=clat;params.dlat=dlat;
DEBUG_OUT2("creating grid, nlat=%d, nlon=%d\n\n",nlat,nlon);
  newGrid(&g,nlon,nlat,(void *) &params,&ij2XY);



/*Loop through remaining records in file--------------------------------------*/

  krecd = 0;
  while ( readUnfRecord(infile) > 0 ) {
    krecd++;
    unfscanf("i,i,i,i,i,i",0, &iyr,&imo,&ida,&ihr,&imn,&ifc);


    if (verb_level>0) printf("\nRecord %d: Sample start:yr:%d mo:%d da:%d hr:%d min:%d ifh:%d\n",
                             krecd,iyr,imo,ida,ihr,imn,ifc);
    base.tm_year = 100 + iyr; base.tm_mon=imo-1;base.tm_mday=ida;base.tm_hour=ihr;
    base.tm_min = base.tm_sec = 0;
    base.tm_isdst = -1;
    base_t = mktime(& base);

    readUnfRecord (infile);
    unfscanf("i,i,i,i,i,i",0, &iyr,&imo,&ida,&ihr,&imn,&ifc);

    if (verb_level>0) printf("Sample Stop:yr:%d mo:%d da:%d hr:%d min:%d ifh:%d\n",
                             iyr,imo,ida,ihr,imn,ifc);

    readUnfRecord (infile);
    offset = unfscanf("c4,i,i",0,ptype,&height,&kntr);
    if (verb_level>0) printf("Ptype:%s height:%d Kntr:%d\n",
                               ptype,height,kntr);fflush(stdout);

/*Determine whether to process this record -----------------------------------*/
    do_process=FALSE;
    if ((nDoRcd + nFcstHours + ntimes) == 0) {
      do_process = TRUE;
    } else {
      for (k=0;k<nDoRcd;k++) {
        do_process = do_process ||
                 ((krecd >= doRcd[k].begin) && (krecd <= doRcd[k].end));
      }

      for (k=0;k<nFcstHours;k++) {
        do_process = do_process ||
                 ((ifc >= fcstHours[k].begin) && (ifc <= fcstHours[k].end));
      }

      for (k=0;k<ntimes;k++) {
        do_process = do_process ||
                 ((difftime(base_t,slots[k].begin) >= 0) &&
                               (difftime(base_t,slots[k].end) <= 0));
      }
    }
DEBUG_OUT2("Do Process? %d, record %d\n",do_process,krecd);
    if (!do_process) continue;
DEBUG_OUT1("AAA doMissing=%d\n",doMissing);
/*Re-initialize grid to missing-----------------------------------------------*/
    if (doMissing) {
      if (strlen(baseshapename) == 0) strcpy(baseshapename,"cloud");
#if 1==0
      for (idx=0;idx<nlat;idx++) {
        for (jdx=0;jdx<nlon;jdx++) {
          GRID((&g),idx,jdx) = 2.;
        }
      }
      for (nzCellCount=0;offset >=0;) {
        offset = unfscanf("s,s,f",offset,&idx,&jdx,&Val);
        if(idx>nlon || jdx>nlat) fprintf(stderr,"Error i=%d,j=%d\n",
                                      idx,jdx);fflush(stderr);
        GRID(&g,idx-1,jdx-1) = 0.;
        nzCellCount++;
      }
      if (verb_level>0) printf("%d missing values\n", nzCellCount);
#else
DEBUG_OUT("CCC\n");
      fillGridWValues(doMissing,offset, &maxVal, &minVal, &nzCellCount, &missVals);
#endif
    } else {
      if (strlen(baseshapename) == 0) strcpy(baseshapename,"shape");
DEBUG_OUT("BBB\n");
      for (idx=0;idx<nlat;idx++) {
        for (jdx=0;jdx<nlon;jdx++) {
#if 1==0
          GRID((&g),idx,jdx) = -2.;
#else
          GRID((&g),idx,jdx) = 0.;
#endif
        }
      }

/*End Initialize Grid --------------------------------------------------------*/
#if 1==0
/*Begin Fill Grid with values ------------------------------------------------*/
      for (nzCellCount=0;offset >=0;) {
        offset = unfscanf("s,s,f",offset,&idx,&jdx,&Val);

        if(idx>nlon || jdx>nlat) fprintf(stderr,"Error i=%d,j=%d\n",idx,jdx);
                                      fflush(stderr);
        if (verb_level>2) printf("i:%d, j:%d, Val:%lf\n",idx,jdx,Val);
        if (verb_level==2&&Val!=0.) printf("i:%d, j:%d, Val:%le\n",idx,jdx,Val);
          GRID(&g,idx-1,jdx-1) = Val;
        if (Val > 0.) {
          if (nzCellCount == 0) maxVal=minVal=Val;
          minVal = (minVal<Val)?minVal:Val;
          maxVal = (maxVal>Val)?maxVal:Val;
          nzCellCount++;
        }
      }
      if (verb_level>0) printf("Non-zero values:%d, max value:%g,"
                           " min value %g\n", nzCellCount,maxVal,minVal);
/*End Fill Grid with values ------------------------------------------------*/
#else
DEBUG_OUT("DDD\n");
      fillGridWValues(doMissing,offset, &maxVal, &minVal, &nzCellCount, &missVals);
DEBUG_OUT("EEE\n");
#endif
      {
      char buffer[12], * ptr;
        sprintf(buffer,"%12.6e",maxVal);
        ptr = strtok(buffer," eE");
        ptr = strtok(NULL," eE");
        sscanf(ptr,"%d",&kexpFactor);
        kexpFactor = (kexpFactor >= 0)?(3 * (kexpFactor/3)) :
                                       (-3*((2-kexpFactor)/3)) ;
        mFactor = pow(10.,(double) kexpFactor);
      }
    }
    sprintf (shapename,"%s%04d%02d%02d%02d",baseshapename,
     ((iyr<50)?iyr+2000:iyr+1900),imo,ida,ihr);
    if (verb_level > 0) printf("New shapename %s -----------------------\n",
                                                             shapename);
    newShapeFile(&outfil, shapename, SHPT_POLYGON, nFields, t);

    if (doMissing) {
      fd[0].value.d = 0.;
      nObj = objContour( & outfil, &g, 1., fd);
DEBUG_OUT("Do Missing\n");
    } else {
/*Fill table of contour levels----------------------------------------------*/
    int nmin,nmax,k1;
/*First add mandatory (specified) levels, if any --------------------------*/
      nLevs = nMandLevels;
      for (k=0;k<nMandLevels;k++) {
        levels[k] = mandLevels[k];
      }
/* Then add all multiples of sepval ----------------------------------------*/
DEBUG_OUT1("Sepval = %lg\n",sepval);
      if (sepval > 0.) {
        nmin = ceil(minVal/sepval);
        nmax = floor(maxVal/sepval);
DEBUG_OUT2("nmin %d, nmax %d\n",nmin,nmax);
     /*Adding nmax-nmin+1 values to table ----------------------------------*/
        if (nLevs + nmax - nmin + 1 > nLevsAv) {
          levels = (double *) realloc((void *)levels,
                             (nLevs + nmax - nmin + 1)*sizeof(double));
          nLevsAv = nLevs + nmax - nmin + 1;
        }
        for (k=nmin;k <= nmax;k++) {
          levels[nLevs + k - nmin] = k * sepval;
        }
      nLevs = nLevs + nmax - nmin + 1;
    }
DEBUG_OUT("In Contour Setting\n");
    if (verb_level > 0)for (k=0;k<nLevs;k++) {
       printf("Contour Level %d is %lg\n",k,levels[k]);
    } 
/*Evaluate separation that fits numCons contours between minVal and maxVal---*/
    if (numCons > 0) {
    int nmin,nmax;
    double dsep1=(maxVal-minVal)/numCons;
    double dsep2=(maxVal-minVal)/(numCons+1);
    char tval1[13],tval2[13];
DEBUG_OUT1("num cons = %d\n",numCons);
      if (verb_level>0) printf("nCons %d, dsep1 = %f, dsep2=%f\n",
                              numCons,dsep1,dsep2);
      sprintf(tval1,"%12le",dsep1);
      sprintf(tval2,"%12le",dsep2);
DEBUG_OUT2("tval1:%s, tval2:%s\n",tval1,tval2);
      if (tval1[0] != tval2[0]) {
        for (k=2;k<8;k++) tval1[k]='0';
      } else {
        for (k=2;k<8;k++) {
          if (tval1[k] != tval2[k]) break;
        }
        if (tval1[k]>='5' && tval2[k]<'5') tval1[k]='5';
        k++;
        for (;k<8;k++) {
          tval1[k] = '0';
        }
      }
      if (verb_level > 0) for (k=0;k<nLevs;k++) {
         printf("Contour Level %d of %d is %lf\n",k,nLevs,levels[k]);
      } 
      sscanf(tval1,"%lf",&dsep1);
DEBUG_OUT2("tval %s dsep1 %lf\n",tval1,dsep1);
      nmin = ceil(minVal/dsep1);
      nmax = floor(maxVal/dsep1);
      if (nLevsAv < nLevs + nmax - nmin + 1) {
        levels = (double *) realloc((void *)levels,
                                   (nLevs + nmax - nmin + 1)*sizeof(double) );
        nLevsAv = nLevs + nmax - nmin + 1;
      }
      for (k=nmin; k<=nmax; k++) {
        levels[nLevs + k - nmin] = k*dsep1;
      }
      nLevs = nLevs + nmax - nmin + 1;
    }
/*In default condition, select default contour levels ---------------------*/
    if (nLevs == 0) {
      levels = (double *) realloc((void *)levels, 3 * sizeof(double) );
      levels[0] = .01 * maxVal;
      levels[1] = .5 * (minVal+maxVal);
      levels[2] = .99 * maxVal;
      nLevs = 3;
    }
/*Sort the levels in increasing order ----------------------------------------*/
    {
      qsort(levels, nLevs, sizeof(double),__compar);
    }
    for (k=1,k1=1;k<nLevs;k++) {
      for (;k1<nLevs;k1++) {
        if (levels[k1] != levels[k-1]) break;
      }
      if (k1 >=nLevs) {
        break;
      }
      levels[k] = levels[k1];
    }
    nLevs = k;
    if (verb_level > 0)for (k=0;k<nLevs;k++) {
      printf("Contour Level %d of %d is %lg\n",k,nLevs,levels[k]);
    }
    for (k=0;k<nLevs;k++) {
    char buffer[20], *ptr;
      sprintf(buffer,"%12.6e",levels[k]);
      ptr = strtok(buffer,"eE");ptr = strtok(NULL,"Ee");
      fieldDataFill(&outfil, fd, levels[k]/mFactor,kexpFactor,ptype,height );
     /* fd[0].value.d = levels[k];*/
      nObj = objContour( & outfil, &g, levels[k], fd );
DEBUG_OUT2("lev # %d, level %g\n",k,levels[k]);
    }
  }
DEBUG_OUT("closing shape file\n");
  ShapeClose(& outfil);
DEBUG_OUT("Shapefile closed\n");
/*End Records loop ------------------------------------------------------*/
  } 

/*  if (levels != NULL) free(levels);
  if (mandLevels != NULL) free(mandLevels);*/
#define CLEANUP(array) if(array !=NULL) {free(array);array=NULL;}

  CLEANUP(levels);
  CLEANUP(mandLevels);
  CLEANUP(slots);ntimes=0;
  CLEANUP(doRcd);nDoRcd=0;
  CLEANUP(fcstHours);nFcstHours=0;
  CLEANUP(rcdRanges);
  CLEANUP(hourRanges);
  CLEANUP(timeRanges);

  freeGrid(&g);
  freeChainLinks();
  freePdfXY();

  free(startLocs);

  freeUnfRecord();

  fclose(infile);

  #ifdef MEMWATCH
    mwTerm();
  #endif
  return 0;
}
