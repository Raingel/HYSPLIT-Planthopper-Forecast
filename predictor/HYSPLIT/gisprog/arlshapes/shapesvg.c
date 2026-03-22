#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <getopt.h>
#include <shapefil.h>
#include "shapext.h"

#define PROGNAME "shapesvg"
#define ERR_USAGE       1

#ifndef  LVERSION
#define MAJORV 1
#define MINORV 1
#define PATCHLEVEL 2
#define LVERSION MAJORV.MINORV.PATCHLEVEL
#endif
#define XSUB(V) #V
#define SUB(V) XSUB(V)

#define LinesPerPage 35
double fpct=100.;
double opacity = 1.0;

void Usage(char * progname) {
  fprintf(stderr, "ARL " PROGNAME " version " SUB(LVERSION) "\n\n");
  fprintf(stderr,"Usage %s [options] file [... file] \n\n"
    "Where file [... file] represents a sequence of shape files.\n\n"
    "Options:\n"
    "\t-f svgfilename [Output File:Default a]\n"
    "\t-l [draw labels on output file]\n"
    "\t-s font size in percentage of default\n"
    "\t-o opacity; 0.0 to 1.0 (default 1.0)\n"
    "\t-r xmin,ymin,xmax,ymax [range of values on output map:Default -180,-90,180,90]\n"
    "\t-R refShapeFile : overrides -r and takes xmin, etc. from shape file refShapeFile\n"
    "\t-n [no labels on output file: Default]\n\n"
    "\t Note: input shape files can optionally be associated with labels,\n"
    "\tIdents and/or colors.\n"
    "\tE.g. \"filename %%l this is a label %%c red %%Ismoke\" will use\n"
    "\tthe label text following %%l and %%I for labels and idents, with\n"
    "\tthe color name following %%c for the file.\n"
    "\tSpaces but not the \"%%\" character are allowed in the label.\n"
    "\tColor names can include the #ffffff hex notation for rgb levels.\n"
    "\tImportant: Filenames and associated labels and colors must be grouped\n"
    "\ttogether using pairs of \" characters.\n\n"
    "\tProviding %%l labels overides the -n no label specification.\n"
    "\tUnder -l label condition, a %%l label value is used, if provided.\n"
    "\tOtherwise, the %%i ident value is used.  If neither a %%l or %%i\n"
    "\tvalue is provided, the filename is used.\n\n"
    ,progname);
    exit(1);
  }

struct {
double xmin,ymin,xmax,ymax;
} vBox = {-180.,-90.,180.,90.};

int xyrange(char * spec,double * xmin,double * ymin,double * xmax,double * ymax) {
double tmp;
char * arg;
  arg = strtok(spec,",");
  sscanf(arg,"%lf",xmin);
  arg = strtok(NULL,",");
  sscanf(arg,"%lf",ymin);
  arg = strtok(NULL,",");
  sscanf(arg,"%lf",xmax);
  arg = strtok(NULL,",");
  sscanf(arg,"%lf",ymax);
  if (* xmin > * xmax) {tmp=* xmin;* xmin=* xmax;* xmax=tmp;}
  if ( * ymin >  * ymax) {tmp= * ymin; * ymin= * ymax; * ymax=tmp;}
  return 0;
}


void copyshape(FILE * svgFile,aShape * shape,char * ident,char * color) {
SHPObject *psShape;
int jRecd,iPart,i;
DEBUG_OUT("entering copyshape\n");
  switch(shape->nShapeType) {
  case SHPT_POLYGON:case SHPT_ARC:
DEBUG_OUT("In switch Polygon\n");
    fprintf(svgFile,"<g id=\"%s\" ",ident);
    fprintf(svgFile,"transform=\"scale(%f,%f) translate(%f,%f)\" ",
             1000./(vBox.xmax - vBox.xmin),1000./(vBox.ymin - vBox.ymax),
             -vBox.xmin,-vBox.ymax);
    switch(shape->nShapeType) {
    case SHPT_POLYGON:
      fprintf(svgFile," stroke=\"none\" fill=\"%s\" fill-opacity=\"%.1f\" >\n"
                , color, opacity);
      break;
    case SHPT_ARC:
      fprintf(svgFile," fill=\"none\" stroke=\"%s\" stroke-width=\"%f\" >\n",
           color,(vBox.xmax-vBox.xmin)/1000.);
      break;
    }
    for (jRecd=0;jRecd<shape->nRecds;jRecd++) {
      shapeGetObj(shape, jRecd,&psShape);
      if (psShape->nParts > 0) {
        fprintf(svgFile,"<path d=\"");
        for (iPart=0;iPart<psShape->nParts;iPart++) {
        int first=psShape->panPartStart[iPart];
        int next=(iPart+1<psShape->nParts)?psShape->panPartStart[iPart+1]-1:
                                 psShape->nVertices;
          fprintf(svgFile,"M %.2f,%.2f L ",
                  psShape->padfX[first],psShape->padfY[first]);
          for(i=first+1;i<next;i++) {
            fprintf(svgFile,"%.2f,%.2f ",
                psShape->padfX[i],psShape->padfY[i]);
          }
/*Make sure sub polygons are closed.*/
          if (psShape->nSHPType == SHPT_POLYGON) { 
            fprintf(svgFile," Z "); 
          }
        }
        fprintf(svgFile,"\"/>\n");
      }
    }
    fprintf(svgFile,"</g>\n");
    break;
  case SHPT_POINT:
DEBUG_OUT2("In switch point ident %s color %s\n",ident,color);
DEBUG_OUT1("nshapes = %d\n",shape->nRecds);
    fprintf(svgFile,"<g id=\"%s\" ",ident);
    fprintf(svgFile,"transform=\"scale(%f,%f) translate(%f,%f)\" ",
             1000./(vBox.xmax - vBox.xmin),1000./(vBox.ymin - vBox.ymax),
             -vBox.xmin,-vBox.ymax);
    fprintf(svgFile," stroke=\"none\" fill=\"%s\" fill-opacity=\"%.1f\" >\n"
              , color, opacity);
    for (jRecd=0;jRecd<shape->nRecds;jRecd++) {
      shapeGetObj(shape, jRecd,&psShape);
      for (i=0;i<psShape->nVertices;i++) {
        fprintf(svgFile,"<circle cx=\"%f\" cy=\"%f\" r=\"%f\" />\n",
               psShape->padfX[i],psShape->padfY[i],20./(vBox.xmax - vBox.xmin));
      }
    }
    fprintf(svgFile,"</g>\n");
    break;
  default:
    break;
  }
}

void svgText(FILE * svgFile,double x,double y,char * text,char * color) {
  fprintf(svgFile,"<text  x=\"%.3f\" y=\"%.3f\" font-size=\"%.3f\" fill=\"%s\" >",
              x,y,10.*fpct/LinesPerPage,color);
  fprintf(svgFile,"%s",text);
  fprintf(svgFile,"</text>\n");
}

FILE * svgOpen(char * svgName,char * title){
FILE * result;
time_t clock;
struct tm atime;
char curtime[25];
  if (isalpha(svgName[0])) {
    char * name = malloc((strlen(svgName)+5) * sizeof(char));
    strcpy(name,svgName);
    strcat(name,".svg");
    result=fopen(name,"w");
    free(name);
  } else {
    result=stdout;
  }
  clock = time(NULL);
  atime = *localtime(&clock);
  
  fprintf(result,"<?xml version=\"1.0\" "\
     "standalone=\"no\" ?>\n");
  fprintf(result,
     "<svg version=\"1.1\" baseProfile=\"full\"\n"\
     "xmlns=\"http://www.w3.org/2000/svg\"\n"\
     "xmlns:xlink=\"http://www.w3.org/1999/xlink\"\n"\
     "xmlns:ev=\"http://www.w3.org/2001/xml-events\"\n");
  fprintf(result,"viewBox=\" 0 0 1000 1000 \"\n");
  fprintf(result,"preserveAspectRatio=\"none\">\n"\
     "<title>%s</title>\n",title);
  strftime(curtime,sizeof(curtime),"on %D at %T.",&atime);
  fprintf(result,"<desc>Shape presentation produced by ARL/NOAA"
     " %s.</desc>\n",curtime);
  return result;
}

int getSubOpt(char * args,char * optstring,char * flag,char ** psOpt) {
char * chk = strtok(NULL,flag);
char * index;
int k;
  *psOpt = NULL;
  if (chk == NULL) {
    return EOF;
  }
  index = strchr (optstring,*chk);/* first character a flag character?*/
  if (index == NULL) {
    return '?';
  }
  chk = chk + 1 + strspn(chk + 1," "); /* First Non-blank character in string*/
  for (k = strlen(chk);k>0;k--) {
    if (chk[k-1] != ' ') {
      chk[k]='\0';
      *psOpt = chk;
      return *index;
    }
  }
  return '?';
}


void svgClose(FILE * svgFile) {
   fprintf(svgFile,"</svg>\n");
   fclose(svgFile);
}

char * colors[] = {"red","blue","green","black"};
int thiscolor=0,ncolors=sizeof(colors)/sizeof(colors[0]);;

int main(int argc,char ** argv) {
aShape shapeFile;
FILE * svgFile;
int DO_LABEL=FALSE;
char ** idFile=NULL;
char * svgFileName=NULL;
char * refShape=NULL;
int linepos=0;
extern int optind;
extern char *optarg;
int ch,i,nFiles;

  #ifdef MEMWATCH
    mwInit();
  #endif

  while ((ch = getopt(argc, argv, "f:nlr:R:s:o:")) != EOF) {
    switch (ch) {
    case 'l':
      DO_LABEL=TRUE;
      break;
    case 'n':
      DO_LABEL=FALSE;
      break;
    case 'R':
      refShape=optarg;
      break;
    case 'r':
      xyrange(optarg , &vBox.xmin, &vBox.ymin, &vBox.xmax, &vBox.ymax) ;
      break;
    case 'f':
      svgFileName=optarg;
      break;
    case 's':
      fpct = atof(optarg);
      break;
    case 'o':
      opacity = atof(optarg);
      break;
    default:
      Usage(argv[0]);
    }
  }
  if ( (nFiles = (argc - optind) ) <= 0) Usage(argv[0]);

  if (refShape != NULL) {
    ShapeOpen(&shapeFile,refShape);
    vBox.xmin=shapeFile.min.x-1;vBox.xmax=shapeFile.max.x+1;
    vBox.ymin=shapeFile.min.y-1;vBox.ymax=shapeFile.max.y+1;
    ShapeClose(& shapeFile);
  }

  idFile = (char **) realloc(idFile,nFiles*sizeof(char *));
  idFile[nFiles-1] = NULL;
  if ( svgFileName == NULL)  svgFileName = "a";
    svgFile=svgOpen(svgFileName,"Testingsvg");
DEBUG_OUT1("Svg File Name: %s\n", svgFileName);
/*Main Loop*/
    for (i=optind;i<argc;i++) {
      char * idval, *lastchr;
      char * labval, * colorval ;
      int ch;
        idFile[i-optind] = strtok(argv[i]," %");
DEBUG_OUT1("Shape File Name: \"%s\"\n", idFile[i-optind]);
        idval = labval = colorval = NULL;

        while ((ch = getSubOpt(argv[i],"LlCcIi","%",&lastchr)) != EOF) {
          switch (ch) {
          case 'l': case 'L':
            labval = lastchr;
            break;
          case 'c': case 'C':
            colorval = lastchr;
            break;
          case 'i': case 'I':
            idval = lastchr;
            break;
          }
        }
        if (colorval == NULL) {
          thiscolor = (thiscolor < ncolors)?thiscolor:0;
          colorval = colors[thiscolor];
          thiscolor++;
        }
        if (idval == NULL) {
          for (idval = idFile[i-optind]+strlen(idFile[i-optind]);idval>idFile[i-optind];idval --) {
            if (*(idval-1) == '/') break;
          }
        }
        if (labval == NULL) {
          if (DO_LABEL) {
            labval = idval;
          } else {
            labval = "";
          }
        }
        DEBUG_OUT1("Label = %s\n",labval);
        DEBUG_OUT1("Color = %s\n",colorval);
        DEBUG_OUT1("Ident = %s\n",idval);
     if (ShapeOpen(&shapeFile,idFile[i-optind]) >= 0) {
       if (DO_LABEL || (strlen(labval)>0) ) {
         linepos++;
         svgText(svgFile, 0., ((double) linepos)*10.*fpct/LinesPerPage,labval,
                        colorval) ;
       }
       copyshape(svgFile,&shapeFile,idval,colorval) ;
     } else {
  DEBUG_OUT1("Shape file %s not opened\n",idFile[i-optind]);
     }
     ShapeClose(&shapeFile);
   }
   free (idFile);
   svgClose(svgFile);

  #ifdef MEMWATCH
    mwTerm();
  #endif
  return 0;
}
