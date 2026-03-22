/* Contour.c
 */
#define PROGNAME "contour"
#define MAJORV 1
#define MINORV 1
#define PATCHLEVEL 12
#define LVERSION MAJORV.MINORV.PATCHLEVEL
#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
/*#include <sys/unistd.h>*/
#include <shapeContour.h>
void printChain(int ichain,FILE * file) ;

  #ifdef MEMWATCH
    #include <memwatch.h>
  #endif

#include "config.h"
#include "splitobj.h"

#include "shapefil.h"
#include "shapext.h"
#include "gridwork.h"

extern int verb_level;
int doSplit;

typedef struct {
  int inplay;
  int prev,next;
  double x,y;
} linkc;

typedef struct {
  int inplay;
  int npoints;
  int firstlink,lastlink;
} chain;


linkc * links = NULL; 
chain * chains = NULL;

int nlinks=0,nchains=0;
int nopen=0,nclosed=0;

void resetChains() {
int i;
  for (i=0;i<nlinks;i++) links[i].inplay=FALSE;
  for (i=0;i<nchains;i++) chains[i].inplay=FALSE;
  nopen=nclosed=0;
}

int getlink(double x,double y) {
int j;
  for (j=0; j<nlinks; j++) {
    if (! links[j].inplay) {
      links[j].inplay = TRUE;
      links[j].x = x;links[j].y=y;
      links[j].prev = links[j].next = -1;
      return j;
    }
  }
  links = (linkc *) realloc(links, (nlinks+10) * sizeof(linkc) );
  nlinks = nlinks+10;
  for (j=nlinks-10;j<nlinks;j++) links[j].inplay=FALSE;
/*  j=nlinks-10;*/
  links[nlinks-10].inplay = TRUE;
  links[nlinks-10].x = x;links[nlinks-10].y=y;
  links[nlinks-10].prev = links[nlinks-10].next = -1;
  return nlinks-10;
}

int getchain(int first,int last) {
int j;
  for (j=0;j<nchains;j++) {
    if (! chains[j].inplay) {
      chains[j].inplay = TRUE;
      chains[j].npoints = 2;
      chains[j].firstlink = links[last].prev = first;
      chains[j].lastlink = links[first].next = last;
      return j;
    }
  }
  chains = (chain *) realloc(chains, (nchains+10) * sizeof(chain) );
  for (j=nchains+9;j>=nchains;j--) {
    chains[j].inplay=FALSE;
  }
  chains[nchains].inplay = TRUE;
  chains[nchains].npoints = 2;
  chains[nchains].firstlink = links[last].prev = first;
  chains[nchains].lastlink = links[first].next = last;
  nchains = nchains+10;
  return nchains-10;
}

void dropChain (int j) {
int klink;
DEBUG_OUT("Dropping Chain\n");
  if (!chains[j].inplay) return;
  for (klink=chains[j].firstlink;klink>=0;klink=links[klink].next) {
    links[klink].inplay = FALSE;
  }
  chains[j].inplay = FALSE;
  if (chains[j].npoints < 0) nclosed --;
  if (chains[j].npoints > 0) nopen --;
}

int dewhiskerChain(int j) {
int first,last,mid,mid2;
#ifdef DEBUGGER
if (chains[j].npoints >-6) {
DEBUG_OUT1("Printing Chain %d --------------------------------------\n",j);
  printChain(j,stderr);
}
#endif
DEBUG_OUT2("Entering dewhisker: chain number = %d, npoints=%d\n",j,chains[j].npoints);
  if (chains[j].npoints>0) {
DEBUG_OUT1("Chain %d open\n",j);
    return -2;
  }
/*If begin of chain the tip of a whisker, remove.*/
  do {
    if (chains[j].npoints > -4) { /*A chain of three links or fewer is by def a whisker.*/
      dropChain(j);
      return -1;
    }
    first = chains[j].firstlink; last = chains[j].lastlink;
    mid = links[first].next; mid2 = links[last].prev;
    if ((links[mid].x == links[mid2].x) && (links[mid].y == links[mid2].y)) {
      links[first].inplay = links[last].inplay = FALSE;
      links[mid].prev = links[mid2].next = -1;
      chains[j].firstlink = mid; chains[j].lastlink = mid2;
      chains[j].npoints += 2;
DEBUG_OUT("Removing Whisker from endpoint\n");
    } else {
      break;
    }
  } while(TRUE);

  first = -1;
  do {
    if (first < 0) first = chains[j].firstlink;
    if ((mid = links[first].next) < 0) break;
    if ((last = links[mid].next) < 0) break;
    if ((links[first].x == links[last].x) && (links[first].y == links[last].y)) { 
      links[first].next = links[last].next;
      links[mid].inplay = links[last].inplay = FALSE;
      if ((mid = links[first].next) < 0) {
        chains[j].lastlink = first;
      } else {
        links[mid].prev = first;
      }
      chains[j].npoints += 2;
      first = links[first].prev;
DEBUG_OUT("Removing Whisker from midpoint\n");
      if (chains[j].npoints > -4) { /*A chain of three links or fewer is by def a whisker.*/
        dropChain(j);
        return -1;
      }
    } else {
      first = mid;
    }
  } while(TRUE);
DEBUG_OUT1("Leaving Dewhisker: %d\n",chains[j].npoints);
  return 0;
}

void earsAndWhiskers(void) {
int k1,k2;
  for (k2=0,k1=0;k2<nchains;k2++) {
    if (!chains[k2].inplay) continue;
    if (dewhiskerChain(k2) < 0) continue;
    chains[k2].inplay = FALSE;
    chains[k1] = chains[k2];
    chains[k1].inplay = TRUE;
    k1++;
  }
}

void printChain(int ichain,FILE * file) {
int first,current;
linkc * ll;
  if (! chains[ichain].inplay) {
    fprintf(file,"Chain %d not inplay\n",ichain );
    return;
  }
  fprintf(file,"Chain %d has %d points.\n",ichain,chains[ichain].npoints);
  if (verb_level > 0) {
    current=first=chains[ichain].firstlink;
    while (current >=0) {
      ll = links + current;
    fprintf(file,"Link %d: prev %d next %d, x %f, y %f\n", current, ll->prev,
        ll->next, ll->x, ll->y);
      current = ll -> next;
    }
  }
}

void freeChainLinks(void) {
  if (links != NULL) {
    free (links);
    nlinks = 0;
    links = NULL;
  }
  if (chains != NULL) {
    free(chains);
    nchains = 0;
    chains = NULL;
  }
  nopen = nclosed = 0;
}

const double mfactor=1.e6;

void addlink(double x0,double y0,double x1, double y1) {
int ibeg,iend;
int ichain;
int i;
linkc * l1, * l2;
  x0 = floor(mfactor*x0+0.5);x1 = floor(mfactor*x1+0.5);
  y0 = floor(mfactor*y0+0.5);y1 = floor(mfactor*y1+0.5);
  if ( (fabs(x0 - x1) == 0.) && (fabs(y0 - y1) == 0.0)) return;
  /* No zero-length Chains------*/
  ibeg = getlink(x0,y0);
  iend = getlink(x1,y1);
  ichain = getchain(ibeg,iend);
  nopen ++;
  l1 = &links[chains[ichain].firstlink];
  for (i=0; i<nchains ;i++) {
    if ( (! chains[i].inplay) || (i == ichain)) continue;
    if ( chains[i].npoints <= 0) continue;
    l2 = &links[chains[i].lastlink];
    if ((l1->x != l2->x)  || (l1->y != l2->y) ) continue;
    l2->next = l1->next;
    links[l1->next].prev = chains[i].lastlink;
    l1->inplay = FALSE;
    chains[ichain].firstlink = chains[i].firstlink;
    chains[ichain].npoints += chains[i].npoints -1;
    chains[i].inplay = FALSE;
    nopen --;
    break;
  }
  l1 = &links[chains[ichain].lastlink];
  for (i=0; i<nchains ;i++) {
    if ( (! chains[i].inplay) || (i == ichain)) continue;
    if ( chains[i].npoints <= 0) continue;
    l2 = &links[chains[i].firstlink];
    if ((l1->x != l2->x)  || (l1->y != l2->y) ) continue;
    l2->prev = l1->prev;
    links[l1->prev].next = chains[i].firstlink;
    l1->inplay = FALSE;
    chains[ichain].lastlink = chains[i].lastlink;
    chains[ichain].npoints += chains[i].npoints -1;
    chains[i].inplay = FALSE;
    nopen --;
    break;
  }
  if (chains[ichain].npoints < 3) return;
  l1 = &links[chains[ichain].firstlink];
  l2 = &links[chains[ichain].lastlink];
  if ( (l1->x == l2->x) && (l1->y == l2->y) ) {
    if (chains[ichain].npoints > 0) {
      chains[ichain].npoints = -chains[ichain].npoints;
      nclosed++;nopen--;
/*Recognize circular chains by setting point count negative*/
    }
  }
}

int mkContours(grid * g, double V) {
int ix,iy,jx,jy,idif,jdif;
int offset[][2] = {{0,0},{0,1},{1,1},{1,0},{0,0}};
int iside,ncount,kstp;
double vmin,vmax,vmean;
double V0,V1;
double x0,y0,x1,y1;
DEBUG_OUT3("Here A nx=%d,ny=%d, V=%g\n",g->nx,g->ny,V);
  for (ix=0;ix<g->nx-1;ix++) {
/*DEBUG_OUT1("ix:%d\n",ix);*/
    for (iy=0;iy<g->ny-1;iy++) {
/*DEBUG_OUT1("iy:%d\n",iy);*/
/* Begin analyzing current square*/
      for (jx=0,vmean=0.,vmax=vmin=GRID(g,ix,iy);jx<2;jx++) {
        for (jy=0;jy<2;jy++) {
          vmax = (vmax>GRID(g,ix+jx,iy+jy))?vmax:GRID(g,ix+jx,iy+jy);
          vmin = (vmin<GRID(g,ix+jx,iy+jy))?vmin:GRID(g,ix+jx,iy+jy);
          vmean += GRID(g,ix+jx,iy+jy);
        }
      }
      vmean /= 4.;
      if ((vmax < V) || (vmin > V)) continue;
      if (vmean > V) {
        for (iside = 0; iside < 4;iside++) {
          V0 = GRID(g,ix+offset[iside][0],iy+offset[iside][1]);
          V1 = GRID(g,ix+offset[iside+1][0],iy+offset[iside+1][1]);
          if ((V0 > V) && (V1 >= V) ) continue;
          if ((V0 >= V) && (V1 > V) ) continue;
          if ((V0 <= V) && (V1 <= V) ) {
            x0 = (offset[iside][0]*(V-vmean) + .5 * (V0-V))/(V0-vmean);
            y0 = (offset[iside][1]*(V-vmean) + .5 * (V0-V))/(V0-vmean);
            x1 = (offset[iside+1][0]*(V-vmean) + .5 * (V1-V))/(V1-vmean);
            y1 = (offset[iside+1][1]*(V-vmean) + .5 * (V1-V))/(V1-vmean);
          } else if (V1 > V) {
            x0 = (offset[iside][0]*(V-vmean) + .5 * (V0-V))/(V0-vmean);
            y0 = (offset[iside][1]*(V-vmean) + .5 * (V0-V))/(V0-vmean);
            x1 = ((double)offset[iside][0]*(V-V1) + (double)offset[iside+1][0]*(V0-V))/(V0-V1);
            y1 = ((double)offset[iside][1]*(V-V1) + (double)offset[iside+1][1]*(V0-V))/(V0-V1);
          } else {
            x0 = ((double)offset[iside][0]*(V-V1) + (double)offset[iside+1][0]*(V0-V))/(V0-V1);
            y0 = ((double)offset[iside][1]*(V-V1) + (double)offset[iside+1][1]*(V0-V))/(V0-V1);
            x1 = (offset[iside+1][0]*(V-vmean) + .5 * (V1-V))/(V1-vmean);
            y1 = (offset[iside+1][1]*(V-vmean) + .5 * (V1-V))/(V1-vmean);
          }
          addlink(ix + x0,iy + y0,ix + x1,iy + y1);
        }
      } else {
        for (iside = 0; iside < 4;iside++) {
          V0 = GRID(g,ix+offset[iside][0],iy+offset[iside][1]);
          V1 = GRID(g,ix+offset[iside+1][0],iy+offset[iside+1][1]);
          if ((V0 <= V) && (V1 <= V) ) continue;
          if ((V0 > V) && (V1 > V) ) {
            if ( V == vmean) continue;
            x0 = (offset[iside+1][0]*(V-vmean) + .5 * (V1-V))/(V1-vmean);
            y0 = (offset[iside+1][1]*(V-vmean) + .5 * (V1-V))/(V1-vmean);
            x1 = (offset[iside][0]*(V-vmean) + .5 * (V0-V))/(V0-vmean);
            y1 = (offset[iside][1]*(V-vmean) + .5 * (V0-V))/(V0-vmean);
          } else if (V1 <= V) {
            x0 = ((double)offset[iside][0]*(V-V1) + (double)offset[iside+1][0]*(V0-V))/(V0-V1);
            y0 = ((double)offset[iside][1]*(V-V1) + (double)offset[iside+1][1]*(V0-V))/(V0-V1);
            x1 = (offset[iside][0]*(V-vmean) + .5 * (V0-V))/(V0-vmean);
            y1 = (offset[iside][1]*(V-vmean) + .5 * (V0-V))/(V0-vmean);
          } else {
            x0 = (offset[iside+1][0]*(V-vmean) + .5 * (V1-V))/(V1-vmean);
            y0 = (offset[iside+1][1]*(V-vmean) + .5 * (V1-V))/(V1-vmean);
            x1 = ((double)offset[iside][0]*(V-V1) + (double)offset[iside+1][0]*(V0-V))/(V0-V1);
            y1 = ((double)offset[iside][1]*(V-V1) + (double)offset[iside+1][1]*(V0-V))/(V0-V1);
          }
          addlink(ix + x0,iy + y0,ix + x1,iy + y1);
        }
      }
/*End analysis of square */
    }
  }
/*Gone through all squares.  Need to check borders.*/
  ix=jx=0;
  for (iside =0;iside <4;iside++) {
    idif=offset[iside+1][0] - offset[iside][0];
    jdif=offset[iside+1][1] - offset[iside][1];
    ncount=(idif==0)?(g->ny-1):(g->nx-1);
    for (kstp = 0;kstp<ncount;kstp++,ix += idif,jx += jdif) {
      V0 = GRID(g,ix,jx);V1=GRID(g,ix+idif,jx+jdif);
      if ((V0 >= V) && (V1 >= V)) {
        if ((V0 == V) && (V1 == V)) {
          if ((GRID(g,ix+jdif,jx-idif)+GRID(g,ix+idif+jdif,jx+jdif-idif)
             -V -V) <0) continue;
        }
        addlink(ix,jx,ix+idif,jx+jdif);
        continue;
      }
      if ((V0 >= V) && (V1 < V) ) {
        addlink(ix,jx,ix + idif*(V0-V)/(V0-V1), jx + jdif*(V0-V)/(V0-V1));
      }
      if ((V0 < V) && (V1 >= V) ) {
        addlink(ix + idif*(V0-V)/(V0-V1),jx + jdif*(V0-V)/(V0-V1),
          ix + idif, jx + jdif);
      }
    }
  }
/*Done with borders.*/
DEBUG_OUT2("Before earsAndWhiskers: Chains: Open: %d, Closed %d\n",nopen,nclosed);
  earsAndWhiskers();
DEBUG_OUT2("After earsAndWhiskers: Chains: Open: %d, Closed %d\n",nopen,nclosed);
  return nclosed;
}

void writeDBFRecord(aShape * outfile,int recno,fieldData * fd) {
int k;
  for (k=0;k<outfile->nFields;k++) {
DEBUG_OUT2("Writing field %d of %d\n",k,outfile->nFields);
    switch (outfile->f[k].Type) {
      case FTDouble:
        DBFWriteDoubleAttribute( outfile->hDBF, recno, k, fd[k].value.d);
      break;
      case FTInteger:
        DBFWriteIntegerAttribute( outfile->hDBF, recno, k, fd[k].value.i);
      break;
      case FTString:
        DBFWriteStringAttribute( outfile->hDBF, recno, k, fd[k].value.s);
      break;
      case FTLogical:
        DBFWriteIntegerAttribute( outfile->hDBF, recno, k, fd[k].value.i);
      break;
      case FTInvalid:
        DBFWriteIntegerAttribute( outfile->hDBF, recno, k, 0);
      break;
    }
  }
}

double * pdfX=NULL, * pdfY=NULL; int nPdf = 0;
int * panPartStart=NULL, nparts=0;

int shapeContours(SHPObject ** objOut) {
int chainNum,partNo=0;
chain * ch;
linkc * lk;
int nvertices=0,vtx=0;
  panPartStart = (int *) realloc(panPartStart, nchains*sizeof(int));
  panPartStart[partNo] = 0;
  for (chainNum = 0;chainNum<nchains;chainNum++) {
    ch = chains + chainNum;
    if ( ! ch->inplay) continue;
DEBUG_OUT2("ShapeContours: Chains Open %d Closed %d\n",nopen,nclosed);
    if (ch->npoints >= 0) {
      if (ch->npoints < 4) continue;
      fprintf(stderr,"Non-closed chain %d\n",chainNum);
      lk = links + ch->firstlink;
      fprintf(stderr,"FirstLink %.16f,%.16f\n",lk->x/mfactor,lk->y/mfactor);
      lk = links + ch->lastlink;
      fprintf(stderr,"LastLink %.16f,%.16f\n",lk->x/mfactor,lk->y/mfactor);
      fprintf(stderr,"%d Links this chain\n\n",ch->npoints);
      continue;
    }
    nvertices -= ch->npoints;
DEBUG_OUT1("Nvertices: %d\n",nvertices);
    if (nPdf < nvertices) {
      pdfX = (double *) realloc(pdfX, (nvertices+500)  * sizeof(double));
      pdfY = (double *) realloc(pdfY, (nvertices+500)  * sizeof(double));
      nPdf = nvertices+500;
    }
    partNo ++;
DEBUG_OUT1("PartNo %d\n",partNo);
    panPartStart[partNo] = panPartStart[partNo-1] - ch->npoints;
DEBUG_OUT2("Indices %d %d \n",panPartStart[partNo-1],panPartStart[partNo]);
    lk = links + ch->firstlink;
    pdfX[vtx]=lk->x/mfactor;
    pdfY[vtx]=lk->y/mfactor;
#ifdef DEBUGGER
if (verb_level>0) {
DEBUG_OUT3("vtx %d %f,%f\n",vtx,lk->x/mfactor,lk->y/mfactor);
}
#endif

    vtx++;
    while (lk->next >=0) {
      lk = links + lk->next;
      pdfX[vtx]=lk->x/mfactor;
      pdfY[vtx]=lk->y/mfactor;
#ifdef DEBUGGER
if (verb_level>0) {
DEBUG_OUT3("vtx %d %f,%f\n",vtx,lk->x/mfactor,lk->y/mfactor);
}
#endif
      vtx++;
    }
DEBUG_OUT("------------------------------------\n");
    
    if ( chains[chainNum].npoints > 0) {
      fprintf(stderr,"Chain %d not closed %d\n",chainNum,nchains);
    }
  }
DEBUG_OUT2("PartNo %d, nvertices %d\n",partNo,vtx);
DEBUG_OUT("Creating Object\n");
  *objOut = SHPCreateObject( SHPT_POLYGON, 0 /*id*/,
     partNo,panPartStart,NULL, vtx,pdfX,pdfY,NULL,NULL);

  free(panPartStart);panPartStart=NULL;
DEBUG_OUT("Done creating Object\n");
  return 0;
}


void gformObj(grid * g,SHPObject * obj) {
                         /*gformObj translates obj x-y values from grid to lat-lon*/
int k;
double xX, yY;
  for (k=0;k < obj->nVertices ;k++) {
    (g->xy2XY)((g->params), obj->padfX[k], obj->padfY[k], &xX, &yY);
    obj->padfX[k] = xX;
    obj->padfY[k] = yY;
  }
  SHPComputeExtents( obj );
}


int objContour(aShape * shapeFileOut,grid * g,double V,fieldData * fd) {
SHPObject * objHold;
SHPObject ** objOut;
int nObj;
int m,recno,ichain;

  resetChains();
  if (mkContours(g, V) <= 0) return 0;
  shapeContours (& objHold );
  gformObj(g, objHold ); /*gformObj translates objHold x-y values from grid to lat-lon*/
  if (doSplit) {
    objOut = (SHPObject **) malloc(objHold->nParts * sizeof( SHPObject *));
    nObj = splitObj(objHold,objOut);
    SHPDestroyObject( objHold);
DEBUG_OUT1("Nobjects: %d\n",nObj);
    for (m=0;m<nObj;m++) {
      recno =  shapePutObj( shapeFileOut,objOut[m]);
      SHPDestroyObject( objOut[m] );objOut[m] = NULL;
DEBUG_OUT2("writing record %d of %d\n",recno,nObj);
      writeDBFRecord(shapeFileOut, recno, fd) ;
DEBUG_OUT1("end writing record %d\n",recno);
    }
    free (objOut);
DEBUG_OUT1("Recno %d\n",recno);
  } else {
    nObj=1;
    recno =  shapePutObj( shapeFileOut,objHold);
    writeDBFRecord(shapeFileOut, recno, fd) ;
    SHPDestroyObject( objHold);
  }

  if (verb_level>3) {
    printf("nlinks:%d, nchains:%d,nopen:%d,nclosed:%d\n", nlinks,nchains, nopen,nclosed);
    for (ichain=0;ichain<nchains;ichain++) {
      printChain( ichain, stdout) ;
    }
  }

  return nObj;
}

void freePdfXY(void) {
  free(pdfX);pdfX=NULL;
  free(pdfY);pdfY=NULL;nPdf=0;
}
