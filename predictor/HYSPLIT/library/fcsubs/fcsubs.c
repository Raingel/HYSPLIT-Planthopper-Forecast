#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <string.h>
#define ERR -1
#include "fcsubs.h"


/*
void main(){
char check[20]="FCSUBS.bak     ";
INT4 twenty=20,fp;
printf("%ld\n",(fp=FCOPEN(check,&twenty)) );
printf("%ld\n",FCLEN(&fp));
}
*/

/*Local definition of tell, deprecated file positioning routine
 */

INT4 tell(INT4 filedes) {
  return lseek(filedes, 0L, SEEK_CUR);
}

#if defined(CRAY)
INT4 FCOPEN(_fcd path_p, _fcd access_p) {
  char * path = _fcdtocp(path_p);
  char * access = _fcdtocp(access_p);
  INT4 l_path = _fcdlen(path_p);
  INT4 l_access = _fcdlen(access_p);

#else
INT4 FCOPEN_F77(char * path,char * access,INT4 l_path, INT4 l_access){
#endif


#define PATHLEN 100
char opath[PATHLEN+1],*oblank,blank=' ';
int ptpos;
INT4 fp,length;
int accx;
mode_t perms=S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWGRP;
INT4 ln2;ln2 = l_path;
length = ln2 > PATHLEN ? PATHLEN : ln2;
strncpy(opath,path,length);
opath[length]=' ';
oblank=strchr(opath,blank);
oblank[0]='\0';
accx=0;
for (ptpos=0 ; ptpos < l_access ; ptpos++){
switch (access[ptpos]) {
  case 'r':case 'R': accx = accx | 1; break;
  case 'w':case 'W': accx = accx | 2; break;
  }
}
switch (accx) {
case 3:case 0: fp=open(opath,O_RDWR|O_CREAT,perms);break;
case 2: fp=open(opath,O_WRONLY|O_CREAT,perms);break;
case 1: fp=open(opath,O_RDONLY);break;
}
if (fp==ERR) {
  fprintf(stderr,"Cannot open File %s",opath);
  perror("");
}
return fp;
}

INT4 FCLEN_F77(INT4 *fpoint){
INT4 place,length;
place=tell(*fpoint);
if (place!=ERR){
  lseek( *fpoint,0L,SEEK_END);
  length=tell( *fpoint);
  lseek(*fpoint,place,SEEK_SET);
  return length;
  }
else {
  perror(" ");
  return place;
  }
}

INT4 FCGTPS_F77(INT4 *fpoint) {
  return tell( *fpoint);
}

INT4 FCPTPS_F77(INT4 *fpoint,INT4 * place) {
if (lseek( *fpoint,*place,SEEK_SET) <0) {
  perror("");
  return 1;
  }
else return 0;
}

INT4 FCREAD_F77(INT4 *fpoint,void *buf, \
       INT4 *flipsize,INT4 *count) {
INT4 n_bytes;
INT4 abscount,absflip;
int retcode;
absflip = (*flipsize < 0) ? -(*flipsize) : *flipsize;
abscount = (*count < 0 ) ? -(*count) : *count;
n_bytes = absflip * abscount;
n_bytes = (n_bytes>0)?n_bytes:0;
if ((retcode = read(*fpoint,buf,n_bytes)) != n_bytes) {
  if (retcode < 0) {
    perror("Read Error");
    }
  return 1;
  }
else
  {
    if (*flipsize < -1) {
      swap_c(buf,absflip,abscount);
      }
    return 0;
  }
}

INT4 FCCLOS_F77(INT4 *fpoint){
int ret;
ret=close(*fpoint);
if (ret==0) return 0;
perror(" ");
return 1;
}

INT4 FCWRIT_F77(INT4 *fpoint,void *buf, \
       INT4 *flipsize,INT4 *count) {
INT4 n_bytes;
int retcode;
INT4 abscount,absflip;
absflip = (*flipsize < 0) ? -(*flipsize) : *flipsize;
abscount = (*count < 0 ) ? -(*count) : *count;
n_bytes = absflip * abscount;
if (*flipsize < -1) {
  swap_c(buf,absflip,*count);
  }
retcode = write(*fpoint,buf,n_bytes);
if (retcode < 0) {
  perror("Error writing file");
  }
if (*flipsize < -1) {
  swap_c(buf,absflip,*count);
  }
if (retcode != n_bytes) return 1;
else return 0;
}

INT4 FCTRNC_F77(INT4 *fpoint,INT4 *length) {
INT4 setsize;
setsize = *length;
if (setsize < 0) {
  if ( (setsize = tell(*fpoint)) < 0) return 1;
  } 
#if defined PCCLONE
if (chsize(*fpoint,setsize) == 0) return 0;
  else return 1;
#else
if (ftruncate(*fpoint,setsize) == 0) return 0;
  else {
    perror ("Truncation Problem");
    return 1;
  }
#endif
}
