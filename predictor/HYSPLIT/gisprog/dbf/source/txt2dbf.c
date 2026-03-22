/*
 * $Source: /home/greatdev/src/analysis/txt2dbf/src/RCS/txt2dbf.c,v $
 *
 * $Author: jwagner $
 * 
 * $Revision: 1.5 $
 * $Date: 1998/09/09 12:37:00 $
 * $Locker:  $
 *
 * Description:
 *	Converting tab-delimited ASCII-Tables in the dbase-III-Format.
 *	It is necessary to describe the structure of the table on the 
 *	commandline:
 *		-Cn	text, n characters
 *		-In	integer, with n digits
 *		-Rn.d	real, with n digits (including '.') and d decimals
 *
 *	To handle with *dbf-files, this program uses the functions and
 *	datastructures of a by me modified version of dbf.c, written
 *	by Maarten Boeklund (boeklund@cindy.et.tudelft.nl)
 *
 * Change history:
 * 10 Jun 2022 (SYZ) - Fix MSVC compilation error by including windefs.h.
 */
 
#include <stdio.h>
#include <fcntl.h>
#ifdef _MSC_VER
#include "windefs.h"
#else
#include <unistd.h>
#endif
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "dbf.h"
#include "utils.h"
#include "main.h"

/* program information -------------------------------------------------*/
char *progname;
char usage[] = "[{-Cn | -In | -Rn.d}] [-d delimiter] [-v] txt-file dbf-file";
char version[] = "txt2dbf 1.0.2, 09.09.1998 by Frank Koormann";

/* type definition -----------------------------------------------------*/
/* content of record as list of char-vectors 				*/
typedef char **tRecord;

/* global variables ----------------------------------------------------*/

/* Fielddescription: type, length, decimals
   The structure is described by a list of fielddescriptions 		*/
typedef struct {
		char 	type;
		int	n,d;
		} tFieldDescr;
	
/* empty string noData. If a field in a record is empty (or missed), the
 * concerning pointer is directed to noData to avoid errors while
 * coping NULL pointers with strcpy					*/
char noData[] = "";

/* function declaration ------------------------------------------------*/

/* add field to the list of fielddesriptions 				*/
tFieldDescr *tabAddField( char type, char *format, tFieldDescr *ptr, int num );

/* read line from the ASCII-table and write it divided by delim in the 
 * record description, delim should contain ONE delimiter !		*/
int readRecord(FILE *fp, tRecord rec, int fields, char * delim );


/* main ----------------------------------------------------------------*/
int main( int argc, char *argv[] )
{	
	/* common */
	char *dummy;
	char *delim = "\t";
	int i;
	int v=0;	/* verbose? 0: no, 1: yes  			*/ 
	int cnt;	/* Counter for written Records 			*/

	/* .txt */
	char *txtname = NULL;
	tRecord record;

	tFieldDescr *tabDescr = NULL;
	int numFields = 0;

	/* .dbf */
	dbhead *dbh;
	field  *rec;
	char   *dbfname = NULL;

	/* Get program name and eval commandline			*/
	progname = *argv;	
	OPT
		ARG 'C' : PARM	
				tabDescr = 
				    tabAddField('C',*argv,tabDescr,++numFields);
			NEXTOPT
		ARG 'I' : PARM	
				tabDescr = 
				    tabAddField('I',*argv,tabDescr,++numFields);
			NEXTOPT
		ARG 'R' : PARM	
				tabDescr =
				    tabAddField('R',*argv,tabDescr,++numFields);
			NEXTOPT
		ARG 'd' : PARM
				delim = *argv;
			NEXTOPT
		ARG 'v' :
				fprintf(stderr,"%s\n",version);
				v=1;
			NEXTOPT
		OTHER 
			fprintf(stderr,"Usage: %s %s\n",progname, usage);
			exit(1);
	ENDOPT

	/* after processing the commandline, argv should contain the two 
         * filenames ... else error and abort				*/
	txtname = argv[0];
	dbfname = argv[1];

	if ( argc != 2 )
	{	fprintf(stderr,"Usage: %s %s\n",progname, usage);
                exit(1);
	}

	if (v)
		fprintf(stderr," %s --> %s\n",txtname,dbfname);

	/* alloc mem for the recordhead (numFields vectors of char) 	*/
	if ((record = (tRecord ) calloc(numFields, sizeof(char *))) == NULL)
		perror(progname), exit(1);  

	/* alloc mem for the vectors (with constant size)    	*/
	for (i=0; i<numFields; i++) {
		if ((record[i] = (char *) calloc(255, sizeof(char)))
			== NULL )
			perror(progname), exit(1);
	}
	
	/* open the txt-file */
	if ( (strcmp(txtname, "-") != 0) && (!freopen(txtname, "r", stdin)) ) 
		perror(progname), exit(1);

	/* read the first line (contains fieldnames)			*/
	if ( readRecord(stdin, record, numFields, delim) == EOF )
	{	fprintf(stderr,"%s: %s: empty file\n",progname,txtname);
		exit(1);
	}

	/* cut of '#' out of first fieldname (the '#' declares the line for
 	 * some programs, i.e. gnuplot, as comment)			*/
	if ( record[0][0] == '#' )
	{
		/*
		dummy = record[0];
		dummy++;
		strcpy(record[0], dummy);
		 */
		/*
		 * 8 JUN 2020
		 * For macOS, strcpy() should not be used when the source
		 * and destination strings overlap. The outcome is undefined.
                 */
                int k;
		char* input = record[0];
		int len = strlen(input);
		for (k = 1; k < len; k++) input[k-1] = input[k];
		input[len - 1] = 0;
	}
	
	/* create new dbase-file					*/
	dbh = dbf_open_new(dbfname, OPENNEWFLAGS);

	if (v)
		fprintf(stderr," FIELDS:\n NAME\t\tTYPE\tLENGTH\tDECIMALS\n");
		
	/* add fields to dbf						*/
	for (i=0; i<numFields; i++) {
		dbf_add_field(dbh, record[i], tabDescr[i].type, 
			tabDescr[i].n, tabDescr[i].d);
		if (v)
			fprintf(stderr," %-12s\t%c\t%d\t%d\n",
				record[i], tabDescr[i].type,
				tabDescr[i].n, tabDescr[i].d);
	}	
	dbf_write_head(dbh);
	dbf_put_fields(dbh);

	/* write records into dbf					*/
	if (v)
	{	cnt = 0;
		fprintf(stderr," RECORDS:\n");
	}
	while ( readRecord(stdin, record, numFields, delim ) != EOF )
	{
		rec = dbf_build_record(dbh);
		for (i=0; i< numFields; i++) {
			if (strlen(record[i]) > tabDescr[i].n) {
				strcpy(rec[i].db_contents, noData);
				if (v)
					fprintf(stderr, " ignoring %s (too long) ", record[i]);
			} else {
				strcpy(rec[i].db_contents,record[i]);
			}
		}

		dbf_put_record(dbh, rec, dbh->db_records + 1);
		dbf_free_record(dbh, rec);
		if (v)
			fprintf(stderr," [%d]",++cnt);

	}

	/* rewrite head (num of records known yet)			*/
	dbf_write_head(dbh);

	close(dbh->db_fd);
	free(dbh);

	if (v)
		fprintf(stderr,"\n %d Records written into %s\n",cnt, dbfname);
	return 0;
}

/* tabAddField ---------------------------------------------------------*/
/* Add field to the list of fielddescriptions				*/
tFieldDescr *tabAddField( char type, char *format, tFieldDescr *ptr, int num )
{	int new;

	if (( ptr = (tFieldDescr *)realloc(ptr, num*sizeof(tFieldDescr)))==NULL) 
	{	perror(progname);
		exit(1);
	}
	
	new = num-1;

	switch ( type ) {
		case 'C' : 	ptr[new].type = 'C';
			   	ptr[new].n    = atoi(format);
				ptr[new].d    = 0;
			   break;
		case 'I' : 	ptr[new].type = 'N';
			   	ptr[new].n    = atoi(format);
				ptr[new].d    = 0;
			   break;
		case 'R' :	ptr[new].type = 'N';
				ptr[new].n    = atoi(strtok(format,".")); 
				ptr[new].d    = atoi(strtok(NULL,"."));
			   break;
	} 
	
	return ptr;
}

/* readRecord ----------------------------------------------------------*/
/* read line from the ASCII-table and write it divided in the 
 * record description							*/
int readRecord(FILE *fp, tRecord rec, int fields, char * delim )
{ 	char line[255];
	char *dummy;
	int ret,i;

	ret = parseline(fp, line);

	if ( ret != EOF )
	{	strcpy(rec[0], dtok(line, delim[0] ));
		for (i=1; i<fields; i++)	
			if ((dummy = dtok(NULL, delim[0] )) != NULL)
				strcpy(rec[i], dummy);
			else
			{	strcpy(rec[i], noData);
				fprintf(stderr," Missing field %d:",i);
			}
	}

	return ret;
}

