/*
 * $Source: /home/greatdev/src/analysis/txt2dbf/src/RCS/dbf2txt.c,v $
 *
 * $Author: fkoorman $
 * 
 * $Revision: 1.2 $
 * $Date: 1999/04/23 12:45:16 $
 * $Locker:  $
 *
 * Description:
 *	Reading a *.dbf file and writing as tab-delimited ASCII-table.
 *
 *	To handle with *.dbf-files, this program uses the functions and
 *	datastructures of a by me modified version of dbf.c, written
 *	by Maarten Boeklund (boeklund@cindy.et.tudelft.nl)
 *
 * History: 
 *	Wed Jun 19 10:50:10 MET DST 1996 : creation
 *	Mon Jul 22 16:17:25 MET DST 1996 : write to stdout (for pipes)
 *
 *	$Log: dbf2txt.c,v $
 * Revision 1.2  1999/04/23  12:45:16  fkoorman
 * - configurable delimiter (cmd-line -d)
 * - tailing delimiter bug fixed
  *
 * Change history:
 * 10 Jun 2022 (SYZ) - Fix MSVC compilation error by including windefs.h.
 */

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#ifdef _MSC_VER
#include "windefs.h"
#else
#include <unistd.h>
#endif
#include <string.h>

#include "dbf.h"
#include "main.h"

/* program information -------------------------------------------------*/
char *progname;
char usage[] = "[-d delimiter] [-v] dbf-file";
char version[] = "dbf2txt 0.5.2, 14.01.2000 by Frank Koormann";
 
/* main ----------------------------------------------------------------*/
int main( int argc, char *argv[] )
{
	/* common */
	int i,j, result;
	int v = 0;		/* verbose? 0 = no, 1 = yes 		*/
	char *delim= "\t";
 	
	/* .dbf */
	dbhead *dbh;
	field  *fields;
	char *dbfname = NULL;

	/* save programname and eval the commandline 			*/
	progname = *argv;
	OPT
		ARG 'd' : PARM
				delim = *argv;
			NEXTOPT
		ARG 'v' :
			fprintf(stderr,"%s\n",version);
			v = 1;
			NEXTOPT
		OTHER
			fprintf(stderr,"Usage: %s %s\n",progname,usage);
			exit(1);
	ENDOPT

	dbfname = argv[0];
	
	if  ( argc == 0 )
	{	fprintf(stderr,"Usage: %s %s\n",progname,usage);
		exit(1);
	}

	/* filename given ? --> open file 				*/
	dbh = dbf_open(dbfname, O_RDONLY);

	if ( dbh == (dbhead *)DBF_ERROR )
	{	perror(progname);
		exit(1);
	}

        if (v)
        {       fprintf(stderr," %s --> stdout\n",dbfname);
		fprintf(stderr," FIELDS:\n NAME\t\tTYPE\tLENGTH\tDECIMALS\n");
	}

	/* Print Fieldnames into *.txt with leading '#' 		*/
	if (dbh->db_fields[0].db_name[0] != '#')
		printf("#");
	for (i=0; i<dbh->db_nfields-1; i++)
	{	printf("%s%c",dbh->db_fields[i].db_name,*delim);
		if (v)
			fprintf(stderr," %-12s\t%c\t%d\t%d\n",
				dbh->db_fields[i].db_name, 
				dbh->db_fields[i].db_type,
				dbh->db_fields[i].db_flen,
				dbh->db_fields[i].db_dec);
	}

	printf("%s\n",dbh->db_fields[dbh->db_nfields-1].db_name);
	if (v)
		fprintf(stderr," %-12s\t%c\t%d\t%d\n",
			dbh->db_fields[i].db_name, 
			dbh->db_fields[i].db_type,
			dbh->db_fields[i].db_flen,
			dbh->db_fields[i].db_dec);
	
	if (v)
		fprintf(stderr," RECORDS:\n");

	/* Print the contents of the dbase file 			*/
	for (i=0; i<dbh->db_records; i++ )
	{	if (( fields = dbf_build_record(dbh)) != (field *)DBF_ERROR)
		{	result = dbf_get_record(dbh, fields, i);
			if (result == DBF_VALID)
			{	for (j=0; j<dbh->db_nfields-1; j++)
					printf("%s%c",fields[j].db_contents,*delim);
				printf("%s\n",fields[dbh->db_nfields-1].db_contents);
			}
		}
		else
		{	fprintf(stderr,"Error in database ! Program aborted\n");
			return 1;
		}
		dbf_free_record(dbh, fields);
	if (v)
		fprintf(stderr," [%d]",i+1);
	}

	close(dbh->db_fd);
	free(dbh);

	if (v)
		fprintf(stderr,"\n %d Records written to stdout\n",i);

	return 0;
}
