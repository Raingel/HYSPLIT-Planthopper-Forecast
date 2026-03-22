/* 
 * $Source: /home/misc/greatdev/src/analysis/txt2dbf/include/RCS/utils.h,v $
 * 
 * $Author: fkoorman $
 * 
 * $Revision: 1.2 $
 * $Date*
 * $Locker*
 *
 * Description: header of utils.c
 *	collection of useful functions:
 *	- getline
 *	- do_nothing	
 *	- tabtok
 *	explanations see below.
 */

#ifndef __utils__
#define __utils__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
 
/* global variables ----------------------------------------------------*/
/* action of a program after evaluating the command line		*/
#define ABORT 0
#define EVALUATE 1
#define LIST 2

/* function declaration ------------------------------------------------*/

/* getline: reads a line (limited by \n) out of stream fp, returns last 
 * char, esp eof							*/
int parseline(FILE *fp, char s[]);

/* do_nothing: dto.							*/
void do_nothing( void );

/* tabtok --------------------------------------------------------------
 * like strtok, breaks a string in sequences delimited by tabs, but do not 
 * overreads sequences of directly followed tabs: like "\t\t\ttest" is
 * divided by strtok into "test" but by tabtok into "", "", "", "test"	*/  
char *tabtok( char *s );

/* dtok --------------------------------------------------------------
 * like strtok, breaks a string in sequences delimited by delim, but do not 
 * overreads sequences of directly followed delims: like "\t\t\ttest" is
 * divided by strtok into "test", but by dtok into "", "", "", "test"	*/  
char *dtok( char *s , char delim );

#endif

