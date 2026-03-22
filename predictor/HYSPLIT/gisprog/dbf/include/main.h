/* 
 * $Source: /home/misc/greatdev/src/analysis/txt2dbf/include/RCS/main.h,v $
 * 
 * $Author: fkoorman $
 *
 *	main.h -- 3.4 Oct 18 12:39:34 1992
 *	Copyright (c) 1986-1992 Axel T. Schreiner
 *
 *	argument standards
 * 
 * $Revision: 1.1 $
 * $Date*
 * $Locker*
 *
 */
#ifndef	__MAIN_H
#define	__MAIN_H


#if defined NeXT && defined __cplusplus
extern "C" {
#endif

#include <string.h>	/* strlen */

#if defined __STDC__ || defined __cplusplus
#define	MAIN	int main (int argc, char * argv[])
#else
#define	MAIN	int main (argc, argv) int argc; char * argv[];
#endif

#define	OPT	while (--argc > 0)					\
		{   switch (**++argv) {					\
	 	    case '-':						\
			switch (*++*argv) {				\
			case 0:     --*argv;				\
				    break;				\
			case '-':   if (! (*argv)[1])			\
				    {	++argv, --argc;			\
					break;				\
				    }					\
			default:    do					\
				    {	switch (**argv) {		\
					case 0:

#define	ARG					continue;		\
					case

#define	PARM					if (*++*argv)		\
						    ;			\
						else if (--argc > 0)	\
						    ++argv;		\
						else			\
						{   --*argv, ++argc;	\
							break;		\
						}

#define	NEXTOPT					*argv += strlen(*argv) - 1;

#define	OTHER					continue;		\
					}

#define	ENDOPT			    } while (*++*argv);			\
				    continue;				\
			}						\
			break;						\
		    }							\
		    break;						\
		}

#define	IFANY	if (argc > 1) while (-- argc > 0)			\
		{	static char __any;				\
			if (**++argv == '-'				\
			    && ! __any) switch (*++*argv) {		\
			case 0:		--*argv;			\
					break;				\
			case '-':	if (! (*argv)[1])		\
					{	__any = 1;		\
						continue;		\
					}				\
			default:	do {				\
						switch (**argv)		\
						case 0:

#define	ANY				} while (*++*argv);		\
					continue;			\
				}

#if defined NeXT && defined __cplusplus
};
#endif
#endif
