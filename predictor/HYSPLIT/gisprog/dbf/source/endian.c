/* 
 * $Source: /home/misc/greatdev/src/analysis/txt2dbf/lib/RCS/endian.c,v $
 * 
 * $Author: fkoorman $
 * 
 * $Revision: 1.1 $
 * $Date*
 * $Locker*
 *
 * Maarten Boekhold (boekhold@cindy.et.tudelft.nl) oktober 1995 
 *
 * Change history:
 * 10 Jun 2022 (SYZ) - Fix MSVC compilation error by including windefs.h.
 */

#include <sys/types.h>
#ifdef _MSC_VER
#include "windefs.h"
#endif
#include "dbf.h"
/*
 * routine to change little endian long to host long
 */
long get_long(u_char *cp)
{
        long ret;

        ret = *cp++;
        ret += ((*cp++)<<8);
        ret += ((*cp++)<<16);
        ret += ((*cp++)<<24);

        return ret;
}

void put_long(u_char *cp, long lval)
{
        cp[0] = lval & 0xff;
        cp[1] = (lval >> 8) & 0xff;
        cp[2] = (lval >> 16) & 0xff;
        cp[3] = (lval >> 24) & 0xff;
}

/*
 * routine to change little endian short to host short
 */
short get_short(u_char *cp)
{
        short ret;

        ret = *cp++;
        ret += ((*cp++)<<8);

        return ret;
}

void put_short(u_char *cp, short sval)
{
        cp[0] = sval & 0xff;
        cp[1] = (sval >> 8) & 0xff;
}
