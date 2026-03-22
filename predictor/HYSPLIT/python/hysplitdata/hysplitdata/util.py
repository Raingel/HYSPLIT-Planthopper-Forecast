# ---------------------------------------------------------------------------
# NOAA Air Resources Laboratory
#
# util.py
#
# Declare a function used by the package.
# ---------------------------------------------------------------------------

import datetime
import logging
import sys


logger = logging.getLogger(__name__)


def restore_year(yr):
    return 2000 + yr if (yr < 40) else 1900 + yr


def myzip(xlist, ylist):
    if sys.version_info[0] >= 3:
        # Python 3 or later
        return list(zip(xlist, ylist))
    else:
        # Python 1 and 2
        return zip(xlist, ylist)


def make_datetime(year, mo, day, hr, min, sec, usec, tz):
    try:
        dt = datetime.datetime(year, mo, day, hr, min, sec, usec, tz)
    except ValueError as ex:
        dt = datetime.datetime(1970, 1, 1, 0, 0, 0, 0, tz)
        logger.error("%s: using %s", str(ex), dt)
    return dt
