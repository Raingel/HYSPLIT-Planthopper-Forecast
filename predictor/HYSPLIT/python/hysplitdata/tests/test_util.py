# ---------------------------------------------------------------------------
# NOAA Air Resources Laboratory
#
# test_util.py
#
# Performs unit tests on functions and class methods defined in util.py.
# ---------------------------------------------------------------------------

import pytest
import pytz
from hysplitdata import util


def test_restore_year():
    assert util.restore_year( 0) == 2000
    assert util.restore_year(39) == 2039
    assert util.restore_year(40) == 1940
    assert util.restore_year(99) == 1999


def test_myzip():
    a = util.myzip([1, 2], [3, 4])

    assert len(a) == 2
    assert a[0] == (1, 3)
    assert a[1] == (2, 4)


def test_make_datetime():
    dt = util.make_datetime(2020, 1, 16, 8, 0, 1, 2, pytz.utc)
    assert dt.year == 2020
    assert dt.month == 1
    assert dt.day == 16
    assert dt.hour == 8
    assert dt.minute == 0
    assert dt.second == 1
    assert dt.microsecond == 2
    assert dt.tzinfo is pytz.utc

    dt = util.make_datetime(0, 0, 0, 8, 0, 1, 2, pytz.utc)
    assert dt.year == 1970
    assert dt.month == 1
    assert dt.day == 1