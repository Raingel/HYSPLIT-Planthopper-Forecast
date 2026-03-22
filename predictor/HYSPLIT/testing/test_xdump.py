#!/usr/bin/env python
# ---------------------------------------------------------------------------
# NOAA Air Resources Laboratory
#
# test_xdump.py
#
# Report changes in numerical values in tdump and cdump files.
# ---------------------------------------------------------------------------
#
# The HYSPLIT python packages must be installed before running this script.
# See ../python/install_linux.txt, install_win10.txt, or install_mac.txt
# depending on your operating system.
#
# Usage:
#
#   $ conda activate hysplit
#   $ pytest
#
#
# Last Revised: 
#      30 March 2021 (AMC) - Added test_044


import datetime
import pytest
import pytz
import hysplitdata.conc.model as cm
import hysplitdata.traj.model as tm

# relative tolerance for comparing two floating-point numbers
_epsilon = 1.0e-6

# absolute tolerance for comparing lat, lon (deg) of trajectory endpoints.
degabs = 0.005
# absolute tolerance for comparing height (m)  at trajectory endpoints.
htabs = 10
# absolute tolerance for comparing pressure (hPa)  at trajectory endpoints.
prsabs = 5

def test_001():
    tdump = tm.TrajectoryDump().get_reader().read('tdump_001')
    # expect 1 trajectory
    assert len(tdump.trajectories) == 1

    t = tdump.trajectories[0]
    # first point
    assert t.latitudes[0] == pytest.approx(40.000)
    assert t.longitudes[0] == pytest.approx(-90.000)
    assert t.heights[0] == pytest.approx(1500.0)
    assert t.others['PRESSURE'][0] == pytest.approx(827.0)
    # last point
    assert t.latitudes[-1] == pytest.approx(32.437)
    assert t.longitudes[-1] == pytest.approx(-84.745)
    assert t.heights[-1] == pytest.approx(826.1)
    assert t.others['PRESSURE'][-1] == pytest.approx(910.1)

def test_002():
    tdump = tm.TrajectoryDump().get_reader().read('tdump_002')
    # expect 1 trajectory
    assert len(tdump.trajectories) == 1

    t = tdump.trajectories[0]
    # first point
    assert t.latitudes[0] == pytest.approx(32.417)
    assert t.longitudes[0] == pytest.approx(-84.740)
    assert t.heights[0] == pytest.approx(823.9)
    assert t.others['PRESSURE'][0] == pytest.approx(911.7)
    # last point
    assert t.latitudes[-1] == pytest.approx(39.933)
    assert t.longitudes[-1] == pytest.approx(-89.918)
    assert t.heights[-1] == pytest.approx(1463.0)
    assert t.others['PRESSURE'][-1] == pytest.approx(832.0)

def test_003():
    tdump = tm.TrajectoryDump().get_reader().read('tdump_003')
    # expect 1 trajectory
    assert len(tdump.trajectories) == 1

    t = tdump.trajectories[0]
    # first point
    assert t.latitudes[0] == pytest.approx(40.000)
    assert t.longitudes[0] == pytest.approx(-90.000)
    assert t.heights[0] == pytest.approx(1500.0)
    assert t.others['PRESSURE'][0] == pytest.approx(827.0)
    # last point
    assert t.latitudes[-1] == pytest.approx(29.790,abs=degabs)
    assert t.longitudes[-1] == pytest.approx(-81.910,abs=degabs)
    assert t.heights[-1] == pytest.approx(1772.1)
    assert t.others['PRESSURE'][-1] == pytest.approx(826.8)

def test_004():
    tdump = tm.TrajectoryDump().get_reader().read('tdump_004')
    # expect 1 trajectory
    assert len(tdump.trajectories) == 1

    t = tdump.trajectories[0]
    # first point
    assert t.latitudes[0] == pytest.approx(29.776)
    assert t.longitudes[0] == pytest.approx(-81.877)
    assert t.heights[0] == pytest.approx(1767.5)
    assert t.others['PRESSURE'][0] == pytest.approx(827.5)
    # last point
    assert t.latitudes[-1] == pytest.approx(39.960)
    assert t.longitudes[-1] == pytest.approx(-89.985)
    assert t.heights[-1] == pytest.approx(1471.0)
    assert t.others['PRESSURE'][-1] == pytest.approx(830.5)

def test_005():
    tdump = tm.TrajectoryDump().get_reader().read('tdump_005')
    # expect 27 trajectories
    assert len(tdump.trajectories) == 27

    t = tdump.trajectories[0]
    # first point
    assert t.latitudes[0] == pytest.approx(40.000)
    assert t.longitudes[0] == pytest.approx(-90.000)
    assert t.heights[0] == pytest.approx(1500.0)
    assert t.others['PRESSURE'][0] == pytest.approx(827.0)
    # last point
    assert t.latitudes[-1] == pytest.approx(32.437)
    assert t.longitudes[-1] == pytest.approx(-84.745)
    assert t.heights[-1] == pytest.approx(826.1)
    assert t.others['PRESSURE'][-1] == pytest.approx(910.1)

def test_006():
    tdump = tm.TrajectoryDump().get_reader().read('tdump_006')
    # expect 36 trajectories
    assert len(tdump.trajectories) == 36

    t = tdump.trajectories[0]
    # first point
    assert t.latitudes[0] == pytest.approx(35.000)
    assert t.longitudes[0] == pytest.approx(-95.000)
    assert t.heights[0] == pytest.approx(1500.0)
    assert t.others['PRESSURE'][0] == pytest.approx(832.5)
    # last point
    assert t.latitudes[-1] == pytest.approx(35.018)
    assert t.longitudes[-1] == pytest.approx(-93.171)
    assert t.heights[-1] == pytest.approx(1579.7)
    assert t.others['PRESSURE'][-1] == pytest.approx(825.9)

def test_007():
    tdump = tm.TrajectoryDump().get_reader().read('tdump_007')
    # expect 6 trajectories
    assert len(tdump.trajectories) == 6

    t = tdump.trajectories[0]
    # first point
    assert t.latitudes[0] == pytest.approx(40.000)
    assert t.longitudes[0] == pytest.approx(-90.000)
    assert t.heights[0] == pytest.approx(1500.0)
    assert t.others['PRESSURE'][0] == pytest.approx(827.0)
    # last point
    assert t.latitudes[-1] == pytest.approx(36.672)
    assert t.longitudes[-1] == pytest.approx(-83.935)
    assert t.heights[-1] == pytest.approx(1189.0)
    assert t.others['PRESSURE'][-1] == pytest.approx(842.3)

def test_008():
    tdump = tm.TrajectoryDump().get_reader().read('tdump_008')
    # expect 1 trajectory
    assert len(tdump.trajectories) == 729

    t = tdump.trajectories[0]
    # first point
    assert t.latitudes[0] == pytest.approx(40.000)
    assert t.longitudes[0] == pytest.approx(-90.000)
    assert t.heights[0] == pytest.approx(750.0)
    assert t.others['PRESSURE'][0] == pytest.approx(906.7)
    # last point
    assert t.latitudes[-1] == pytest.approx(33.335)
    assert t.longitudes[-1] == pytest.approx(-88.555)
    assert t.heights[-1] == pytest.approx(35.9)
    assert t.others['PRESSURE'][-1] == pytest.approx(1004.1)

def test_009():
    tdump = tm.TrajectoryDump().get_reader().read('tdump_009')
    # expect 1 trajectory
    assert len(tdump.trajectories) == 1

    t = tdump.trajectories[0]
    # first point
    assert t.latitudes[0] == pytest.approx(40.000)
    assert t.longitudes[0] == pytest.approx(-90.000)
    assert t.heights[0] == pytest.approx(1500.0)
    assert t.others['PRESSURE'][0] == pytest.approx(827.0)
    # last point
    assert t.latitudes[-1] == pytest.approx(32.437)
    assert t.longitudes[-1] == pytest.approx(-84.745)
    assert t.heights[-1] == pytest.approx(826.1)
    assert t.others['PRESSURE'][-1] == pytest.approx(910.1)

def test_109():
    tdump = tm.TrajectoryDump().get_reader().read('tdump_109')
    # expect 3 trajectories
    assert len(tdump.trajectories) == 3

    t = tdump.trajectories[0]
    # first point
    assert t.latitudes[0] == pytest.approx(45.000)
    assert t.longitudes[0] == pytest.approx(-85.000)
    assert t.heights[0] == pytest.approx(500.0)
    assert t.others['PRESSURE'][0] == pytest.approx(919.0)
    # last point
    assert t.latitudes[-1] == pytest.approx(47.215,abs=degabs)
    assert t.longitudes[-1] == pytest.approx(-48.735,abs=degabs)
    assert t.heights[-1] == pytest.approx(773.3, abs=htabs)
    assert t.others['PRESSURE'][-1] == pytest.approx(918.3,abs=prsabs)

def test_010():
    cdump = cm.ConcentrationDump().get_reader().read('cdump_010')
    # expect 1 concentration grid
    assert len(cdump.grids) == 1
    # check other things in the cdump file
    assert len(cdump.vert_levels) == 1
    assert len(cdump.pollutants) == 1
    assert len(cdump.latitudes) == 601
    assert len(cdump.longitudes) == 601

    g = cdump.grids[0]
    utc = pytz.utc
    assert g.time_index == 0
    assert g.starting_datetime == datetime.datetime(1995, 10, 16, 0, 0, 0, 0, utc)
    assert g.ending_datetime == datetime.datetime(1995, 10, 16, 12, 0, 0, 0, utc)
    assert g.starting_forecast_hr == 0
    assert g.ending_forecast_hr == 0  # TODO: why is this a zero?
    # max conc value
    assert g.latitudes[299] == pytest.approx(39.95)
    assert g.longitudes[301] == pytest.approx(-89.95)
    assert g.conc[299, 301] == pytest.approx(0.51E-11, abs(0.51E-11*_epsilon))
    # two other values
    assert g.conc[299, 302] == pytest.approx(0.35E-11, abs(0.35E-11*_epsilon))
    assert g.conc[299, 303] == pytest.approx(0.53E-12, abs(0.53E-12*_epsilon))



def test_044():
    # basic test for numgrd = -2 and -4.
    # This compares concentrations at one point for
    # the numgrd -2 and -4 cases with regular runs.
    # could possibly expand to look at more points.

    # 044 is base run with kmsl=0 (agl)
    # 244a (numgrd -2) should match 044
    cdumpa = cm.ConcentrationDump().get_reader().read('cdump_044')
    cdumpb = cm.ConcentrationDump().get_reader().read('cdump_244a')
    ga = cdumpa.grids[0]
    gb = cdumpb.grids[0]
    yii = 304
    xii = 311
    assert ga.conc[yii, xii] == pytest.approx(gb.conc[yii,xii],abs=1e-14)
    print(ga.conc[yii,xii], gb.conc[yii,xii])

    # 144 is base run with kmsl=1 (msl)
    # 344b (numgrd -2) should match 144
    cdumpc = cm.ConcentrationDump().get_reader().read('cdump_144')
    cdumpd = cm.ConcentrationDump().get_reader().read('cdump_344b')
    gc = cdumpc.grids[4]
    gd = cdumpd.grids[4]
    yii = 304
    xii = 311
    #print(gc.latitudes[yii])
    #print(gc.longitudes[xii])
    print(gc.conc[yii,xii], gd.conc[yii,xii])
    assert gc.conc[yii, xii] == pytest.approx(gd.conc[yii,xii],abs=1e-14)

    # 044 is base run with kmsl=0 (agl)
    # 444a (numgrd -4) should match 044
    # 444c (numgrd -4) should match 044 
    cdumpe = cm.ConcentrationDump().get_reader().read('cdump_044')
    cdumpf = cm.ConcentrationDump().get_reader().read('cdump_444a')
    cdumpg = cm.ConcentrationDump().get_reader().read('cdump_444c')
    ge = cdumpe.grids[0]
    gf = cdumpf.grids[0]
    gg = cdumpg.grids[0]
    yii = 304
    xii = 311
    #print(gc.latitudes[yii])
    #print(gc.longitudes[xii])
    print(ge.conc[yii,xii], gf.conc[yii,xii], gg.conc[yii,xii])
    assert ge.conc[yii, xii] == pytest.approx(gf.conc[yii,xii],abs=1e-14)
    assert ge.conc[yii, xii] == pytest.approx(gg.conc[yii,xii],abs=1e-14)

    # 144 is base run with kmsl=0 (agl)
    # 544b (numgrd -4) should match 144
    # 544d (numgrd -4) should match 144 
    cdumph = cm.ConcentrationDump().get_reader().read('cdump_144')
    cdumpi = cm.ConcentrationDump().get_reader().read('cdump_544b')
    cdumpj = cm.ConcentrationDump().get_reader().read('cdump_544d')
    gh = cdumph.grids[4]
    gi = cdumpi.grids[4]
    # 544d is run with two less vertical levels than 144.
    gj = cdumpj.grids[2]
    yii = 304
    xii = 311
    #print(gc.latitudes[yii])
    #print(gc.longitudes[xii])
    print(gh.conc[yii,xii], gi.conc[yii,xii], gj.conc[yii,xii])
    assert gh.conc[yii, xii] == pytest.approx(gi.conc[yii,xii],abs=1e-14)
    assert gh.conc[yii, xii] == pytest.approx(gj.conc[yii,xii],abs=1e-14)



