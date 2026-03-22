#!/usr/bin/env python
#
# ---------------------------------------------------------------------------
# NOAA Air Resources Laboratory
#
# setup.py
#
# For installation of this package.
#
# usage: python setup.py install
# ---------------------------------------------------------------------------

from setuptools import setup
from hysplitdata import meta

setup(
    name="hysplitdata",
    version=meta.__version__,
    description="HYSPLIT Data Model",
    author=meta.__author__,
    author_email=meta.__email__,
    packages=["hysplitdata", "hysplitdata.traj", "hysplitdata.conc"],
    python_requires="==3.7",
    install_requires=["numpy==1.20.1", "pytz==2021.1"]
)
