# hysplitdata

This package provides Python classes to read HYSPLIT output files.  The
source code is written using Python 3.7 and it requires a few external
packages: please see setup.py for required external packages.

To install this package, run

    $ python setup.py install

To read a trajectory dump file, say, tdump.20190712, in a Python script:

    import hysplitdata.traj.model
    ...
    tdump = hysplitdata.traj.model.TrajectoryDump().get_reader().read("tdump.20190712")

Similarly, reading a concentration dump file is done:

    import hysplitdata.conc.model
    ...
    cdump = hysplitdata.conc.model.ConcentrationDump().get_reader().read("cdump.20190712")

Unit tests are written using the pytest framework. You will need to install
pytest if it is not alreay installed on your system.  To run the unit tests,
execute:

    $ cd tests
    $ pytest
