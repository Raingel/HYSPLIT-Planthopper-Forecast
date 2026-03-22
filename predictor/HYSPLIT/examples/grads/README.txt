DIRECTORY:	GRADS
Last Revised:	06 Oct 2003
____________________________________

Provides source code and executables to convert Hysplit trajectory,
concentration, and meteorological data files into Grads format.

The general syntax for the Grads conversion programs:

	[???]2bin [file name]

	where ??? =

	asc - for trajectories
	con - for concentrations
	arl - for meteorological data

These fortran programs all create the following grads files:

	grads.bin	-> data file
	grads.ctl	-> definition file
	grads.gs	-> plot script

All displays are created from the Grads command prompt:

	exec makeplot.txt

See the Readme files in the individual directories for more infomation.

