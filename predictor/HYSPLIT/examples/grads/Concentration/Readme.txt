GRADS CONCENTRATION PLOTS
_________________________________________________________________

Convert Hysplit binary file to Grads format:

	con2grad [binary concentration file name]

	Creates:	grads.bin	->grads binary endpoint file
			grads.ctl	->grid definition file
			grads.gs	->specific plot script


Start Grads and then from the command line:

	ga-> exec makeplot.txt
	ga-> wi myfile.gif		then: ga-> print
	ga-> quit

	DOS Options:  	gxps -i plot.mif -o plot.ps


