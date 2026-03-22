GRADS TRAJECTORY PLOTS
_______________________________________________________

Convert ASCII trajectory endpoints file to Grads format:

	asc2grad [ascii endpoints file name]

	Creates:	grads.bin	->grads binary endpoint file
			grads.ctl	->station definition file
			grads.gs	->specific plot script


Start Grads and then enter the following from the command prompt:

	ga-> exec makeplot.txt
	ga-> wi myfile.gif	then: ga-> print
	ga-> quit

	From DOS: gxps -i plot.mif -o plot.ps

