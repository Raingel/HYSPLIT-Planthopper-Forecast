ARL PACKED METEOROLOGY TO GRADS
__________________________________


Convert meteorology file to Grads format:

	arl2grad [arl data file name]

	Creates:	grads.bin	->grads binary meteorology file
			grads.ctl	->station definition file
			grads.gs	->specific plot script

Start Grads and then the following commands:

	ga-> exec makeplot.txt
	ga-> wi myfile.gif	then: ga-> print
	ga-> quit

	From DOS: gxps -i plot.mif -o plot.ps
