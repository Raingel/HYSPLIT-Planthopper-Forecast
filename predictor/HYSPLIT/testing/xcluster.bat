REM ################################################################
REM #    xcluster.bat -- Test of cluster program using             #
REM #                    trajectories in cluster/example/endpts.   #
REM #                    Force solution of 3                       # 
REM #          	         	                                   #
REM # Not as general as the AIX version                            # 
REM #                                                              # 
REM # Initial Version: 11-13-07                                    # 
REM # Last Revised: 09 Jan 2008                                    #
REM #               22 Jan 2010                                    #
REM ################################################################
  
SET PGM=..\exec

SET ilab1=Example
SET nc=3		

REM #################
REM # makeinfile.scr (must already have appropriate directory in INFILE)
REM #################
COPY ..\cluster\example\output\INFILE INFILE

REM #################
REM # cluster1.scr
REM #################
SET dur=12
SET int=1
SET skip=1
SET ODIR=.\
SET PROJ=0
   
ECHO %dur%  > CCONTROL
ECHO %int%  >> CCONTROL 
ECHO %skip% >> CCONTROL 
ECHO %ODIR% >> CCONTROL
ECHO %PROJ% >> CCONTROL

%PGM%\cluster 
DEL TNOCLUS TCLUS

REM #################
REM # cluster2.scr
REM #################
SET  mint=15  
SET  minc=3  
SET  maxc=10
SET  pct=30 
        
REM # TSV plot
%PGM%\clusplot  -iDELPCT -oCLUSPLOT.PS -l%ilab1%
TYPE CLUSPLOT.PS >>results.ps
      
REM # determine final number of clusters
%PGM%\clusend -iDELPCT -n%minc% -a%maxc% -t%mint% -p%pct% -oCLUSEND

REM # generate list of trajs in each cluster
%PGM%\cluslist -iCLUSTER -n%nc% -oCLUSLIST_%nc%

REM # add trajs not clustered (cluster #0) to CLUSLIST
TYPE CLUSLIST_%nc% >> CLUSTERno
DEL CLUSLIST_%nc%
RENAME CLUSTERno CLUSLIST_%nc%

REM # create TRAJ.INP.Cxx file for each cluster (list of trajs) 
%PGM%\clusmem -iCLUSLIST_%nc%

%PGM%\merglist -i+TRAJ.INP.C0_%nc% -omdump -ptdump
RENAME mdump.tdump C0.tdump
%PGM%\trajmean -i+TRAJ.INP.C0_%nc% 
RENAME tmean C0mean.tdump
%PGM%\merglist -i+TRAJ.INP.C1_%nc% -omdump -ptdump
RENAME mdump.tdump C1.tdump
%PGM%\trajmean -i+TRAJ.INP.C1_%nc% 
RENAME tmean C1mean.tdump
%PGM%\merglist -i+TRAJ.INP.C2_%nc% -omdump -ptdump
RENAME mdump.tdump C2.tdump
%PGM%\trajmean -i+TRAJ.INP.C2_%nc% 
RENAME tmean C2mean.tdump
%PGM%\merglist -i+TRAJ.INP.C3_%nc% -omdump -ptdump
RENAME mdump.tdump C3.tdump
%PGM%\trajmean -i+TRAJ.INP.C3_%nc% 
RENAME tmean C3mean.tdump

ECHO C1mean.tdump  > MEAN.LIST
ECHO C2mean.tdump >> MEAN.LIST
ECHO C3mean.tdump >> MEAN.LIST

%PGM%\merglist -i+MEAN.LIST -omdump -ptdump
RENAME mdump.tdump Cmean.tdump
DEL MEAN.LIST

REM  #################
REM  # cluster3.scr
REM  #################

ECHO 'TITLE^&','TRAJECTORIES IN CLUSTER 0 of 3 Example^&' > LABELS.CFG
TYPE LABELS.CFG
%PGM%\trajplot -iC0.tdump -e%dur% -z80 -k0 -l0 -v4 
TYPE TRAJPLOT.PS >>results.ps
RENAME TRAJPLOT.PS TRAJPLOT_C0.PS
DEL LABELS.CFG

ECHO 'TITLE^&','TRAJECTORIES IN CLUSTER 1 of 3 Example^&' > LABELS.CFG
TYPE LABELS.CFG
%PGM%\trajplot -iC1.tdump -e%dur% -z80 -k0 -l0 -v4
TYPE TRAJPLOT.PS >>results.ps
RENAME TRAJPLOT.PS TRAJPLOT_C1.PS
DEL LABELS.CFG

ECHO 'TITLE^&','TRAJECTORIES IN CLUSTER 2 of 3 Example^&' > LABELS.CFG
TYPE LABELS.CFG
%PGM%\trajplot -iC2.tdump -e%dur% -z80 -k0 -l0 -v4
TYPE TRAJPLOT.PS >>results.ps
RENAME TRAJPLOT.PS TRAJPLOT_C2.PS
DEL LABELS.CFG

ECHO 'TITLE^&','TRAJECTORIES IN CLUSTER 3 of 3 Example^&' > LABELS.CFG
TYPE LABELS.CFG
%PGM%\trajplot -iC3.tdump -e%dur% -z80 -k0 -l0 -v4
TYPE TRAJPLOT.PS >>results.ps
RENAME TRAJPLOT.PS TRAJPLOT_C3.PS
DEL LABELS.CFG
   
SET tmout=Cmean.tdump 
ECHO 'TITLE^&','CLUSTER MEAN TRAJECTORIES %ilab1%^&' > LABELS.CFG
TYPE LABELS.CFG
%PGM%\trajplot -i%tmout% -e%dur% -z80 -k%nc%:123 -l-12 -v4
TYPE TRAJPLOT.PS >>results.ps
RENAME TRAJPLOT.PS TRAJPLOT_MEAN.PS
DEL  LABELS.CFG

REM # ---------------------------------------------------------------------
REM # cleanup
DEL INFILE
DEL CCONTROL
DEL CLUSTER CMESSAGE DELPCT
DEL *.tdump
DEL TRAJ.INP*
DEL CLUSLIST_* CLUSEND
