====================================================================================
                                                                  rev 12 June 2014
====================================================================================

Add several sets of test endpoints files to c:\hysplit4\cluster\example\endpts
Results from these clusters are in output/cluster_rev609_20140612.pdf

In the script ..\..\testing\xcluster_example.scr, the appropriate subdir_list= 
line can be chosen and/or edited to run the sets of clusters as needed.
By default the script in ..\..\testing runs only the "Example".


====================================================================================
c:\hysplit4\cluster\example\readme - example run.txt                  20 Sept. 2006
====================================================================================

Instructions to run the trajectory cluster analysis using the 12-h forward
trajectories in c:\hysplit4\cluster\example\endpts.  Reference output is provided in
c:\hysplit4\cluster\example\output.

----------------------------------------------------------------------
Trajectory - Special Simulations - Run Cluster Analysis - Run Example
----------------------------------------------------------------------

--------
Step 1 
--------
No changes needed.

--------
Step 2
--------
(4) Make INFILE  
	-runs immediately
        -INFILE is a text file listing the filenames of the trajectory endpts files
(5) Run cluster
        -window will open showing progress of run
        -sample runs quickly
(6) Display plot
        -create/view postscript file that depicts graphically possible final 
         number of clusters (9, 7, 3)
(7) Run
        -leave default of 30%
        -window will open almost immediately displaying text file CLUSEND 
              (9, 7, and 3 clusters; percent change)
--------
Step 3
--------
(8) Number of clusters:  9 (or 7 or 3)
(9) Text
        -creates text file CLUSLIST_9 (or 7 or 3) listing trajectories 
         in each cluster
        -View opens window displaying file
(10) Display Means
        -check "View", vertical coordinate "None", Execute Display
        -one plot with the mean trajectory of each cluster
(11) Display Clusters, Execute Display
        -one plot per cluster showing all trajectories in that cluster
(12) Trajectories not used
        -automatically shows the trajectories (here only 1) with less than 12-h duration
        -in the "Trajectory Cluster Display" window, choose Execute Display to see 
         the cluster means, including the mean of the trajectory not used

(can repeat 8-12 with other possible final number of clusters)

--------
Archive -moves output from working to archive folder
--------

--------
Exit
--------