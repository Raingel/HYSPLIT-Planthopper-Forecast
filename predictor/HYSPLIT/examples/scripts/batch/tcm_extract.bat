REM The HYSPLIT simulation for each unit source release date
REM (1 unit/hour) should be configured to produce an output
REM file with the name convention TG_{MMDDHH}.

REM The program condecay -{Cnumb:Index:HalfL:Radio} multiplies
REM the unit source factors in each TG file by the emissions
REM defined in file "cfactors.txt" according to the release date.
REM The mapping is defined on the command line:

REM condecay -1:3:8.04:Igas -2:2:8.04:Ipar -3:2:11000.0:Cpar

REM where in this case the first argument (-1:3:8.04:Igas) multiplies
REM the first column in the emission file (I-131g gaseous Iodine) by
REM the 3rd variable (Dgas) in the dispersion factor file, using a 
REM half-life of 8.04 days and assigning the 4-character variable ID
REM as Igas. The output from condecay is written to a file named:

REM DG_{release date}

REM The final step is to merge the individual release files into one file
REM using the program "conmerge", the resulting output file

REM cdump_merged

REM can be used to display the concentration time series at any location
REM using the standard program

REM con2stn

REM ------------------------------------------------------------------------------

cd c:\temp

REM USAGE: condecay -{Cnumb:Index:HalfL:Radio}
REM for example -{1:3:8.04:Igas} maps I-131g (column=1) with Dgas (Index=3)
c:\hysplit4\exec\condecay -1:1:8.04:Igas

REM create list of processed files
dir /b DG_??????_ >merglist.txt 

REM The final step is to merge the individual release files into one file
c:\hysplit4\exec\conmerge -imerglist.txt -ocdump_merged

REM convert binary file to text time series for sites in locations.txt (Bq to mBq)
c:\hysplit4\exec\con2stn  -icdump_merged -ofukushima.txt -slocations.txt -c1000.0

REM plot model prediction time series fukushima.txt as primary file
REM plot measured values at Dutch_Harbor.txt as the secondary file
c:\hysplit4\exec\timeplot -ifukushima.txt -sDutch_Harbor.txt -y
