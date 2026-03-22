#!/bin/sh

#---------------------------
# Last Revised: 7 Jul 2018  (AMC)

##testing for pardump options. 
##Can also be used for testing EMITTIMES vertical line source.

PGM=../exec

##emittimes file creates 4 vertical stacked line sources.
ef=EMITTIMES.txt
echo 'YYYY MM DD HH    DURATION(hhhh) #RECORDS'  >  $ef
echo "YYYY MM DD HH MM DURATION(hhmm) LAT LON HGT(m) RATE(/h) AREA(m2) HEAT(w)" >> $ef
echo '1995 10 17 00  9999  16 ' >>  $ef                                           
echo ' 1995 10 17 00 00 0100 61.00 15.00 2000   0.50 0   0 ' >> $ef                     
echo ' 1995 10 17 00 00 0100 61.00 15.00 2000   0.25 0   0 ' >> $ef                     
echo ' 1995 10 17 00 00 0100 61.00 15.00 10000  0.00 0   0 ' >> $ef                    
echo ' 1995 10 17 00 00 0100 61.00 15.00 10000  0.00 0   0 ' >> $ef                    
echo ' 1995 10 17 00 00 0100 65.00 15.00 10000  1.00 0   0 ' >> $ef                    
echo ' 1995 10 17 00 00 0100 65.00 15.00 10000  0.50 0   0 ' >> $ef                    
echo ' 1995 10 17 00 00 0100 65.00 15.00 15000  0.00 0   0 ' >> $ef                    
echo ' 1995 10 17 00 00 0100 65.00 15.00 15000  0.00 0   0 ' >> $ef                    
echo ' 1995 10 17 00 00 0100 70.00 15.00 4000   1.50 0   0 ' >> $ef                    
echo ' 1995 10 17 00 00 0100 70.00 15.00 4000   0.75 0   0 ' >> $ef                    
echo ' 1995 10 17 00 00 0100 70.00 15.00 5000   0.00 0   0 ' >> $ef                    
echo ' 1995 10 17 00 00 0100 70.00 15.00 5000   0.00 0   0 ' >> $ef                    
echo ' 1995 10 17 00 00 0100 75.00 15.00 1000   2.00 0   0 ' >> $ef                    
echo ' 1995 10 17 00 00 0100 75.00 15.00 1000   1.00 0   0 ' >> $ef                    
echo ' 1995 10 17 00 00 0100 75.00 15.00 10000  0.00 0   0 ' >> $ef              
echo ' 1995 10 17 00 00 0100 75.00 15.00 10000  0.00 0   0 ' >> $ef                     



##052 has ndump=0, ncycl=-2  (plot every time step including t=0 up to hour 2)
#$PGM/hycs_std 052
#$PGM/parxplot -k1 -iPARDUMP_052 -oplot_052.ps

##152 has ndump=0, ncycl=1  (plot every hour including t=0 up to hour 3)
#$PGM/hycs_std 152
#$PGM/parxplot -k1 -iPARDUMP_152 -oplot_152.ps

##252 has ndump=1, ncycl=1  (plot every hour NOT including t=0 up to hour 3)
#$PGM/hycs_std 252
#$PGM/parxplot -k1 -iPARDUMP_252 -oplot_252.ps

##352 has ndump=-1, ncycl=0  (plot every time step up to hour 1 NOT including t=0)
#$PGM/hycs_std 352
#$PGM/parxplot -k1 -iPARDUMP_352 -oplot_352.ps
