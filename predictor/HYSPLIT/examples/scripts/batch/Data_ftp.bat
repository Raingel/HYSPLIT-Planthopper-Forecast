@ECHO OFF

set NUMB=3333

echo user anonymous first.last@host.com       >ftp.txt
echo cd /pub/downloads/nomads/model-%NUMB%   >>ftp.txt
echo binary                                  >>ftp.txt  
echo mget *                                  >>ftp.txt
echo bye                                     >>ftp.txt

ftp -n ftp.ncdc.noaa.gov <ftp.txt
