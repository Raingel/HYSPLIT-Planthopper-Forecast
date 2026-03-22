#!/bin/ksh

if [ -f edas.txt ];then rm edas.txt; fi
let numb=1

for mon in jan feb mar apr may jun jul aug sep oct nov dec; do
for per in 001 002; do

dir="/pub/long/archives/edas/"
dat="edas.subgrd.${mon}02.${per}"

echo "Started: ${dat}"

xtrct_stn <<EOD
${dir}
${dat}
21
TCLD 01
LCLD 01
MCLD 01
HCLD 01
RELH 02
RELH 03
RELH 04
RELH 05
RELH 06
RELH 07
RELH 08
RELH 09
RELH 10
RELH 11
RELH 12
RELH 13
RELH 14
RELH 15
RELH 16
RELH 17
RELH 18
40.0 -90.0
1
edas.asc
${numb}
EOD

let numb=${numb}+128
cat edas.asc >>edas.txt
rm edas.asc

done
done
