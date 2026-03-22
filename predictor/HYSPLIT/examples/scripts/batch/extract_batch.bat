@ECHO OFF

for %%Y in (200 2005) do (
for %%M in (01 02 03 04 05 06 07 08 09 10 11 12) do (

echo .\               >input.txt
echo RP%%Y%%M.gbl    >>input.txt
echo -25.0  80.0     >>input.txt
echo  25.0 130.0     >>input.txt
echo 18              >>input.txt

\home\hysplit4\exec\xtrct_grid <input.txt
rename extract.bin RP%%Y%%M.bmg
del extract.cfg message input.txt

)
)