echo 00 00 00 %1        >CONTROL
echo 1                 >>CONTROL
echo 40.0 -90.0 10.0   >>CONTROL
echo 48                >>CONTROL
echo 0                 >>CONTROL
echo 10000.0           >>CONTROL
echo 1                 >>CONTROL
echo ../metdata/       >>CONTROL
echo oct1618.BIN       >>CONTROL
echo ./                >>CONTROL
echo tdump.%1          >>CONTROL

\hysplit4\exec\hyts_std
\hysplit4\exec\trajplot -itdump.%1 -oplot%1


