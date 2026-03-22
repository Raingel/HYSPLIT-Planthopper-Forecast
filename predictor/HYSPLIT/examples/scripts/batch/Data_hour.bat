@ECHO OFF
for %%h in (0000 0300 0600 0900 1200 1500 1800 2100) do call Data_convert.bat %1 %2 %3 %%h
