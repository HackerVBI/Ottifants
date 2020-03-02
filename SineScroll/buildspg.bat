@echo off
set PROJNAME=sinescroll

spgbld.exe -b spgbld.ini %PROJNAME%.spg -c 0
if exist %PROJNAME%.spg goto ok

:err
echo * ERROR! *

:ok
