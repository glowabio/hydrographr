@echo off

set WSLENV=%1 %2 %3 %4 %5 %6 %7 %8
set SHDIR=%8

wsl dos2unix %SHDIR% 
wsl bash  %SHDIR% $WSLENV