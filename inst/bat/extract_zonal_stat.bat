@echo off

set WSLENV=%1 %2 %3 %4 %5 %6 %7
set SHDIR=%7

wsl dos2unix %SHDIR%
wsl bash  %SHDIR% $WSLENV
