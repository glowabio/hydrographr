@echo off

set WSLENV=%1 %2 %3 %4 %5
set SHDIR=%5

wsl dos2unix %SHDIR%
wsl bash  %SHDIR% $WSLENV
