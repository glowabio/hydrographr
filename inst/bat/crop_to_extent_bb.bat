@echo on

set WSLENV=%1 %2 %3 %4 %5 %6 %7
set DATA=%1
set SHDIR=%7

wsl dos2unix %DATA%
wsl dos2unix %SHDIR% 
wsl bash  %SHDIR% $WSLENV