@echo on

set WSLENV=%1 %2 %3 %4 %5 %6
set DATA=%1
set SHDIR=%6

wsl dos2unix %DATA%
wsl dos2unix %SHDIR% 
wsl bash  %SHDIR% $WSLENV