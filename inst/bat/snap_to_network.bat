@echo on

set WSLENV=%1 %2 %3 %4 %5 %6 %7 %8 %9
set DATA=%1
shift
set SHDIR=%9

wsl dos2unix %DATA%
wsl dos2unix %SHDIR% 
wsl bash  %SHDIR% $WSLENV