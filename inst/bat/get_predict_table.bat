@echo on

set WSLENV=%1 %2 %3 %4 %5 %6 %7 %8 %9

set DATA1=%4
set DATA2=%5
set SHDIR=%9

wsl dos2unix %DATA1%
wsl dos2unix %DATA2%
wsl dos2unix %SHDIR%

wsl bash  %SHDIR% $WSLENV
