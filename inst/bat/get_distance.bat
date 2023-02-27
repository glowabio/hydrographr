@echo on

set WSLENV=%1 %2 %3 %4 %5 %6 %7 %8 %9 %10 %11 %12
set DATA=%2
shift
set SHDIR=%12

wsl dos2unix %DATA%
wsl dos2unix %SHDIR%
wsl bash  %SHDIR% $WSLENV
