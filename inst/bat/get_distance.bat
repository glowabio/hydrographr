@echo on

set DATA=%2
set a=%1 %2 %3 %4 %5 %6 %7 %8 %9
shift
set b=%9
set WSLENV=%a% %b%
shift
set SHDIR=%b%

wsl dos2unix %DATA%
wsl dos2unix %SHDIR%
wsl bash  %SHDIR% $WSLENV
