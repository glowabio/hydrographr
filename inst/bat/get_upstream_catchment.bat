@echo on

set DATA=%1
set a=%1 %2 %3 %4 %5 %6 %7 %8 %9
SHIFT
SHIFT
set b=%8
set WSLENV=%a% %b%
set SHDIR=%9

wsl dos2unix %DATA%
wsl dos2unix %SHDIR%
wsl bash  %SHDIR% $WSLENV
