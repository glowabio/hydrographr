@echo on

set WSLENV=%1 %2 %3 %4 %5
set DATA=%1
set OUT=%3
set SHDIR=%5

wsl dos2unix %DATA%
wsl dos2unix %OUT%
wsl dos2unix %SHDIR%
wsl bash  %SHDIR% $WSLENV
