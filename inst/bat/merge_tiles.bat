@echo on

set WSLENV=%1 %2 %3
set DATA=%1
set SHDIR=%3

wsl dos2unix %DATA%
wsl dos2unix %SHDIR% 
wsl bash  %SHDIR% $WSLENV