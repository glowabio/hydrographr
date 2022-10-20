@echo on

set WSLENV=%1 $2 %3 %4 %5 %6
set RULES=%2
set SHDIR=%7

wsl dos2unix %RULES%
wsl dos2unix %SHDIR% 
wsl bash  %SHDIR% $WSLENV