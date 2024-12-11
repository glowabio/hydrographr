@echo on

set WSLENV=%1 %2 %3 %4 %5 %6 %7 %8 %9
set RULES=%2
set SHDIR=%9

wsl dos2unix %RULES%
wsl dos2unix %SHDIR%
wsl bash  %SHDIR% $WSLENV
