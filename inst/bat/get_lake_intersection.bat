@echo off

REM Capture all 13 arguments
set ARG1=%~1
set ARG2=%~2
set ARG3=%~3
set ARG4=%~4
set ARG5=%~5
set ARG6=%~6
set ARG7=%~7
set ARG8=%~8
set ARG9=%~9
shift
set ARG10=%~9
shift
set ARG11=%~9
shift
set ARG12=%~9
shift
set ARG13=%~9

REM Convert line endings for the data file and shell script
wsl dos2unix "%ARG1%"
wsl dos2unix "%ARG13%"

REM Execute the shell script with all arguments
wsl bash "%ARG13%" "%ARG1%" "%ARG2%" "%ARG3%" "%ARG4%" "%ARG5%" "%ARG6%" "%ARG7%" "%ARG8%" "%ARG9%" "%ARG10%" "%ARG11%" "%ARG12%"
