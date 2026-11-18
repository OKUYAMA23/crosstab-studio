@echo off
rem CrossTab Studio - start with a visible log (use this when the normal start does not work)
cd /d "%~dp0"
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0tools\launch.ps1" -ShowLog
echo.
echo ---- startup.log ----
if exist "%~dp0startup.log" type "%~dp0startup.log"
pause
