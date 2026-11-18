@echo off
rem CrossTab Studio - open in the default browser tab instead of the app window
cd /d "%~dp0"
start "" /min powershell -NoProfile -ExecutionPolicy Bypass -WindowStyle Hidden -File "%~dp0tools\launch.ps1" -BrowserTab
exit /b
