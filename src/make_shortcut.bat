@echo off
rem CrossTab Studio - create a desktop shortcut (with the app icon)
cd /d "%~dp0"
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0tools\make_shortcut.ps1"
pause
