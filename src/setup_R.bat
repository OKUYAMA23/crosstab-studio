@echo off
rem CrossTab Studio - first-time setup: puts R and two packages inside this folder
cd /d "%~dp0"
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0tools\setup_R.ps1"
pause
