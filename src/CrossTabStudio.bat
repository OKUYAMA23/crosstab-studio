@echo off
rem CrossTab Studio - start
rem 1) open the loading screen right away (Edge / Chrome app window), 2) start R in the background.
rem The loading screen switches to the app when R writes app\www\ready.js with the same key (K).
cd /d "%~dp0"
set "BR="
for %%P in ("%ProgramFiles(x86)%\Microsoft\Edge\Application\msedge.exe" "%ProgramFiles%\Microsoft\Edge\Application\msedge.exe" "%ProgramFiles%\Google\Chrome\Application\chrome.exe" "%ProgramFiles(x86)%\Google\Chrome\Application\chrome.exe" "%LocalAppData%\Google\Chrome\Application\chrome.exe") do (
  if not defined BR if exist "%%~P" set "BR=%%~P"
)
set "K=%RANDOM%%RANDOM%"
set "SPL=%~dp0app\www\splash.html"
set "SPL=%SPL:\=/%"
if defined BR start "" "%BR%" --app="file:///%SPL%?k=%K%" --window-size=1320,860
start "" /min powershell -NoProfile -ExecutionPolicy Bypass -WindowStyle Hidden -File "%~dp0tools\launch.ps1" -SplashShown -Token %K%
exit /b
