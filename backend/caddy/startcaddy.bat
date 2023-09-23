@ECHO OFF
cd /d %~dp0
REM start /min startphp.bat
SET HOME=%~dp0
SET CADDYPATH=%~dp0
caddy -conf="caddy.config"