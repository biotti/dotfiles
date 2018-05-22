@REM ECHO off
SET OLD_DIR=%cd%
SET PSSCRIPT=install.ps1
SET PSCMD=%~dp0%PSSCRIPT%
ECHO Esecuzione dello script PowerShell %PSCMD%
ECHO Per il profilo %USERPROFILE% (%USERDOMAIN%\%USERPROFILE%)
ECHO .
CD %~dp0
git submodule update --init --recursive
PowerShell -Command Start-Process "$PSHome\PowerShell.exe" -Verb RunAs -ArgumentList '-ExecutionPolicy bypass -File %PSCMD% %USERDOMAIN% %USERNAME% %USERPROFILE%'
CD %OLD_DIR%
SET PSCMD=
SET PSSCRIPT=
SET OLD_DIR=
