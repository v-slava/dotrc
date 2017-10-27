:: Usage example:
:: create_shortcut.bat C:\file1.txt Desktop\file1.lnk

@echo off
set TARGET=%1
set SHORTCUT=%2
set PWS=powershell.exe -ExecutionPolicy Bypass -NoLogo -NonInteractive -NoProfile
%PWS% -Command "$s=(New-Object -COM WScript.Shell).CreateShortcut('%SHORTCUT%');$s.TargetPath='%TARGET%';$s.Save()" || goto error
goto end
:error
set /p DUMMY=Hit ENTER to exit...
:end