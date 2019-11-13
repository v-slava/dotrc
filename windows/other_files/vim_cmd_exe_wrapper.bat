@echo off
echo Command: %* Command output:
%*
echo.
if %errorlevel% equ 0 (
    echo Command succeeded ^(exit code = 0^)
) else (
    echo Command failed ^(exit code = %errorlevel%^)
)
