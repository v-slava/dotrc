@echo off
:: if not defined in_subprocess (cmd /k set in_subprocess=y ^& %0 %*) & exit )
echo Command: %* Command output:
%* 2>&1 || goto continue
:continue
echo.
if %errorlevel% equ 0 (
    echo Command succeeded ^(exit code = 0^)
) else (
    echo Command failed ^(exit code = %errorlevel%^)
)
