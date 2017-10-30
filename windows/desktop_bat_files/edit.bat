call %~dp0\..\settings.bat || goto error
%GIT_BASH% -c e || goto error
goto end
:error
set /p DUMMY=Hit ENTER to exit...
:end