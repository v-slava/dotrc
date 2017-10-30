call %~dp0\..\settings.bat || goto error
:: cd %SOME_REPO_ROOT% || goto error
%GIT_BASH% || goto error
goto end
:error
set /p DUMMY=Hit ENTER to exit...
:end
