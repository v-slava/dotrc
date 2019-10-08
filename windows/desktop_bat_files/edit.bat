call %~dp0\..\settings.bat || goto error
set ORIGINAL_ARGS=%*
:: set MODIFIED_ARGS=%ORIGINAL_ARGS:\=/%
set MODIFIED_ARGS=%ORIGINAL_ARGS%
%GIT_BASH% -c 'e %MODIFIED_ARGS%' || goto error
goto end
:error
set /p DUMMY=Hit ENTER to exit...
:end