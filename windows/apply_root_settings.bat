cd %~dp0
call settings.bat || goto error
mklink %homepath%\os_settings %DOTRC% || goto error
exit
:error
set /p DUMMY=Hit ENTER to exit...