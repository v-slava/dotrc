cd %~dp0
call settings.bat || goto error
if not exist %homepath%\os_settings (
    mklink %homepath%\os_settings %DOTRC% || goto error
)
if not exist %homepath%\.spacemacs (
    mklink /h %homepath%\.spacemacs %homepath%\os_settings\other_files\dot_spacemacs.el || goto error
)
exit
:error
set /p DUMMY=Hit ENTER to exit...