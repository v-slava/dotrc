cd %~dp0
call settings.bat || goto error
if not exist %homepath%\.spacemacs (
    mklink /h %homepath%\.spacemacs %DOTRC%\other_files\dot_spacemacs.el || goto error
)
exit
:error
set /p DUMMY=Hit ENTER to exit...
