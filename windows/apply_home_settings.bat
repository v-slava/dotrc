call settings.bat || goto error
cd "%DOTRC%\windows\home_settings" || goto error
for %%f in (*) do copy %%f %homepath%\ || goto error
cd "%DOTRC_S%\windows\home_settings" || goto error
for %%i in (*) do type %%i >> %homepath%\%%i || goto error
exit
:error
set /p DUMMY=Hit ENTER to exit...