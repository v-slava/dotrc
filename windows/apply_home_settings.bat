call settings.bat || goto error

cd "%DOTRC%\windows\home_settings" || goto error
for /r %%f in (*) do copy %%f %homepath%\ || goto error

cd "%DOTRC%\windows\desktop_bat_files" || goto error
for %%f in (*) do call %DOTRC%\windows\bin\create_shortcut.bat %DOTRC%\windows\desktop_bat_files\%%f %homepath%\Desktop\%%f.lnk

cd "%DOTRC_S%\windows\home_settings" || goto error
for %%f in (*) do type %%f >> %homepath%\%%f || goto error

cd "%DOTRC_S%\windows\desktop_bat_files" || goto error
for %%f in (*) do call %DOTRC%\windows\bin\create_shortcut.bat %DOTRC_S%\windows\desktop_bat_files\%%f %homepath%\Desktop\%%f.lnk || goto error

call %DOTRC%\windows\bin\create_shortcut.bat C:\Windows\System32\devmgmt.msc %homepath%\Desktop\device_manager.lnk || goto error

exit
:error
set /p DUMMY=Hit ENTER to exit...