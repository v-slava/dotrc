call settings.bat || goto error

cd "%DOTRC%\windows\home_settings" || goto error
:: for /r %%f in (*) do copy %%f %homedrive%%homepath%\ || goto error
xcopy /e /f /h /i /r /y * %homedrive%%homepath%\ || goto error

echo | set /p="let g:My_win_dotrc = '%DOTRC%'" > %TEMP%\.vimrc
type %homedrive%%homepath%\.vimrc >> %TEMP%\.vimrc || goto error
copy %TEMP%\.vimrc %homedrive%%homepath%\.vimrc || goto error

cd "%DOTRC%\windows\desktop_bat_files" || goto error
for %%f in (*) do call %DOTRC%\windows\bin\create_shortcut.bat %DOTRC%\windows\desktop_bat_files\%%f %homedrive%%homepath%\Desktop\%%f.lnk

if exist "%DOTRC_S%\windows\home_settings" (
    cd "%DOTRC_S%\windows\home_settings" || goto error
    for %%f in (*) do type %%f >> %homedrive%%homepath%\%%f || goto error
)

if exist "%DOTRC_S%\windows\desktop_bat_files" (
    cd "%DOTRC_S%\windows\desktop_bat_files" || goto error
    for %%f in (*) do call %DOTRC%\windows\bin\create_shortcut.bat %DOTRC_S%\windows\desktop_bat_files\%%f %homedrive%%homepath%\Desktop\%%f.lnk || goto error
)

:: call %DOTRC%\windows\bin\create_shortcut.bat C:\Windows\System32\devmgmt.msc %homedrive%%homepath%\Desktop\device_manager.lnk || goto error

exit
:error
set /p DUMMY=Hit ENTER to exit...
