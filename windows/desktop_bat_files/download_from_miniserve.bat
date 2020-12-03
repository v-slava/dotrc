set IP=xxx.xxx.xxx.xxx

:: Use the following portable tftp server for windows:
:: https://www.softpedia.com/get/PORTABLE-SOFTWARE/Internet/Servers/Tftpd32-Portable.shtml#download

:: remove old data, if any:
if exist miniserve rmdir /s /q miniserve || goto error
if exist miniserve.zip del miniserve.zip || goto error
if exist tftpd\zImage del tftpd\zImage || goto error
if exist tftpd\1.dtb del tftpd\1.dtb || goto error
if exist tftpd\initramfs.cpio del tftpd\initramfs.cpio || goto error

curl http://%IP%:8080/?download=zip -o miniserve.zip || goto error
"C:\Program Files\7-Zip\7z.exe" x miniserve.zip || goto error
del miniserve.zip || goto error
move miniserve\* tftpd\ || goto error
rmdir miniserve || goto error

exit
:error
set /p DUMMY=Hit ENTER to exit...
