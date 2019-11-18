@echo off
python3 gen_report.py --outlook || goto error
exit
:error
set /p DUMMY=Hit ENTER to exit...
