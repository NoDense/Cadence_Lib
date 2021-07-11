cd %~dp0
allegro -s CustomShapes.scr
pad_designer -s 2020-03-18_00-42-30_pads.scr
CALL :checkfile "EX22Y22D0T.pad"
allegro -s 2020-03-18_00-42-30_brd.scr
CALL :checkfile "98ASA00222D.psm"

exit

:checkfile
@echo off
dir %1 1>nul 2>nul
if errorlevel 1 goto checkfile_err
goto end
:checkfile_err
echo Expected file %1 not found
pause > nul
exit
:end
@echo on
