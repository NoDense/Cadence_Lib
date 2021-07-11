cd %~dp0
allegro -s CustomShapes.scr
pad_designer -s 2020-01-07_01-54-38_pads.scr
CALL :checkfile "RX10p63Y57p53D0T.pad"
CALL :checkfile "RX10p63Y69p53D0T.pad"
CALL :checkfile "RX10p63Y45p53D0T.pad"
allegro -s 2020-01-07_01-54-38_brd.scr
CALL :checkfile "LQFP100_14X14.psm"
CALL :checkfile "LQFP100_14X14-M.psm"
CALL :checkfile "LQFP100_14X14-L.psm"

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
