@ECHO OFF
CLS
COLOR 17

CD %~dp0

IF EXIST ROM_NEW.sms ERASE ROM_NEW.sms
IF EXIST ROM_NEW.sym ERASE ROM_NEW.sym


ECHO.
ECHO Compiling Object File...
IF %ERRORLEVEL% EQU 0 WLADX\wla-z80 -v Ottifants.asm

ECHO.
ECHO Compiling start file...
_spg\sjasmplus "start.asm"

ECHO.
ECHO Linking ROM...
ECHO ===============================================================================
IF %ERRORLEVEL% EQU 0 wladx\wlalink -r Ottifants.link _spg/ROM_NEW.sms

ECHO.

cd _spg
spgbld.exe -b spgbld.ini start.spg -c 0

cd ../
ERASE Ottifants.o
PAUSE