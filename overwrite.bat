@ECHO OFF
CLS
COLOR 17

CD %~dp0

IF EXIST ROM_NEW.sms ERASE ROM_NEW.sms
IF EXIST ROM_NEW.sym ERASE ROM_NEW.sym


ECHO.
ECHO Compiling Object File...
IF %ERRORLEVEL% EQU 0 WLADX\wla-z80 -v overwrite.asm

ECHO.
ECHO Compiling start file...
_spg\sjasmplus "start.asm"
_spg\sjasmplus "intro.asm"

ECHO.
ECHO Linking ROM...
ECHO ===============================================================================
IF %ERRORLEVEL% EQU 0 wladx\wlalink -r overwrite.link _spg/ROM_NEW.sms

ECHO.

cd _spg
spgbld.exe -b spgbld.ini Ottifants.spg -c 0

cd ../
ERASE overwrite.o
PAUSE