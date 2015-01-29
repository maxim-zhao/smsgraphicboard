@echo off
set WLAPATH=..\..\wla\
set BMP2TILE=..\..\bmp2tile\bmp2tile.exe

rem speed up
goto :skipgraphics

rem Graphics
rem We don't build the pscompr because the BMP2Tile compressor is better than the one Sega used...
rem "%BMP2TILE%" "Graphics\PNG\Control tiles.pscompr.png" -noremovedupes -savetiles "Graphics\Control tiles.pscompr" -exit
rem "%BMP2TILE%" "Graphics\PNG\Font tiles.pscompr.png"    -noremovedupes -savetiles "Graphics\Font tiles.pscompr"    -exit
rem "%BMP2TILE%" "Graphics\PNG\Sega logo.pscompr.png"       -removedupes -savetiles "Graphics\Sega logo.pscompr"     -tileoffset 144 -savetilemap "Graphics\Sega logo.lsbtilemap" -exit

rem 4bpp graphics. We want a .4bpp extension for clarity.
"%BMP2TILE%" "Graphics\PNG\Cursor tiles.4bpp.png"     -noremovedupes -savetiles "Graphics\Cursor tiles.bin"      -exit
if exist "Graphics\Cursor tiles.4bpp" del /q "Graphics\Cursor tiles.4bpp"
ren "Graphics\Cursor tiles.bin" "Cursor tiles.4bpp"

rem 2bpp graphics
"%BMP2TILE%" "Graphics\PNG\Button tiles.2bpp.png"     -noremovedupes -savetiles "Graphics\Button tiles.2bpp"     -exit
"%BMP2TILE%" "Graphics\PNG\Font.2bpp.png"             -noremovedupes -savetiles "Graphics\Font.2bpp"             -exit
"%BMP2TILE%" "Graphics\PNG\Logo tiles.2bpp.png"       -noremovedupes -savetiles "Graphics\Logo tiles.2bpp"       -exit
"%BMP2TILE%" "Graphics\PNG\Pen tiles.2bpp.png"        -noremovedupes -savetiles "Graphics\Pen tiles.2bpp"        -exit

:skipgraphics

rem Compile
"%WLAPATH%wla-z80.exe" -o "Sega Graphic Board v2_0 [Proto].sms.asm" "object.o"
if errorlevel 1 goto :end

rem Link
echo [objects]>linkfile
echo object.o>>linkfile
"%WLAPATH%wlalink.exe" -drvs linkfile "Sega Graphic Board v2_0 [Proto].sms"
if errorlevel 1 goto :end

rem Compare
fc /b "Sega Graphic Board v2_0 [Proto].sms" target.sms

:end
rem Clean up 
del /q *.o
del linkfile
