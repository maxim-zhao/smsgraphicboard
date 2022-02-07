WLA-Z80 = wla-z80
WLALINK = wlalink
BMP2TILE = bmp2tile
GENERATED_FILES = *.sms *.o *.makefile Graphics\*.1bpp Graphics\*-tiles.bin *.sym
7Z = 7z

DUMMY: smsgraphicboard.sms

# Graphics conversion
# We do not make %.pscompr because our compressor does not produce bit-identical output
Graphics/%.2bpp: Graphics/PNG/%.2bpp.png
	bmp2tile "$<" -noremoveduplicates -savetiles "$@"

Graphics/%.bin: Graphics/PNG/%.4bpp.png
	bmp2tile "$<" -noremoveduplicates -savetiles "$@"

smsgraphicboard.sms.o: smsgraphicboard.sms.asm \
	sms-definitions.asm \
	definitions.asm \
	ram.asm \
	macros.asm \
	graphics.asm \
	graphicboard.asm \
	maths.asm \
	titlescreen.asm \
	Graphics/logo-tilemap.bin \
	Graphics/logo-tiles.2bpp \
	Graphics/sega-logo.lsbtilemap \
	Graphics/sega-logo.pscompr \
	Graphics/font-tiles.pscompr \
	Graphics/cursor-tiles.bin \
	Graphics/pen-tiles.2bpp \
	Graphics/button-tiles.2bpp \
	Graphics/font.2bpp \
	Graphics/control-tiles.pscompr
	$(WLA-Z80) -o $@ $<

smsgraphicboard.sms: smsgraphicboard.sms.o linkfile
	$(WLALINK) -d -r -v -S -A linkfile $@

clean:
	del $(GENERATED_FILES)