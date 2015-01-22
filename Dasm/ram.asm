; RAM usage
; Should be converted to a ramsection or enum later...

.define RAM_ResetButton1 $C000 ; 1b Currently pressed value
.define RAM_ResetButton2 $C001 ; 1b Positive edge signal
.define RAM_VDPReg1Value $C003 ; 1b
.define RAM_VRAMFillHighByte $C004 ; 1b
.define RAM_Write1bppToVRAMWithExtensionMask_Mask $C005 ; 1b Work RAM for unused function Write1bppToVRAMWithExtensionMask
.define RAM_FrameCounter $c006 ; 1b Counter incremented on every frame. Used for detecting even/odd frame and not much else.
.define RAM_VBlankFunctionControl $C007 ; 1b - bit 1 set means read the graphic board in the VBlank
.define RAM_SpriteTable2DirtyFlag $C008 ; 1b - non-zero if sprite table should be copied to VRAM in VBlank
.define RAM_Beep $C009 ;  1b ???
.define RAM_StatusBarTextIndex $C00A ; 1b Index of mode text to draw in status bar. Zero when no change is necessary.
;---
.define RAM_TitleScreenTextFlashCounter $C00E ; 1b Counter for title screen text flashing
.define RAM_TitleScreenTextFlashState   $C00F ; 1b 1 or 0 for title screen text state. Assumed to follow previous byte.
.define RAM_TitleScreenTextPointer      $C010 ; 2b Pointer to text for title screen to show
.define RAM_TitleScreenTextLocation     $C012 ; 2b VRAM write address for text for title screen to show
.define RAM_TitleScreenTextDimensions   $C014 ; 2b Dimensions for text for title screen to show
.define RAM_TitleScreenTextFlashSpeed   $C015 ; 1b Number of frames between flashes
;
.define RAM_GraphicsDataBuffer_VRAMAddress_Tiles $C016 ; 2b VRAM (write) address to use for GraphicsDataBuffer tile read/write. Argument to functions.
.define RAM_GraphicsDataBuffer_VRAMAddress_Tilemap $C018 ; 2b VRAM (write) address to use for GraphicsDataBuffer tilemap read/write. Argument to functions.
.define RAM_GraphicsDataBuffer_Dimensions $C01A ; 2b Row, column dimensions of area for GraphicsDataBuffer operations. Argument to functions.
;
.define RAM_ButtonsPressed $C02C ; 1b: buttons pressed last time we looked
.define RAM_ButtonsNewlyPressed $C02D ; 1b: buttons pressed last time we looked which were'nt pressed in the previous frame
.define RAM_PenY_Smoothed $C02E ; 1b: average of itself and the last raw value
.define RAM_PenX_Smoothed $C02F ; 1b: average of itself and the last raw value
.define RAM_PenX $C030 ; 1b
;---
.define RAM_PenY $C033 ; 1b
.define RAM_Pressure $C034 ; 1b - never used
.define RAM_NonVBlankDynamicFunction $C03C ; 1b Index into function pointer tables - low 6 bits only. High bits are ???
;---
.define RAM_Palette      $C042 ; 17b
;---
.define RAM_TileModificationBuffer $C073 ; 4b+?
;---
.define RAM_CurrentCursorIndex $C083 ; 1b Low bits are the cursor index, high bits are ??? TODO
.define RAM_CurrentCursorDataAddress $C084 ; 2b Pointer to cursor tile data
;---
.define RAM_CursorColourCycle_Delay $C086 ; 1b Counter for frame between colour changes
.define RAM_CursorColourCycle_Index $C087 ; 1b Current colour index, must be following previous
;---
.define RAM_ButtonStateShownOnScreen $C088 ; 1b Holds the button bits as last drawn to the screen
;---
.define RAM_BytesPerRow $c0bb ; 2b Used during RAM<->VRAM tile copies
;---
.define RAM_SplashScreenTimeout $C163 ; 2b
;---
.define RAM_UnknownWriteOnlyC182 $C182 ; 1b
.define RAM_UnknownWriteOnlyC183 $C183 ; 2b
;---
.define RAM_UnknownWriteOnlyC187 $C187 ; 2b
;---
.define RAM_SpriteTable1 $C200 ; 192b - write here
.define RAM_SpriteTable1_Y RAM_SpriteTable1
.define RAM_SpriteTable1_XN RAM_SpriteTable1+64
.define RAM_SpriteTable2 $C2C0 ; 192b - copy here for staging to VRAM
.define RAM_SpriteTable2_Y RAM_SpriteTable2
.define RAM_SpriteTable2_XN RAM_SpriteTable2+64
; ---
.define RAM_GraphicsDataBuffer $c400 ; lots of bytes?