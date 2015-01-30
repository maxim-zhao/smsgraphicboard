; RAM usage

.struct PenMode
  IsSet   db ; 1 when the current pen mode icon is red
  Current db ; Current pen mode index - must follow preceding
  Dots    db ; 1 when drawing in dots, 0 when drawing in lines
.endst

.struct XY
  y db
  x db
.endst

.struct SpriteTable
  y dsb 64
  xn dsb 128
.endst

.enum $c000 asc export
RAM_ResetButton1                            db ; $c000 Currently pressed value
RAM_ResetButton2                            db ; $c001 Positive edge signal
RAM_unusedC002                              dsb 1
RAM_VDPReg1Value                            db ; $c003
RAM_VRAMFillHighByte                        db ; $c004
RAM_Write1bppToVRAMWithExtensionMask_Mask   db ; $c005 Work RAM for unused function Write1bppToVRAMWithExtensionMask ###
RAM_FrameCounter                            db ; $c006 Counter incremented on every frame. Used for detecting even/odd frame and not much else.
RAM_VBlankFunctionControl                   db ; $c007 bit 1 set means read the graphic board in the VBlank
RAM_SpriteTable2DirtyFlag                   db ; $c008 non-zero if sprite table should be copied to VRAM in VBlank
RAM_Beep                                    db ; $c009 ???
RAM_StatusBarTextIndex                      db ; $c00a Index of mode text to draw in status bar. Zero when no change is necessary.
RAM_PenMode                                 instanceof PenMode ; $c00b
RAM_TitleScreenTextFlashCounter             db ; $c00e Counter for title screen text flashing
RAM_TitleScreenTextFlashState               db ; $c00f 1 or 0 for title screen text state. Assumed to follow previous byte.
RAM_TitleScreenTextPointer                  dw ; $c010 Pointer to text for title screen to show
RAM_TitleScreenTextLocation                 dw ; $c012 VRAM write address for text for title screen to show
RAM_TitleScreenTextLength                   db ; $c014 Length of text for title screen to show
RAM_TitleScreenTextFlashSpeed               db ; $c015 Number of frames between flashes
RAM_GraphicsDataBuffer_VRAMAddress_Tiles    dw ; $c016 VRAM (write) address to use for GraphicsDataBuffer tile read/write. Argument to functions.
RAM_GraphicsDataBuffer_VRAMAddress_Tilemap  dw ; $c018 VRAM (write) address to use for GraphicsDataBuffer tilemap read/write. Argument to functions.
RAM_GraphicsDataBuffer_Dimensions           dw ; $c01a Row, column dimensions of area for GraphicsDataBuffer operations. Argument to functions.
RAM_unusedC01C                              dsb 16
RAM_ButtonsPressed                          db ; $c02c buttons pressed last time we looked
RAM_ButtonsNewlyPressed                     db ; $c02d buttons pressed last time we looked which were'nt pressed in the previous frame
RAM_Pen_Smoothed                            instanceof XY ; $c02e x and y are averages of themselves and the last raw value
RAM_PenX                                    db ; $c030 raw
RAM_Pen_Backup                              instanceof XY ; $c031 A backup? Not sure how that makes sense
RAM_PenY                                    db ; $C033
RAM_Pressure                                db ; $C034 - never used
RAM_unusedC035                              dsb 7
RAM_CurrentMode                             db ; $C03C Index into function pointer tables - low 6 bits only. High bits are ??? Signifies the current mode?
RAM_c03d                                    db
RAM_c03e                                    dw
RAM_unusedC040                              dsb 2
RAM_Palette                                 dsb 17 ; $C042
RAM_ColourSelectionStartValue               db ; $c053 Palette value for start of 8-colour palette used when choosing new colours
RAM_c054 db ; $c054
RAM_unusedC055                              dsb 13
RAM_c062 db ; $c062
RAM_c063 db ; $c063
RAM_c064 db ; $c064
RAM_c065 db ; $c065
RAM_c066 db ; $c066
RAM_c067 db ; $c067
RAM_c068 db ; $c068
RAM_c069 db ; $c069
RAM_c06a db ; $c06a
RAM_c06b db ; $c06b
RAM_c06c db ; $c06c
RAM_c06d db ; $c06d
RAM_c06e dw ; $c06e
RAM_c070 db ; $c070
RAM_c071 db ; $c071
RAM_c072 db ; $c072
RAM_TileModificationBuffer                  dsb 15 ; $C073 ; 4b+?
RAM_c082 db ; $c082
RAM_CurrentCursorIndex                      db ; $C083 Low bits are the cursor index, high bits are ??? TODO
RAM_CurrentCursorDataAddress                dw ; $C084 Pointer to cursor tile data
RAM_CursorColourCycle_Delay                 db ; $C086 Counter for frame between colour changes
RAM_CursorColourCycle_Index                 db ; $C087 Current colour index, must be following previous
RAM_ButtonStateShownOnScreen                db ; $C088 Holds the button bits as last drawn to the screen
RAM_c089 db ; $c089
RAM_c08a db ; $c08a
RAM_unusedC08B dsb 2
RAM_c08d dw ; $c08d
RAM_c08f dw ; $c08f
RAM_unusedC091 dsb 41
RAM_c0ba db ; $c0ba
RAM_BytesPerRow                             dw ; $c0bb Used during RAM<->VRAM tile copies
RAM_unusedc0bd dsb 1
RAM_c0be db ; $c0be
RAM_c0bf db ; $c0bf
RAM_c0c0 db ; $c0c0
RAM_c0c1 db ; $c0c1
RAM_c0c2 db ; $c0c2
RAM_c0c3 db ; $c0c3
RAM_c0c4 db ; $c0c4
RAM_c0c5 db ; $c0c5
RAM_c0c6 db ; $c0c6
RAM_c0c7 db ; $c0c7
RAM_c0c8 db ; $c0c8
RAM_c0c9 db ; $c0c9
RAM_unusedC0CA dsb 147
RAM_c15d db ; $c15d
RAM_c15e db ; $c15e
RAM_c15f db ; $c15f
RAM_c160 db ; $c160
RAM_c161 db ; $c161
RAM_c162 db ; $c162
RAM_SplashScreenTimeout                   dw ; $C163 ; 2b
RAM_unusedC165 dsb 1
RAM_c166 db ; $c166
RAM_unusedC167 dsb 8
RAM_c16f db ; $c16f
RAM_c170 db ; $c170
RAM_c171 dw ; $c171
RAM_unusedc173 dsb 15
RAM_UnknownWriteOnlyC182                  db ; $C182
RAM_UnknownWriteOnlyC183                  dw ; $C183
RAM_unusedc185 dsb 2
RAM_UnknownWriteOnlyC187                  dw ; $C187
RAM_unusedc188 dsb 119
RAM_SpriteTable1                          instanceof SpriteTable ; $C200 write here
RAM_SpriteTable2                          instanceof SpriteTable ; $C2C0 copy here for staging to VRAM
RAM_unusedC380 dsb 128
RAM_GraphicsDataBuffer                    dsb 5376 ; $c400 backup of graphics data when showing menus, or doing copy/mirror/???. Biggest size used seems to be the main menu at 12x14 tiles.
.ende
