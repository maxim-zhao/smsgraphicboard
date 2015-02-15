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

.struct DrawingData
  c062                          db ; $c062 0 for lines, 1 for dots?
  PixelXPlus0                   db ; $c063 Used during drawing of the pen
  PixelXToDraw                  db ; $c064
  PixelXPlus1                   db ; $c065
  PixelXMinus1                  db ; $c066
  PixelYPlus0                   db ; $c067
  PixelYToDraw                  db ; $c068
  PixelYPlus1                   db ; $c069
  PixelYMinus1                  db ; $c06a
  PenStyleForCurrentShape       db ; $c06b Copy of RAM_PenStyle but zero for fill mode
  CurrentlySelectedPaletteIndex db ; $c06c Index of currently selected item in the top palette
  ButtonsPressed_virtual        db ; $c06d Pen bits for the purposes of the current operation (e.g. pen button maintained during drawing)
  SquareCorner1                 dsb 0 ; $c06e Coordinates of first corner of square (.dw doesn't work here)
  SquareCorner1_y               db
  SquareCorner1_x               db
  SquareCorner2                 dsb 0 ; $c070 Coordinates of second corner of square
  SquareCorner2_y               db
  SquareCorner2_x               db
  SquareHeight                  db ; $c072 Height of square in pixels
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
RAM_Pen_Smoothed_Previous                   instanceof XY ; $c031 A backup? Not sure how that makes sense
RAM_PenY                                    db ; $C033
RAM_Pressure                                db ; $C034 - never used
RAM_unusedC035                              dsb 7
RAM_CurrentMode                             db ; $C03C Current "mode". High bit set when just changed? Low 6 bits are mode number.
RAM_SelectedNextMode                        db ; $C03D
RAM_Pen_InMenus                             instanceof XY ; $C03E The pen position while in menus
RAM_unusedC040                              dsb 2
RAM_Palette                                 dsb 17 ; $C042
RAM_ColourSelectionStartValue               db ; $c053 Palette value for start of 8-colour palette used when choosing new colours
RAM_NeedToUpdatePalette                     db ; $c054 Non-zero when we need to update the palette - in colour mode only
RAM_unusedC055                              dsb 13
RAM_DrawingData                             instanceof DrawingData ; $c062 A bunch of variables, referenced using index registers starting at this address
RAM_TileModificationBuffer                  dsb 15 ; $C073 ; 4b+?
RAM_MenuShowing                             db ; $c082 High bit is set when a menu has been drawn into the tiles
RAM_CurrentCursorIndex                      db ; $C083 Low bits are the cursor index, high bits are ??? TODO
RAM_CurrentCursorDataAddress                dw ; $C084 Pointer to cursor tile data
RAM_CursorColourCycle_Delay                 db ; $C086 Counter for frame between colour changes
RAM_CursorColourCycle_Index                 db ; $C087 Current colour index, must be following previous
RAM_ButtonStateShownOnScreen                db ; $C088 Holds the button bits as last drawn to the screen
RAM_ActionStateFlags                        db ; $c089 Bits indicate the phase of drawing. Bit 0 unset = step 1, set = step 2. Bit 1 set = doing it.
RAM_PenStyle                                db ; $c08a 0-2 = thin, medium, wide; 3 = erase
RAM_unusedC08B dsb 2
RAM_Pen_Smoothed_Backup                     instanceof XY ; $c08d Backup of value for switching modes. When the pen can go off the screen, this is used to remember where it was while manipulating menus, allowing you go go back to where you were (to some extent).
RAM_Pen_Smoothed_Previous_Backup            instanceof XY ; $c08f See RAM_Pen_Smoothed_Backup
RAM_PaintStartingPoint                      instanceof XY ; $c091 Point where paint was invoked
RAM_unusedC093 dsb 15
RAM_EllipseCurrentX                         dw ; $c0a2 Circle current point x
RAM_EllipseCurrentY                         dw ; $c0a4 Circle current point y
RAM_EllipseLastPointError                   dw ; $c0a6 Part of circle drawing algorithm
RAM_EllipseRatio                            dw ; $c0a8 Fixed-point ellipse squashed-ness factor - $0100 = a circle
RAM_CircleEllipseCentre                     .dw ; $c0aa X, Y coordinate of the centre of a circle or ellipse
RAM_FloodFillXY                             instanceof XY ; $c0aa X,Y coordinates used during flood fills
RAM_EllipseMinorRadius                      .db ; $c0ac Circle radius, or minor radius for ellipse
RAM_FloodFill_StackCounter                  dw ; $c0ac Counter for pushed X,Y pairs that we need to come back to
RAM_FloodFill_PreviousPixelAboveMatches     db ; $c0ae 0 if the pixel at (x-1, y-1) is the right colour, 1 otherwise
RAM_FloodFill_PreviousPixelBelowMatches     db ; $c0af 0 if the pixel at (x-1, y+1) is the right colour, 1 otherwise
RAM_FloodFill_PixelAboveMatches             db ; $c0b0 0 if the pixel at (x, y-1) is the right colour, 1 otherwise
RAM_FloodFill_PixelBelowMatches             db ; $c0b1 0 if the pixel at (x, y+1) is the right colour, 1 otherwise
RAM_SelectedPixelColour                     db ; $c0b2 Selected colour (palette index) in paint mode
RAM_unusedC0b3 dsb 7
RAM_SubmenuSelectionIndex                   db ; $c0ba Index of item last selected in a submenu
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
RAM_unusedC0CA dsb 147 ; Is used... a buffer of some sort?
RAM_c15d db ; $c15d
RAM_c15e db ; $c15e
RAM_c15f db ; $c15f
RAM_c160 db ; $c160
RAM_c161 db ; $c161
RAM_c162 db ; $c162
RAM_TitleScreenAndEndTimeout             dw ; $C163 ; 2b Counter for title screen (loops itself every 8.5s) and when you choose "end" (2.1s, first byte only)
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
