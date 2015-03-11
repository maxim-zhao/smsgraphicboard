.define DRAWING_AREA_WIDTH_PIXELS 176 ; $b0
.define DRAWING_AREA_HEIGHT_PIXELS 144 ; $90
.define DRAWING_AREA_WIDTH_TILES DRAWING_AREA_WIDTH_PIXELS / 8 ; $16 = 22
.define DRAWING_AREA_HEIGHT_TILES DRAWING_AREA_HEIGHT_PIXELS / 8 ; $12 = 18
.define DRAWING_AREA_TOTAL_TILES DRAWING_AREA_WIDTH_TILES * DRAWING_AREA_HEIGHT_TILES
.define DRAWING_AREA_TOTAL_BYTES DRAWING_AREA_TOTAL_TILES * SizeOfTile
.define DRAWING_AREA_MIN_X_PIXELS 36 ; $24
.define DRAWING_AREA_MAX_X_PIXELS DRAWING_AREA_MIN_X_PIXELS + DRAWING_AREA_WIDTH_PIXELS ; $d4 = 212
.define DRAWING_AREA_MIN_Y_PIXELS 60 ; $3c
.define DRAWING_AREA_MAX_Y_PIXELS DRAWING_AREA_MIN_Y_PIXELS + DRAWING_AREA_HEIGHT_PIXELS ; $cc = 204

.define MAGNIFY_SIZE_PIXELS 32

; Cursor indices
.define SetCursorIndex_Second 1<<5 ; Bitmask on cursor index to indicate to set the 2nd cursor
.enum 0
CursorIndex_Crosshair:         db ; Drawing cursor
CursorIndex_PaletteSelect:     db ; Used when selecting stuff up top
CursorIndex_Square:            db ; Used when sleecting a colour in the menu
CursorIndex_MenuArrowRight:    db ; Used in menu
CursorIndex_ArrowTopLeft:      db ; Used for defining one corner of something rectangular
CursorIndex_ArrowBottomRight:  db ; Used for defining opposite corner of something rectangular
CursorIndex_ArrowDown:         db ; Used for defining H-flip axis
CursorIndex_ArrowRight:        db ; Used for defining V-flip axis
CursorIndex_ZoomedPixel:       db ; Used to show snapped pixel when in Zoom mode
CursorIndex_X:                 db ; Used for defining points for circles/ellipses
.ende

; Pen styles
.enum 0
PenStyle_Thin db
PenStyle_Medium db
PenStyle_Thick db
PenStyle_Erase db
.ende

; Indices of pen tiles (relative to first one)
.enum 0
PenTile_Thin_Off      db
PenTile_Thin_On       db
PenTile_Medium_Off    db
PenTile_Medium_On     db
PenTile_Thick_Off     db
PenTile_Thick_On      db
PenTile_Erase_Off     db
PenTile_Erase_On      db
PenTile_DotMode_Off   db
PenTile_DotMode_On    db
.ende
; A macro for addressing the data for them
.macro LD_HL_PEN_TILE_GRAPHICS args tileIndex
  ld hl, PenTiles + SizeOfTile/2 * tileIndex
.endm

; Values used for RAM_CurrentMode. This is mostly used for determining the "mode" we are in, which includes both submenus and root menu items.
.enum 0
Mode0_Drawing                 db
Mode1_Menu                    db
Mode2_MenuItemSelected        db
Mode3_Colour                  db
Mode4_Erase                   db
Mode5_Square                  db
Mode6_Circle                  db
Mode7_Ellipse                 db
Mode8_Paint                   db
Mode9_Copy                    db
Mode10_Mirror                 db
Mode11_Magnify                db
Mode12_Display                db
Mode13_End                    db
Mode14_LinePaintMenu          db
Mode15_ColourSelectionMenu    db
Mode16_MirrorAxisMenu         db
Mode17_EraseConfirmationMenu  db
.ende

; Bits in RAM_VBlankFunctionControl. They control behaviour during VBlank.
.enum 0
VBlankFunctionControl_DrawingUIEnabled db
VBlankFunctionControl_ReadGraphicBoard db
VBlankFunctionControl_TitleScreen_UpdateText db
VBlankFunctionControl_bit3 db ; Unused
VBlankFunctionControl_bit4 db
VBlankFunctionControl_bit5 db
VBlankFunctionControl_bit6 db
VBlankFunctionControl_TitleScreen db
.ende

; Tile indices (in drawing mode)
.enum 0
TileIndex_DrawingArea dsb DRAWING_AREA_TOTAL_TILES
wtf db
TileIndex_ControlTilesStart .db ; $18d
TileIndex_Palette0  db ; $18d..$19c
TileIndex_Palette1  db ; <-- Used with second palette for blanks during colour selection
TileIndex_Palette2  db
TileIndex_Palette3  db
TileIndex_Palette4  db ; <-- Next 8 used with second palette for options during colour selection
TileIndex_Palette5  db
TileIndex_Palette6  db
TileIndex_Palette7  db
TileIndex_Palette8  db
TileIndex_Palette9  db
TileIndex_Palette10 db
TileIndex_Palette11 db
TileIndex_Palette12 db
TileIndex_Palette13 db
TileIndex_Palette14 db
TileIndex_Palette15 db
TileIndex_ThinPen  db ; $19d
TileIndex_MediumPen db
TileIndex_ThickPen db
TileIndex_Erase db
TileIndex_LinesOrDots db
TileIndex_TopLeftCorner db ; $1a2
TileIndex_RightEdge db
TileIndex_BottomEdge db
TileIndex_Cursor_VerticalLine db ; $1a5
TileIndex_Cursor_HorizontalLine db ; $1a6
TileIndex_Cursor1 db ; $1a7
TileIndex_Cursor2 db ; $1a8
TileIndex_Cursor3 db ; $1a9
TileIndex_Menu dsb 3 ; $1aa..c
TileIndex_Do   dsb 3 ; $1ad..f
TileIndex_Pen  dsb 3 ; $1b0..2
TileIndex_Message dsb 13 ; $1b3..f
.ende
