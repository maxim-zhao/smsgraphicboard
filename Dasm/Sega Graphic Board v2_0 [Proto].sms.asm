.memorymap
slotsize $8000
slot 0 $0000
defaultslot 0
.endme
.rombankmap
bankstotal 1
banksize $8000
banks 1
.endro

;.define BypassDetection

.emptyfill $ff

.include "definitions.asm"
.include "ram.asm"
.include "macros.asm"

; Early definitions of some stuff needed later...

.define SetCursorIndex_Second 1<<5 ; Bitmask on cursor index to indicate to set the 2nd cursor
; Cursor indices. WLA gets confused if we don't define them now, if they're used in arithmetic later.
.enum 0
CursorIndex_Crosshair:         db ; Drawing cursor
CursorIndex_PaletteSelect:     db ; Used when selecting stuff up top
CursorIndex_Square:            db ; Unused?
CursorIndex_MenuArrowRight:    db ; Used in menu
CursorIndex_ArrowTopLeft:      db ; Used for defining one corner of something rectangular
CursorIndex_ArrowBottomRight:  db ; Used for defining opposite corner of something rectangular
CursorIndex_ArrowDown:         db ; Used for defining H-flip axis
CursorIndex_ArrowRight:        db ; Used for defining V-flip axis
CursorIndex_ZoomedPixel:       db ; Used to show snapped pixel when in Zoom mode
CursorIndex_X:                 db ; Used for defining points for circles/ellipses
.ende

.enum 0
PenStyle_Thin db
PenStyle_Medium db
PenStyle_Thick db
PenStyle_Erase db
.ende

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
.macro LD_HL_PEN_TILE_GRAPHICS args tileIndex
  ld hl, PenTiles + SizeOfTile/2 * tileIndex
.endm

.enum 0
Mode0_Drawing db ; Drawing
Mode1_Menu db
Mode2_MenuItemSelected db ; Process menu selection?
Mode3_Colour db
Mode4_Erase db
Mode5_Square db
Mode6_Circle db
Mode7_Ellipse db
Mode8_Paint db
Mode9_Copy db
Mode10_Mirror db
Mode11_Magnify db
Mode12_Display db
Mode13_End db
Mode14_LinePaintMenu db
Mode15_ColourSelectionMenu db
Mode16_MirrorAxisMenu db
Mode17_EraseConfirmationMenu db
.ende

.define ModeHighBit 1<<7

.enum 0
VBlankFunctionControl_DrawingUIEnabled db
VBlankFunctionControl_ReadGraphicBoard db
VBlankFunctionControl_TitleScreen_UpdateText db
VBlankFunctionControl_bit3 db ; Unused?
VBlankFunctionControl_bit4 db
VBlankFunctionControl_bit5 db
VBlankFunctionControl_bit6 db
VBlankFunctionControl_TitleScreen db
.ende

.bank 0 slot 0
.org $0000
.section "Boot" force
FullReset:
    jp Start
.ends

.dsb 5, 0 ; 5 bytes blank

.org $0008
.section "rst $08" force
VDPAddressToDE:
    ld a, e
    out (Port_VDPAddress), a
    ld a, d
    out (Port_VDPAddress), a
    ret
.ends

.section "Helper function" force ; Could align it to a rst to save four bytes (!)
JumpToFunction:
    ; Look up a'th entry in table at hl, and jump to it
    add a, a
    LD_DE_A
    add hl, de
    ld a, (hl)
    inc hl
    ld h, (hl)
    ld l, a
    jp (hl)
.ends

.db "PROGRAM By K.WAKIHARA"

.org $0038
.section "Interrupt handler" force
    jp InterruptHandlerImpl
.ends

.section "VDP register intialisation data" force
VDPRegisterValues: ; $003b
.db VDPR0B0_VideoSync_ON | VDPR0B1_ExtraHeightModes_ON | VDPR0B2_SMSMode_ON | VDPR0B3_SpriteShift_OFF | VDPR0B4_LineInterrupts_OFF | VDPR0B5_BlankLeftColumn_OFF | VDPR0B6_FixTop2Rows_OFF | VDPR0B7_FixRight8Columns_OFF
.db VDPR1B0_ZoomedSprites_OFF | VDPR1B1_DoubledSprites_OFF | VDPR1B2 | VDPR1B3_30RowMode_OFF | VDPR1B4_28RowMode_OFF | VDPR1B5_VBlankInterrupts_ON | VDPR1B6_EnableDisplay_OFF | VDPR1B7
.db $ff, $ff ; unused regs
.db (TileMapAddress>>10)   |%11110001
.db (SpriteTableAddress>>7)|%10000001
.db (SpriteSet<<2)         |%11111011
.db 0 ; Border palette colour (sprite palette) (unused bits not set)
.db 0 ; Horizontal scroll
.db 0 ; Vertical scroll
.db 0 ; Line interrupt spacing ($ff to disable)
.ends

.section "Screen on/off control" force
ScreenOff:
    ld a, (RAM_VDPReg1Value)
    and (1<<6) ~ $ff  ; unset bit 6 = screen off
    jp +

ScreenOn:
    ld a, (RAM_VDPReg1Value)
    or 1<<6 ; set bit 6 = screen on
+:  ld (RAM_VDPReg1Value), a
    ld e, a
    ld d, $81
    jp VDPAddressToDE
.ends

.org $0066
.section "Pause handler" force
NMIHandler:
    retn
.ends

.section "Entry point" force
Start:
    di
    im 1

    ; Zero all of RAM
    ld hl, $C000
    ld de, $C001
    ld bc, $1FFF
    ld (hl), 0
    ldir

Start_AfterRAMClear:
    ld sp, $DFFE
    call SilencePSG
    call DelayLoop1
    call InitialiseVDPRegisters
    call FillNameTableWithTile9

    ld a, IO_TR1_IN | IO_TH1_IN | IO_TR2_IN | IO_TH2_IN ; $FF ; all inputs
    out (Port_IOPortControl), a

    ei
    ld a, 1<<VBlankFunctionControl_DrawingUIEnabled
    call SetVBlankFunctionAndWait

    di
    call TitleScreen

    ; Zero tiles
    LD_DE_TILE 0
    ld bc, 448*SizeOfTile
    ld h, 0
    call FillVRAMWithH

    ; Set up screen

    ld hl, ControlTiles ; data: tiles for palette, UI controls
    LD_DE_TILE $18d
    call DecompressGraphics

    LD_DE_TILE $1b3
    ld b, 13 ; 13 tiles
    ld hl, Font2bpp ; Will only use the first tile, which is blank
    call FillTiles2bpp

    LD_DE_TILEMAP 0, 0
    ld hl, $8D09 ; tile $18d = background, tile palette index 1
    ld bc, 32*28 ; $0380
    call FillVRAMWithHL

    call DrawUIControls
    call SetDrawingAreaTilemap

    ld hl, DrawingPalette
    ld de, RAM_Palette
    ld bc, 17
    ldir
    ld hl, DrawingPalette
    LD_DE_PALETTE 0
    ld bc, 32
    call RawDataToVRAM
    ; Blank ???
    ld hl, $C15D
    ld de, $C15E
    ld bc, 8
    ld (hl), 0
    ldir

    ; Blank the upper 32KB of ROM space - which doesn't exist
    ; Maybe a holdover from an original design to use RAM for the bitmap?
    ; Maybe just to clear out the RAM devcart, to make sure it's not used?
    ; Maybe a sneaky way to make a 32KB RAM cart kill itself?
    ; Maybe a holdover from a time when you could save your images to save RAM?
    ld a, $02
    ld ($FFFF), a
    ld hl, $8000
    ld de, $8001
    ld bc, $3FFF
    ld (hl), 0
    ldir
    ; Then another 16KB
    ld a, $03
    ld ($FFFF), a
    ld hl, $8000
    ld de, $8001
    ld bc, $3FFF
    ld (hl), 0
    ldir

    ; Some write-only stuff...
    ld hl, $8000
    ld (RAM_UnknownWriteOnlyC183), hl
    ld (RAM_UnknownWriteOnlyC187), hl
    ld a, $01
    ld (RAM_UnknownWriteOnlyC182), a

    call InitialiseCursorSprites
    call ScreenOn
    LD_HL_LOCATION 88, 72
    ld (RAM_Pen_InMenus), hl

    ld a, 1
    ld (RAM_PenMode.IsSet), a

    ; Main loop
-:  ei
    ld a, 1<<VBlankFunctionControl_DrawingUIEnabled + 1<<VBlankFunctionControl_ReadGraphicBoard
    call SetVBlankFunctionAndWait
    call CallNonVBlankModeGraphicBoardHandler
    call CallModeDrawingFunction
    call SpriteTable1to2
    jp -
.ends

; Various functions for manipulating graphics
.section "Graphics helpers 1" force
InitialiseVDPRegisters:
    ld hl, VDPRegisterValues ; VDP register data
    ld b, 11 ; counter
    ld c, $80 ; register number + reg bits
-:  ld a, (hl)
    out (Port_VDPAddress), a
    inc hl
    ld a, c
    out (Port_VDPAddress), a
    inc c
    djnz -

    ; Save reg 1 value to RAM
    ld a, (VDPRegisterValues+1)
    ld (RAM_VDPReg1Value), a

    ld de, VDPAddressMask_Write | $0000 ; start of VRAM
    ld h, 0
    ld bc, SizeOfVRAM
    call FillVRAMWithH
    jp DisableSprites_RAMAndVRAM

FillNameTableWithTile9:
    LD_DE_TILEMAP 0, 0
    ld bc, 32*28 ; Entry count
    ld hl, $0009 ; Date to write
    ; fall through

FillVRAMWithHL:
    VDP_ADDRESS_TO_DE
-:  ld a, h
    out (Port_VDPData), a
    push af
      ld a, l
      out (Port_VDPData), a
    pop af
    dec bc
    ld a, b
    or c
    jp nz, -
    ret

DisableSprites_RAMAndVRAM:
    ld hl, RAM_SpriteTable1.y
    ld de, RAM_SpriteTable1.y + 1
    ld bc, 64-1
    ld (hl), SpriteTableYTerminator
    ldir
    ld hl, RAM_SpriteTable2.y
    ld de, RAM_SpriteTable2.y + 1
    ld bc, 64-1
    ld (hl), SpriteTableYTerminator
    ldir
DisableSprites_VRAM:
    LD_DE_SPRITE_TABLE_Y 0
    ld h, SpriteTableYTerminator
    ld bc, 64
    ; fall through

FillVRAMWithH:
    ; Write h to VRAM address de, bc times
    VDP_ADDRESS_TO_DE
-:  ld a, h
    out (Port_VDPData), a
    dec bc
    ld a, b
    or c
    jp nz, -
    ret

RawDataToVRAM:
    ; write bc bytes from hl to VRAM address de
    VDP_ADDRESS_TO_DE
    ld a, c
    or a
    jr z, +
    inc b ; for multiples of 256, correct incorrect loop count
+:  ld a, b
    ld b, c
    ld c, Port_VDPData
-:  outi
    push af ; delay
    pop af
    jp nz, - ; loop over b
    dec a
    jp nz, - ; then loop over a
    ret

RawDataToVRAM_Interleaved1:
    ; copy b bytes from hl to VRAM address de, interleaving with a
    ld (RAM_VRAMFillHighByte), a
    ; fall through

RawDataToVRAM_Interleaved2:
    ; copy b bytes from hl to VRAM address de, interleaving with value in RAM_VRAMFillHighByte
    VDP_ADDRESS_TO_DE
    ld c, Port_VDPData
-:  outi
    push af
      ld a, (RAM_VRAMFillHighByte)
      out (c), a
    pop af
    jp nz, -
    ret

Write1bppToVRAMWithExtensionMask:
    ; Unused function
    ; Inputs:
    ; a = bitmask. For the low 4 bits, a 1 will cause the data at hl to be written to VRAM and a 0 gives a 0.
    ; bc = count
    ; de = dest VRAM address
    ; hl = source
    ; This acts to take some 1bpp data and extend it up to 4bpp using the supplied bitmask.
    ld (RAM_Write1bppToVRAMWithExtensionMask_Mask), a ; save the bitmask
    VDP_ADDRESS_TO_DE
--: ld a, (hl) ; Read a byte
    exx
      ld c, Port_VDPData
      ld b, 4 ; Counter
      ld h, a ; Hold read byte
      ld a, (RAM_Write1bppToVRAMWithExtensionMask_Mask)
-:    rra
      ld d, h
      jr c, +
      ld d, 0
+:    out (c), d
      djnz -
    exx
    inc hl
    dec bc
    ld a, b
    or c
    jp nz, --
    ret

FillTiles2bpp:
    ; hl = source
    ; de = dest
    ; b = tile count
    VDP_ADDRESS_TO_DE
FillTiles2bppCurrentAddress:
    ; hl = source
    ; b = tile count
--: push hl
    push bc
      ld b, 16
      ld c, Port_VDPData
-:    xor a
      outi
      push de
      pop de
      outi
      push de
      pop de
      out (c), a
      push de
      pop de
      out (c), a
      push de
      pop de
      jp nz, -
    pop bc
    pop hl
    djnz --
    ret

Write2bppToVRAMSlowly:
    ; Write data from hl to VDP address de, interleaving with zeroes
    ; Writes b tiles
    ; Trashes c, a
    ; 37 cycles between each write so safe for use during active display
    VDP_ADDRESS_TO_DE
--: push bc
      ld b, $10 ; counter: 16 bytes data + 16 bytes zero = 1 tile
      ld c, Port_VDPData
-:    xor a
      outi    ; copy 1 byte
      push de ; delay
      pop de
      outi    ; copy 1 byte
      push de ; delay
      pop de
      out (c), a ; zero
      push de ; delay
      pop de
      out (c), a ; zero
      push de ; delay
      pop de
      jp nz, -
    pop bc
    djnz --
    ret

WriteAreaToTilemap_1byte:
    ; write data from hl to tilemap, with high byte from RAM_VRAMFillHighByte
    ; data is c tiles wide, b tiles tall
--: push bc
      VDP_ADDRESS_TO_DE
      ld b, c
      ld c, Port_VDPData
-:    outi
      push af
        ld a, (RAM_VRAMFillHighByte)
        out (c), a
      pop af
      jp nz, -
      ex de, hl
      ld bc, $0040
      add hl, bc
      ex de, hl
    pop bc
    djnz --
    ret

WriteAreaToTilemap:
    ; Write data from hl to VRAM address de, with delays. Write c bytes then skip forward by 64 bytes and repeat b times.
    ; This we fill a tilemap area c bytes x b rows.
--: push bc
      VDP_ADDRESS_TO_DE
      ld b, c
      ld c, Port_VDPData
-:    outi
      push af
      pop af
      jp nz, -
      ex de, hl
      ld bc, $0040
      add hl, bc
      ex de, hl
    pop bc
    djnz --
    ret

CopyVRAMToRAM:
    ; read b*c bytes from VRAM address de and write to hl
    ; if b=0, acts as if b=1
    VDP_ADDRESS_TO_DE
    push af ; delay
    pop af
--: push bc
      ld b, c
      ld c, Port_VDPData
-:    ini ; read a byte to hl, --b
      push af ; delay
      pop af
      jr nz, -
    pop bc
    ld a, b
    or a
    ret z
    djnz --
    ret

SetAreaTileAttributes:
    ; Modify the tilemap data in VRAM to have the given tile attributes
    ; h = 0000pcvh0 (priority, palette, v-flip, h-flip)
    ; Parameters:
    ; de = VRAM address
    ; b = rows
    ; c = columns
    ; h = bitmask
--: push bc
      ld b, c ; c = width
      push de
-:      VDP_ADDRESS_TO_DE
        ex (sp), hl ; delay
        ex (sp), hl
        in a, (Port_VDPData) ; Read a byte
        ex af, af' ; save value
        ex (sp), hl ; delay
        ex (sp), hl
        ; re-set address with write bit set
        ld a, e
        out (Port_VDPAddress), a
        ld a, d
        or >VDPAddressMask_Write
        out (Port_VDPAddress), a
        ex af, af' ; restore value
        and %11100001 ; Zero some bits
        or h ; Set some bits from h
        push af ; delay
        pop af
        out (Port_VDPData), a ; Write it back
        inc de ; Move on two bits
        inc de
        djnz -
      pop de
      push hl
        ld hl, 64
        add hl, de
        ex de, hl
      pop hl
    pop bc
    djnz --
    ret

Fill1bppWithBitmaskToTilesColumn:
    ; Unused function
    ; de = tilemap address
    ; c = number of tiles
    ; h = 4-bit bitmask for bitplanes to write to
    ; l = data to write for selected bitplanes
-:  push bc
    push hl
      LD_HL_C      ; bc = c * 8
      add hl, hl
      add hl, hl
      add hl, hl
      ld b, h
      ld c, l
    pop hl
    call +
    push hl
      ld hl, 22 * SizeOfTile ; Presumably 22 is the "stride"? TODO
      add hl, de
      ex de, hl
    pop hl
    pop bc
    djnz -
    ret

+:  VDP_ADDRESS_TO_DE
--: push hl
    push bc
      ld b, 4      ; Counter
-:    rrc h       ; Rotate a bit into carry
      ld a, l      ; 1 = use l, 0 = use 0
      jp c, +
      xor a
+:    out (Port_VDPData), a
      nop         ; delay
      djnz -
    pop bc
    pop hl
    dec bc
    ld a, b
    or c
    jp nz, --
    ret

DecompressGraphics:
    ld b, 4 ; bitplane count
-:  push bc
      push de
        call DecompressBitplane
      pop de
      inc de
    pop bc
    djnz -
    ret

DecompressBitplane:
--: ld a, (hl)
    inc hl
    or a
    ret z
    ld c, a
    and %01111111
    ld b, a
    ld a, c
    and %10000000
-:  VDP_ADDRESS_TO_DE
    ld a, (hl)
    out (Port_VDPData), a
    push af
    pop af
    jp z, +
    inc hl
+:  inc de
    inc de
    inc de
    inc de
    djnz -
    jp nz, --
    inc hl
    jp --
.ends

.section "Sprite table handlers including flicker helper" force
CopySpriteTable2ToVRAM:
    ld a, (RAM_SpriteTable2DirtyFlag)
    or a
    ret z ; Do nothing if not dirty
    xor a
    ld (RAM_SpriteTable2DirtyFlag), a
    ld a, (RAM_FrameCounter) ; ### Immediately discarded...
    LD_DE_SPRITE_TABLE_Y 0 ; Sprite table: Y
    VDP_ADDRESS_TO_DE
    ld hl, RAM_SpriteTable2.y
    ld c, Port_VDPData
    call Outi64
    ld hl, RAM_SpriteTable2.xn
    LD_DE_SPRITE_TABLE_X 0 ; Sprite table: XN
    VDP_ADDRESS_TO_DE
    ; fall though
Outi128:
.rept 64
    outi
.endr
Outi64:
.rept 64
    outi
.endr
    ret

SpriteTable1to2:
    ld a, (RAM_CurrentMode)
    and %00111111
    cp Mode12_Display ; Leave the sprites alone - they're turned off!
    ret z
    ld a, (RAM_FrameCounter)
    rrca ; Check low bit
    jp c, +
    ; Even: straight copy
    ld hl, RAM_SpriteTable1.y
    ld de, RAM_SpriteTable2.y
    ld bc, 64
    ldir
    ld hl, RAM_SpriteTable1.xn
    ld de, RAM_SpriteTable2.xn
    ld bc, 64*2
    ldir
    ld a, 1
    ld (RAM_SpriteTable2DirtyFlag), a
    ret

+:  ; Odd: reverse order
    ld hl, RAM_SpriteTable1.y + 63
    ld de, RAM_SpriteTable2.y
    ld b, 64
-:  ld a, (hl)
    ld (de), a
    dec hl
    inc de
    djnz -
    ld hl, RAM_SpriteTable1.xn + 63 * 2
    ld de, RAM_SpriteTable2.xn
    ld b, 64
-:  ld a, (hl)
    ld (de), a
    inc hl
    inc de
    ld a, (hl)
    ld (de), a
    inc de
    dec hl
    dec hl
    dec hl
    djnz -
    ld a, $01
    ld (RAM_SpriteTable2DirtyFlag), a
    ret
.ends

.section "Startup delay loop" force
DelayLoop1:
    ; Delay (including call) = 3145810 cycles = ~879ms
    ld e, $02
--: ld bc, $0000
-:  dec bc
    ld a, b
    or c
    jp nz, -
    dec e
    jp nz, --
    ret
.ends

.section "UI initialisatiomn" force
DrawUIControls:
    ld hl, BottomStatusBarTiles ; tilemap data: MENU | DO | PEN | mode text bar
    LD_DE_TILEMAP 4, 21
    LD_BC_TILEMAP_AREA 24, 3
    call WriteAreaToTilemap
    ld hl, TopBarPaletteTiles ; tilemap data: palette and pen mode controls
    LD_DE_TILEMAP 5, 1
    ld bc, 22*SizeOfNameTableEntry ; count
    call RawDataToVRAM
    ld hl, PenControlsBottomLineTilemapData ; tilemap data: top bar status (apparently unused)
    LD_DE_TILEMAP 22, 2
    ld bc, 5*SizeOfNameTableEntry ; count
    jp RawDataToVRAM ; and ret

SetDrawingAreaTilemap:
    ld hl, 0 ; Tilemap data to write - start at index 0 and increment
    LD_BC_TILEMAP_AREA 11, 18
    LD_DE_TILEMAP 5, 3
--: VDP_ADDRESS_TO_DE
    push bc
      ld b, c
      ld c, Port_VDPData
-:    out (c), l ; Emit hl to the tilemap
      push af ; delay
      pop af
      out (c), h
      inc hl ; Next tile
      djnz - ; Loop along row
      push hl
        ld hl, 64 ; Move down one row
        add hl, de
        ex de, hl
      pop hl
    pop bc
    djnz -- ; Loop down rows
    ret
.ends

.section "Sound driver (!)" force
Beep:
    ld a, (RAM_Beep)
    or a
    ret z ; Do nothing while zero

    inc a ; Else make sound for three more frames
    ld (RAM_Beep), a
    cp 5
    jp z, SilencePSG

    ; Set channel 0 to tone $3f = 1775.6Hz = A6
    ld a, PSG_Latch | PSG_Channel0 | PSG_Tone | %1111
    out (Port_PSG), a
    ld a, PSG_Data | %000011 ; $03
    out (Port_PSG), a
    ld a, PSG_Latch | PSG_Channel0 | PSG_Volume | 0
    out (Port_PSG), a
    ret

SilencePSG:
    ld a, PSG_Latch | PSG_Channel0 | PSG_Volume | 15
    out (Port_PSG), a
    ld a, PSG_Latch | PSG_Channel1 | PSG_Volume | 15
    out (Port_PSG), a
    ld a, PSG_Latch | PSG_Channel2 | PSG_Volume | 15
    out (Port_PSG), a
    ld a, PSG_Latch | PSG_Channel3 | PSG_Volume | 15
    out (Port_PSG), a
    cpl ; results in zero - could use "or a" to be clear
    ld (RAM_Beep), a ; Disable counter
    ret
.ends

.section "Drawing-time palettes" force
; Unused palette? Black with blue and white
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
  COLOUR 0, 0, 3
  COLOUR 0, 0, 0
  COLOUR 3, 3, 3
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00

;.org $051d
DrawingPalette:
  COLOUR 3, 3, 3 ; White
  COLOUR 0, 0, 0 ; Black
  COLOUR 1, 0, 0 ; Reds
  COLOUR 2, 0, 0
  COLOUR 3, 0, 0
  COLOUR 0, 1, 0 ; Greens
  COLOUR 0, 2, 0
  COLOUR 0, 3, 0
  COLOUR 0, 0, 1 ; Blues
  COLOUR 0, 0, 2
  COLOUR 0, 0, 3
  COLOUR 0, 2, 3 ; Sky blue
  COLOUR 3, 1, 0 ; Orange
  COLOUR 3, 3, 0 ; Yellows
  COLOUR 3, 3, 1
  COLOUR 3, 3, 2
  ; Sprites
  COLOUR 0, 0, 0 ; Tranparent/black
  COLOUR 0, 0, 0 ; Black for sprites
  COLOUR 3, 3, 3 ; White
  COLOUR 3, 0, 0 ; Red
  COLOUR 0, 0, 0 ; These blacks are used for the colour palette selection later.
  COLOUR 0, 0, 0
  COLOUR 0, 0, 0
  COLOUR 0, 0, 0
  COLOUR 0, 0, 0
  COLOUR 0, 0, 0
  COLOUR 0, 0, 0
  COLOUR 0, 0, 0
  COLOUR 0, 0, 0
  COLOUR 0, 0, 0
  COLOUR 0, 0, 0
  COLOUR 3, 0, 0 ; Cycling colour
.ends

.section "Tilemap data for UI" force
;.org $053d
TopBarPaletteTiles: ; 22x1
.dw $018d, $018e, $018f, $0190, $0191, $0192, $0193, $0194, $0195, $0196, $0197, $0198, $0199, $019a, $019b, $019c ; Palette colours
.dw TileAttribute_Palette2<<8 | $018D ; Unused
.dw TileAttribute_Palette2<<8 | $019D, TileAttribute_Palette2<<8 | $019E, TileAttribute_Palette2<<8 | $019F, TileAttribute_Palette2<<8 | $01A0, TileAttribute_Palette2<<8 | $01A1 ; Pen controls using sprite palette

PenControlsBottomLineTilemapData: ; 5x1
.dw $09A4, $09A4, $09A4, $09A4, $09A4 ; Draw bottom line for buttons

BottomStatusBarTiles: ; 24x3
.dw $09A2, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0BA2 ; .______________________.
.dw $0BA3, $09AA, $09AB, $09AC, $09AD, $09AE, $09AF, $09B0, $09B1, $09B2, $09B3, $09B4, $09B5, $09B6, $09B7, $09B8, $09B9, $09BA, $09BB, $09BC, $09BD, $09BE, $09BF, $09A3 ; |                      | <-- with stuff in it
.dw $0DA2, $09A4, $09A4, $09A4, $09A4, $09A4, $09A4, $09A4, $09A4, $09A4, $09A4, $09A4, $09A4, $09A4, $09A4, $09A4, $09A4, $09A4, $09A4, $09A4, $09A4, $09A4, $09A4, $0FA2 ; '^^^^^^^^^^^^^^^^^^^^^^'
.ends

.section "VBlank handler implementation" force
InterruptHandlerImpl:
    di
    push af
    push bc
    push de
    push hl
    push ix
    push iy
    ex af, af'
    exx
    push af
    push bc
    push de
    push hl
      in a, (Port_VDPStatus) ; Satisy VBlank interrupt
      ld a, (RAM_VBlankFunctionControl)
      or a
      jp z, VBlank_CheckResetAndExit
      bit VBlankFunctionControl_TitleScreen, a
      jp nz, VBlank_TitleScreen ; Bit 7 set -> we are in the title screen, jump to a specialised handler
      rrca
      jp nc, + ; Bit 0 set -> we are in the main program

      ; Regular VBlank
      call CopySpriteTable2ToVRAM
      call UpdateCursorGraphics
      call UpdateCursorColourCycling

      ld a, (RAM_CurrentMode)
      cp ModeHighBit | Mode3_Colour
      jp nz, +
      ld a, (RAM_NeedToUpdatePalette)
      or a
      jp z, +

      ; When RAM_NeedToUpdatePalette is non-zero...
      ; ...for one frame only...
      xor a
      ld (RAM_NeedToUpdatePalette), a

      ; ...update the palette
      ld hl, RAM_Palette
      LD_DE_PALETTE 0
      VDP_ADDRESS_TO_DE
      ld b, 17 ; palette entries
-:    ld a, (hl)
      inc hl
      push af ; Delay
      pop af
      out (Port_VDPData), a
      djnz -

      ; Skip 3 palette entries
      LD_DE_PALETTE 20
      VDP_ADDRESS_TO_DE
      ld a, (RAM_ColourSelectionStartValue) ; value to write
      ld b, 8 ; 8 palette entries
-:    out (Port_VDPData), a
      inc a ; Write incrementing values. These are used for the colour selection screen
      push af ; Delay
      pop af
      djnz -

      call UpdateColourSelectionLabels

+:    ; Increment the frame counter
      ld hl, RAM_FrameCounter
      inc (hl)

      call UpdateStatusBarText
      ld a, (RAM_VBlankFunctionControl)
      bit VBlankFunctionControl_ReadGraphicBoard, a
      jp z, +
      push af
        call ReadGraphicBoard
      pop af
+:    bit VBlankFunctionControl_DrawingUIEnabled, a
      jp z, +
      call UpdateButtonGraphics
      call UpdatePenGraphics
+:    call Beep
      ; fall through
VBlank_CheckResetAndExit:
      xor a
      ld (RAM_VBlankFunctionControl), a
      in a, (Port_IOPort2)
      ld hl, RAM_ResetButton1
      cpl
      and 1<<4 ; check for reset button
      ld c, a
      xor (hl)
      ld (hl), c
      inc hl ; RAM_ResetButton2
      and c
      ld (hl), a
      bit 4, (hl) ; check for reset press
      jp nz, VBlank_HandleReset
    pop hl
    pop de
    pop bc
    pop af
    ex af, af'
    exx
    pop iy
    pop ix
    pop hl
    pop de
    pop bc
    pop af
    ei
    ret

.endasm ; Some pushes to match the pops below - ignore!
push af
push af
push af
push af
push af
push af
push af
push af
push af
push af
.asm

VBlank_HandleReset:
    pop hl
    pop de
    pop bc
    pop af
    ex af, af'
    exx
    pop iy
    pop ix
    pop hl
    pop de
    pop bc
    pop af
    ; Skip interrupt return address
    inc sp
    inc sp
    ; fall through
HandleReset:
    ; Blank RAM, but leave VBlank bytes alone
    ld hl, $C002
    ld de, $C003
    ld bc, $1FFD
    ld (hl), 0
    ldir
    ; Then go to the usual startup code
    jp Start_AfterRAMClear

.ends

.section "Title screen VBlank handler" force
VBlank_TitleScreen:
    ld a, (RAM_VBlankFunctionControl)
    or a
    jp z, VBlank_CheckResetAndExit
    push af
      call TitleScreenAnimationVBlankEntry ; Always update title screen animation
    pop af
    bit VBlankFunctionControl_ReadGraphicBoard, a ; Bit 1 set -> read graphic board
    push af
      call nz, ReadGraphicBoard
    pop af
    bit VBlankFunctionControl_TitleScreen_UpdateText, a ; Bit 2 set -> update title screen text
    call nz, TitleScreenTextUpdate
    jp VBlank_CheckResetAndExit
.ends

.section "VBlank function set and wait" force
SetVBlankFunctionAndWait:
    ld (RAM_VBlankFunctionControl), a
-:  ld a, (RAM_VBlankFunctionControl)
    or a
    jp nz, -
    ret
.ends

.section "Reset button handler" force
CheckForReset:
    push hl
    push af
      in a, (Port_IOPort2)
      ld hl, $C000
      cpl
      and $10
      ld c, a
      xor (hl)
      ld (hl), c
      inc hl
      and c
      ld (hl), a
      bit 4, (hl)
      jp nz, HandleReset
    pop af
    pop hl
    ret
.ends

.section "Graphic board" force
.include "graphicboard.asm"
.ends

.section "Maths 1" force
.include "maths.asm"
.ends

.section "Title screen main loop" force
TitleScreen: ; $865
    ; blank RAM for title screen animation?
    ld hl, $C15D
    ld de, $C15E
    ld bc, 8
    ld (hl), 0
    ldir

    ; Initialise timeout counter
    ld hl, $0200 ; 8533ms
    ld (RAM_TitleScreenAndEndTimeout), hl

    ld hl, TitleScreenFont ; $3C02 ; compressed tile data: font
    LD_DE_TILE 0
    call DecompressGraphics

    ld h, 0
    LD_DE_TILE 256
    ld bc, 192 * 32 ; $1800 ; 192 tiles (up to name table)
    call FillVRAMWithH

    ld hl, Tiles_SegaLogo ; $14CA ; compressed tile date: Sega logo
    LD_DE_TILE 400
    call DecompressGraphics

    ld hl, Palette_TitleScreen
    LD_DE_PALETTE 0 ; Tile palette
    ld bc, 7 ; Count
    call RawDataToVRAM

    ld hl, Palette_Logo ; $0C2A
    LD_DE_PALETTE 16 ; Sprite palette
    ld bc, 8 ; Count
    call RawDataToVRAM

    in a, (Port_IOPort1)
    and %11101111 ; $EF ; Ignore TL
    cp  %11100000 ; $E0 ; Expect UDLR lines low, rest high
    ld a, 1<<VBlankFunctionControl_TitleScreen + 1<<VBlankFunctionControl_bit3 + 1<<VBlankFunctionControl_ReadGraphicBoard ; $8A
    jp z, +
    ld a, 1<<VBlankFunctionControl_TitleScreen + 1<<VBlankFunctionControl_bit3
+:  ld ($C15D), a ; gets either $8a or $88 accordingly

    ld b, $00   ; Draw into tilemap for splash screen animation
    ld c, $23
-:  push bc
      call UpdateSplashScreenAnimationTilesLine
    pop bc
    inc b
    dec c
    jp p, -

    ld a, $1F
    ld ($C160), a

    call ScreenOn

    ei
-:  ld a, ($C15D)
    call SetVBlankFunctionAndWait

    ld a, (RAM_ButtonsNewlyPressed)
    and %0000111 ; any button
    jp nz, TitleScreenButtonPressed

    ; No button pressed
    ; Update title screen state?
    ld hl, - ; push loop address
    push hl
      ld ix, $C15D
      bit 0, (ix+1) ; $c15e
      jp z, TitleScreenAnimate_Bit0Zero ; Blinds slide animation
      bit 1, (ix+1) ; $c15e
      jp z, TitleScreenAnimate_Bit1Zero ; Pixel slide/flip animation
    inc sp ; Discard loop address - could have popped it...
    inc sp
    di

; Matching push above, ignore
.endasm
pop hl
.asm

TitleScreen_PostAnimationLoop:
    ld hl, $14A2 ; data: Sega logo tilemap data
    LD_DE_TILEMAP 11, 3
    ld bc, $040A ; 10x4
    ld a, $01
    ld (RAM_VRAMFillHighByte), a
    call WriteAreaToTilemap_1byte

    ld hl, Text_CopyrightSega1987 ; $0A08 ; data: (c) Sega 1987
    LD_DE_TILEMAP 10, 22
    ld b, $0C
    xor a
    call RawDataToVRAM_Interleaved1
    call ScreenOn
    ei

CheckForGraphicsBoard:
    in a, (Port_IOPort1)
    and $EF
    cp $E0
.ifdef BypassDetection
    jp GraphicsBoardDetected
.else
    jp z, GraphicsBoardDetected
.endif

    ld hl, Text_NotGraphicBoard ; $09E8 ; Data: "NOT GRAPHIC BOARD !!"
    ld (RAM_TitleScreenTextPointer), hl
    LD_DE_TILEMAP 6, 16
    ld (RAM_TitleScreenTextLocation), de
    ld bc, 20 | (12<<8)
    ld (RAM_TitleScreenTextLength), bc ; also sets RAM_TitleScreenTextFlashSpeed
    ld a, 1<<VBlankFunctionControl_TitleScreen + 1<<VBlankFunctionControl_TitleScreen_UpdateText
    call SetVBlankFunctionAndWait

    ; decrement timeout counter
    ld hl, (RAM_TitleScreenAndEndTimeout)
    dec hl
    ld (RAM_TitleScreenAndEndTimeout), hl
    ld a, l
    or h
    jp z, TitleScreenTimedOut
    jp CheckForGraphicsBoard

GraphicsBoardDetected:
    ld hl, Text_PushButton ; $09FC ; Data: "PUSH  BUTTON"
    ld (RAM_TitleScreenTextPointer), hl
    LD_DE_TILEMAP 10, 16
    ld (RAM_TitleScreenTextLocation), de
    ld bc, 12 | (32 << 8)
    ld (RAM_TitleScreenTextLength), bc ; also sets RAM_TitleScreenTextFlashSpeed
-:  ld a, 1<<VBlankFunctionControl_TitleScreen + 1<<VBlankFunctionControl_TitleScreen_UpdateText + 1<<VBlankFunctionControl_ReadGraphicBoard
    call SetVBlankFunctionAndWait

    ; check the board again
    in a, (Port_IOPort1)
    and $EF
    cp $E0
.ifdef BypassDetection
    jp z, CheckForGraphicsBoard
.else
    jp nz, CheckForGraphicsBoard
.endif

    ; decrement title screen counter again
    ld hl, (RAM_TitleScreenAndEndTimeout)
    dec hl
    ld (RAM_TitleScreenAndEndTimeout), hl
    ld a, l
    or h
    jp z, TitleScreenTimedOut

    ; check button presses from last read
    ld a, (RAM_ButtonsNewlyPressed)
    and %00000111 ; any button
    jp z, - ; loop until pressed

    ld a, 1
    ld (RAM_Beep), a
    di
    jp ScreenOff ; and return

TitleScreenTimedOut:
    di
    call ScreenOff
    call FillNameTableWithTile9
    jp TitleScreen

TitleScreenButtonPressed:
    di
    call ScreenOff
    call FillNameTableWithTile9
    ld hl, Tiles_Logo ; $0C32 ; Data
    LD_DE_TILE $101
    ld b, 135 ; $87 ; 135 tiles
    call Write2bppToVRAMSlowly
    LD_DE_TILEMAP 0, 8
    ld hl, Tilemap_Logo ; $0B8A
    ld bc, $0520 ; 5 rows, 32 columns
    ld a, $09
    ld (RAM_VRAMFillHighByte), a
    call WriteAreaToTilemap_1byte
    jp TitleScreen_PostAnimationLoop

TitleScreenTextUpdate:
    ld hl, RAM_TitleScreenTextFlashCounter ; Decrement counter
    dec (hl)
    ret p ; Wait for it to go below zero
    ld a, (RAM_TitleScreenTextFlashSpeed) ; Reset value
    ld (hl), a

    inc hl ; Then go to the following byte (RAM_TitleScreenTextFlashState)
    ld a, (hl)
    xor 1 ; Flip its LSB
    ld (hl), a
    jp z, +

    ; If 1, draw the text
    ld de, (RAM_TitleScreenTextLocation)
    ld a, (RAM_TitleScreenTextLength)
    ld b, a
    ld hl, (RAM_TitleScreenTextPointer)
    xor a ; high byte
    jp RawDataToVRAM_Interleaved1

+:  ; If 0, blank the text area
    LD_DE_TILEMAP 6, 16
    ld bc, 20 ; 20 tiles
    ld hl, 9
    jp FillVRAMWithHL ; and ret
.ends

.section "Title screen data" force
; This mapping only applies to the title screen font.
.asciitable
map ' ' = 0
map '0' to '9' = 1
map 'A' to 'Z' = 11
map '!' = 38
map '#' = 39 ; (c)
.enda
Text_NotGraphicBoard: ; $9e8
.asc "NOT GRAPHIC BOARD !!"
Text_PushButton: ; $9fc
.asc "PUSH  BUTTON"
Text_CopyrightSega1987: ; $a08
.asc "# SEGA  1987"

;.org $0a14
Palette_TitleScreen:
.db %00010000 ; dark blue
.db %00000000 ; Black
.db %00111111 ; White
.db %00000000 ; Black
.db %00110000 ; Bright blue
.db %00000000 ; Black
.db %00111111 ; White
.ends

.section "Title screen animation" force
TitleScreenAnimate_Bit0Zero:
    set 7, (ix+1) ; set high bit of $c15e
    inc (ix+2) ; increment $c15f
    dec (ix+3) ; decrement $c160
    ret p      ; if it's reached -1:
    set 0, (ix+1) ; set low bit of $c15e
    ret

UpdateTilemap_RightToLeftRow:
    ; get animation control value
    ld a, (ix+2)
    push hl
      ; de += 31 - n
      ld c, a
      ld a, $1F
      sub c
      add a, a
      ld l, a
      ld h, 0
      add hl, de
      ex de, hl
    pop hl
    ld b, (ix+2)
    inc b
    jp RawDataToVRAM_Interleaved2

UpdateTilemap_LeftToRightRow:
    ; get animation control value
    ld c, (ix+2)
    ; hl += 31 - n
    ld a, $1F
    sub c
    ld c, a
    ld b, $00
    add hl, bc
    ld b, (ix+2)
    inc b
    jp RawDataToVRAM_Interleaved2

TitleScreenAnimate_Bit1Zero:
    set 7, (ix+1) ; Set high bit of $c15e
    inc (ix+4)    ; Increment $c161
    ld a, (ix+4)  ; check if it's reached 36
    cp 36 ; $24
    ret nz
    set 1, (ix+1) ; Set bit 1 of $c15e when it does
    ret

UpdateSplashScreenAnimationTilesLine:
    ; parameters = b, c
    ; b determines VRAM address, c determines data source written there

    ; get b
    ld a, b
    ; get high 5 bits
    and %11111000
    ; multiply by 108
    LD_HL_A
    add hl, hl  ; x2
    add hl, hl  ; x4
    push hl
      add hl, hl  ; x8
      push hl
        add hl, hl  ; x16
        add hl, hl  ; x32
        push hl
          add hl, hl  ; x64
        pop de
        add hl, de  ; x64 + x32
      pop de
      add hl, de  ; x64 + x32 + x8
    pop de
    add hl, de  ; x64 + x32 + x8 + x4 = x108

    ; re-get b
    ld a, b
    ; get low 3 bits
    and %00000111
    add a, a ; x2
    add a, a ; x4
    LD_DE_A
    add hl, de ; x8
    ld de, $6020 ; magic? Tile 257
    add hl, de
    push hl
      ld a, c ; high 5 bits of c
      and $F8
      ; Multiply by 54
      LD_HL_A
      add hl, hl ; x2
      push hl
        add hl, hl  ; x4
        push hl
          add hl, hl  ; x8
          add hl, hl  ; x16
          push hl
            add hl, hl  ; x32
          pop de
          add hl, de  ; x32 + x16
        pop de
        add hl, de  ; x32 + x16 + x4
      pop de
      add hl, de  ; x32 + x16 + x4 + x2 = x54

      ld a, c ; low 3 bits of c
      and $07
      add a, a ; x2
      LD_DE_A
      add hl, de ; x4
      ld de, Tiles_Logo ; $0C32
      add hl, de
    pop de

    ld b, 27 ; counter
    ld c, Port_VDPData
-:  push bc
    push de
      push hl
        ; VRAM address to de
        ld a, e
        out (Port_VDPAddress), a
        ld a, d
        out (Port_VDPAddress), a
        ; output 2 bytes from hl
        push af ; delay
          outi
        pop af ; delay
        outi
      pop hl
      ld de, 16 ; hl += 16
      add hl, de
    pop de
    ; de += 32
    push hl
        ld hl, 32
        add hl, de
        ex de, hl
    pop hl
    pop bc
    djnz - ; repeat for 27x2 bytes
    ret

_LABEL_ACC_:
    ld a, b
    and $F8
    LD_HL_A
    add hl, hl ; x2
    add hl, hl ; x4
    push hl
      add hl, hl ; x8
      push hl
        add hl, hl ; x16
        add hl, hl ; x32
        push hl
          add hl, hl ; x64
        pop de
        add hl, de ; x96
      pop de
      add hl, de ; x104
    pop de
    add hl, de ; x108
    ld a, b
    and $07
    add a, a
    add a, a
    LD_DE_A
    add hl, de
    ld de, $6020
    add hl, de
    ld de, $0020
    ld b, $1B
    ld c, Port_VDPData
-:  push bc
      ld a, l
      out (Port_VDPAddress), a
      ld a, h
      out (Port_VDPAddress), a
      xor a
      push de
      pop de
      out (Port_VDPData), a
      push de
      pop de
      out (Port_VDPData), a
      add hl, de
    pop bc
    djnz -
    ret

TitleScreenAnimationVBlankEntry:
    ld a, ($C15E) ; check for high bit = new write
    bit 7, a
    ret z
    and %01111111 ; clear high bit
    ld ($C15E), a
    bit 0, a
    jp z, TitleScreenAnimation_Part1
    bit 1, a
    jp z, TitleScreenAnimation_Part2
    ret

TitleScreenAnimation_Part1:
    ld a, $09
    ld (RAM_VRAMFillHighByte), a
    ld hl, Tilemap_Logo + 32 * 0 ; $0B8A
    LD_DE_TILEMAP 0, 8
    call UpdateTilemap_RightToLeftRow
    ld hl, Tilemap_Logo + 32 * 1 ; $0BAA
    LD_DE_TILEMAP 0, 9
    call UpdateTilemap_LeftToRightRow
    ld hl, Tilemap_Logo + 32 * 2 ; $0BCA
    LD_DE_TILEMAP 0, 10
    call UpdateTilemap_RightToLeftRow
    ld hl, Tilemap_Logo + 32 * 3 ; $0BEA
    LD_DE_TILEMAP 0, 11
    call UpdateTilemap_LeftToRightRow
    ld hl, Tilemap_Logo + 32 * 4 ; $0C0A
    LD_DE_TILEMAP 0, 12
    jp UpdateTilemap_RightToLeftRow ; and ret

TitleScreenAnimation_Part2:
    ld c, $23
    ld b, (ix+4)
-:  push bc
      call UpdateSplashScreenAnimationTilesLine
    pop bc
    dec c
    dec b
    jp p, -

    ld a, (ix+4)
    cp $11
    jp nc, +

    inc a
    ld d, a
    ld a, $23
    sub d
    ld b, a
    ld c, $00
    dec d
-:  push de
    push bc
        call UpdateSplashScreenAnimationTilesLine
    pop bc
    pop de
    inc c
    dec b
    ld a, b
    cp d
    jp nz, -

    ld a, $23
    sub (ix+4)
    ld b, a
    call _LABEL_ACC_
+:  ret
.ends

.section "Title screen data 2" force
;.orga $b8a
Tilemap_Logo:
.incbin "Graphics/Logo tilemap.bin"
;.orga $c2a
Palette_Logo:
 COLOUR 0, 0, 1 ; Dark blue
 COLOUR 3, 3, 3 ; White
 COLOUR 1, 1, 1 ; Dark grey
 COLOUR 2, 2, 2 ; Light grey
 COLOUR 0, 0, 0 ; Black x4
 COLOUR 0, 0, 0
 COLOUR 0, 0, 0
 COLOUR 0, 0, 0
;.orga $c32
Tiles_Logo:
.incbin "Graphics/Logo tiles.2bpp"
;.orga $14a2
Tilemap_SegaLogo:
.incbin "Graphics/Sega logo.lsbtilemap"
;.orga $14ca
Tiles_SegaLogo:
.incbin "Graphics/Sega logo.pscompr"
.ends

.section "Non-VBlank drawing handlers dispatch" force
CallModeDrawingFunction:
    ld hl, RAM_CurrentMode
    ld a, (hl)
    and %00111111
    exx
      ld hl, CallModeDrawingFunction_JumpTable
      jp JumpToFunction

; Jump Table from 165C to 167F (18 entries, indexed by RAM_CurrentMode)
; These are functions to perform drawing tasks for the current mode, e.g. drawing with the pen, drawing a circle, drawing a menu...
; They sometimes do other stuff, e.g. mode 2 dispatches to some other mode.
CallModeDrawingFunction_JumpTable:
.dw NonVBlankMode0_DrawingFunction
.dw NonVBlankMode1_MenuFunction
.dw NonVBlankMode2_MenuItemSelectedFunction
.dw NonVBlankMode3_ColourFunction
.dw NonVBlankMode4_EraseFunction
.dw NonVBlankMode5_SquareFunction
.dw NonVBlankMode6_CircleAnd7_EllipseFunction
.dw NonVBlankMode6_CircleAnd7_EllipseFunction
.dw NonVBlankMode8_PaintFunction
.dw NonVBlankMode9_CopyFunction
.dw NonVBlankMode10_MirrorFunction
.dw NonVBlankMode11_MagnifyFunction
.dw NonVBlankMode12_DisplayFunction
.dw NonVBlankMode13_EndFunction
.dw NonVBlankMode14_LinePaintMenuFunction
.dw NonVBlankMode15_ColourSelectionMenuFunction
.dw NonVBlankMode16_MirrorAxisMenuFunction
.dw NonVBlankMode17_EraseConfirmationMenuFunction
.ends
; Note: menu-showing handlers could be refactored as they are all the same except for the parameters...

.section "Menu show/hide implementation" force
; 2nd entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode1_MenuFunction:
    exx
    ; Only do this when the high bit is set
    bit 7, (hl) ; RAM_CurrentMode
    ret nz
    set 7, (hl) ; RAM_CurrentMode
    inc hl
    ld (hl), Mode0_Drawing ; RAM_SelectedNextMode
    di
      xor a
      ld (RAM_ActionStateFlags), a
      inc a
      ld (RAM_StatusBarTextIndex), a ; Set it to blank
      call EnableOnlyThreeSprites
      call SetDrawingAreaTilemap

      LD_BC_AREA 12, 14
      ld de, MenuText
      LD_HL_LOCATION 5, 4
      call DrawTextToTilesWithBackup

      ld hl, (RAM_Pen_Smoothed)
      ld (RAM_Pen_Smoothed_Backup), hl
      ld hl, (RAM_Pen_Smoothed_Previous)
      ld (RAM_Pen_Smoothed_Previous_Backup), hl
      ld hl, (RAM_Pen_InMenus)
      ld (RAM_Pen_Smoothed), hl
      ld (RAM_Pen_Smoothed_Previous), hl
      ld a, 1
      ld (RAM_Beep), a
    ei
    ret
.ends

.section "Menu item selected implementation" force
; 3rd entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode2_MenuItemSelectedFunction:
      di
        ; Restore the tiles under the menu
        call SetDrawingAreaTilemap
        ld hl, (RAM_Pen_Smoothed)

        ld a, 72
        cp l ; Y position
        jp c, +
        LD_HL_LOCATION 88, 72
+:      ld (RAM_Pen_InMenus), hl
        call RestoreTileData
        ld hl, (RAM_Pen_Smoothed_Backup)
        ld (RAM_Pen_Smoothed), hl
        ld hl, (RAM_Pen_Smoothed_Previous_Backup)
        ld (RAM_Pen_Smoothed_Previous), hl
      exx
      inc hl
      ld a, (hl) ; RAM_SelectedNextMode
      ld (hl), Mode0_Drawing ; Default is to go back to drawing

      ; We check what was requested to decide if there is more to do...
      cp Mode3_Colour
      jp nz, +

      ; Mode3_Colour
      ld (hl), a ; Advance to next menu first
      ld a, Mode15_ColourSelectionMenu
      jp ++

+:    cp Mode4_Erase
      jp nz, +

      ; 4
      ld (hl), a ; 17 then 4
      ld a, Mode17_EraseConfirmationMenu
      jp ++

+:    cp Mode5_Square
      jp c, +
      cp Mode8_Paint
      jp nc, +
      ld (hl), a
      ld a, Mode14_LinePaintMenu ; Select line/paint for square, circle, ellipse
      jp ++

+:    cp Mode10_Mirror
      jp nz, ++
      ld (hl), a
      ld a, Mode16_MirrorAxisMenu ; Select axis first

++:   dec hl
      ld (hl), a ; RAM_CurrentMode
      ld a, 1
      ld (RAM_Beep), a
    ei
    ret
.ends

.section "Erase implementation" force
; 5th entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode4_EraseFunction:
      ; Only do it if "yes" was selected (2nd option)
      ld a, (RAM_SubmenuSelectionIndex)
      or a
      jp z, +

      ld a, (RAM_Beep)
      or a
      ret nz

      ; Zero all tile data
      di
        LD_DE_TILE 0
        ld h, 0
        ld bc, DRAWING_AREA_TOTAL_BYTES ; All tiles
        call FillVRAMWithH
      ei
    ; fall through
+:  exx
    ld (hl), Mode0_Drawing ; RAM_CurrentMode
    ld a, 1 ; Blank text
    ld (RAM_StatusBarTextIndex), a
    ret
.ends

.section "Display implementation" force
; 13th entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode12_DisplayFunction:
      ld a, (RAM_Beep)
      or a
      ret nz
    exx
    bit 7, (hl) ; RAM_CurrentMode
    jp z, + ; High bit unset means "enter display mode"

    ; High bit set means "wait for a button, then exit display mode"

    ; Wait for button
    ld a, (RAM_ButtonsNewlyPressed)
    bit 0, a
    ret z

    ; Restore the missing parts of the screen
    di
      call DrawUIControls
      ld hl, (RAM_Pen_Smoothed_Backup)
      ld (RAM_Pen_Smoothed), hl
      ld hl, (RAM_Pen_Smoothed_Previous_Backup)
      ld (RAM_Pen_Smoothed_Previous), hl
      ld a, Mode1_Menu
      ld (RAM_CurrentMode), a
    ei
    ret

+:  ; Enter "display mode"
    set 7, (hl) ; RAM_CurrentMode
    ld hl, (RAM_Pen_Smoothed)
    ld (RAM_Pen_Smoothed_Backup), hl
    ld hl, (RAM_Pen_Smoothed_Previous)
    ld (RAM_Pen_Smoothed_Previous_Backup), hl
    di
      call ScreenOff
      ; Sprites off
      call DisableSprites_VRAM
      ; Blank tilemap
      LD_DE_TILEMAP 0, 0
      ld hl, $8D09 ; Blank tile
      ld bc, 32*28
      call FillVRAMWithHL
      ; Restore tilemap
      call SetDrawingAreaTilemap
    ei
    jp ScreenOn ; and ret
.ends

.section "Line/Paint menu implementation" force
; 15th entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode14_LinePaintMenuFunction:
      ld a, (RAM_Beep)
      or a
      ret nz
    exx
    ; Bit 7 signals if the menu has been drawn yet
    bit 7, (hl)
    jp z, +

    ; Bit 6 signals if a choice has been chosen yet
    bit 6, (hl)
    ret z

    di
      exx
        ; Restore the graphics state
        call RestoreTileData
        ld hl, (RAM_Pen_Smoothed_Backup)
        ld (RAM_Pen_Smoothed), hl
        ld hl, (RAM_Pen_Smoothed_Previous_Backup)
        ld (RAM_Pen_Smoothed_Previous), hl
      exx
      ; Switch to the selected next mode
      inc hl ; RAM_SelectedNextMode
      ld a, (hl)
      ld (hl), Mode0_Drawing
      dec hl ; RAM_CurrentMode
      ld (hl), a
    ei
    ret

+:  set 7, (hl) ; set "menu drawn" flag
    di
      ; Draw it
      LD_BC_AREA 10, 4
      ld de, ModeMenuText
      LD_HL_LOCATION 5, 4
      call DrawTextToTilesWithBackup

      ld hl, (RAM_Pen_Smoothed)
      ld (RAM_Pen_Smoothed_Backup), hl
      ld hl, (RAM_Pen_Smoothed_Previous)
      ld (RAM_Pen_Smoothed_Previous_Backup), hl
      LD_HL_LOCATION 88, 72
      ld (RAM_Pen_Smoothed), hl
      ld (RAM_Pen_Smoothed_Previous), hl
    ei
    ret
.ends

.section "Colour selection menu show/hide" force
; 16th entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode15_ColourSelectionMenuFunction:
      ld a, (RAM_Beep)
      or a
      ret nz
    exx
    ; Bit 7 signals if the menu has been drawn yet
    bit 7, (hl)
    jp z, +

    ; Bit 6 signals if a choice has been chosen yet
    bit 6, (hl)
    ret z

    di
      exx
        ; Restore tiles
        call RestoreTileData
        ld hl, (RAM_Pen_Smoothed_Backup)
        ld (RAM_Pen_Smoothed), hl
        ld hl, (RAM_Pen_Smoothed_Previous_Backup)
        ld (RAM_Pen_Smoothed_Previous), hl
      exx
      ; Switch to the selected next mode (Mode3_Colour)
      inc hl ; RAM_SelectedNextMode
      ld a, (hl)
      ld (hl), Mode0_Drawing
      dec hl ; RAM_CurrentMode
      ld (hl), a
    ei
    ret

+:  set 7, (hl)
    di
      ; Draw the menu
      LD_BC_AREA 16, 4
      ld de, ColorMenuText
      LD_HL_LOCATION 5, 4
      call DrawTextToTilesWithBackup
      ld hl, (RAM_Pen_Smoothed)
      ld (RAM_Pen_Smoothed_Backup), hl
      ld hl, (RAM_Pen_Smoothed_Previous)
      ld (RAM_Pen_Smoothed_Previous_Backup), hl
      LD_HL_LOCATION 88, 72
      ld (RAM_Pen_Smoothed), hl
      ld (RAM_Pen_Smoothed_Previous), hl
    ei
    ret
.ends

.section "Mirror axis menu implementation" force
; 17th entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode16_MirrorAxisMenuFunction:
      ld a, (RAM_Beep)
      or a
      ret nz
    exx
    ; Bit 7 signals if the menu has been drawn yet
    bit 7, (hl)
    jp z, +

    ; Bit 6 signals if a choice has been chosen yet
    bit 6, (hl)
    ret z

    di
      exx
        ; Restore tiles
        call RestoreTileData
        ld hl, (RAM_Pen_Smoothed_Backup)
        ld (RAM_Pen_Smoothed), hl
        ld hl, (RAM_Pen_Smoothed_Previous_Backup)
        ld (RAM_Pen_Smoothed_Previous), hl
      exx
      ; Switch to the selected next mode
      inc hl ; RAM_SelectedNextMode
      ld a, (hl)
      ld (hl), Mode0_Drawing
      dec hl ; RAM_CurrentMode
      ld (hl), a
    ei
    ret

+:  set 7, (hl)
    di
      ; Draw the menu
      LD_BC_AREA 14, 4
      ld de, MirrorMenuText
      LD_HL_LOCATION 5, 4
      call DrawTextToTilesWithBackup
      ld hl, (RAM_Pen_Smoothed)
      ld (RAM_Pen_Smoothed_Backup), hl
      ld hl, (RAM_Pen_Smoothed_Previous)
      ld (RAM_Pen_Smoothed_Previous_Backup), hl

      LD_HL_LOCATION 88, 72
      ld (RAM_Pen_Smoothed), hl
      ld (RAM_Pen_Smoothed_Previous), hl
    ei
    ret
.ends

.section "Erase confirmation implementation" force
; 18th entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode17_EraseConfirmationMenuFunction:
      ld a, (RAM_Beep)
      or a
      ret nz
    exx
    ; Bit 7 signals if the menu has been drawn yet
    bit 7, (hl)
    jp z, +

    ; Bit 6 signals if a choice has been chosen yet
    bit 6, (hl)
    ret z

    di
      exx
        ; Restore tiles
        call RestoreTileData
        ld hl, (RAM_Pen_Smoothed_Backup)
        ld (RAM_Pen_Smoothed), hl
        ld hl, (RAM_Pen_Smoothed_Previous_Backup)
        ld (RAM_Pen_Smoothed_Previous), hl
      exx
      ; Switch to the selected next mode
      inc hl ; RAM_SelectedNextMode
      ld a, (hl)
      ld (hl), Mode0_Drawing
      dec hl ; RAM_CurrentMode
      ld (hl), a
    ei
    ret

+:  set 7, (hl)
    di
      ; Draw the menu
      LD_BC_AREA 13, 4
      ld de, EraseMenuText
      LD_HL_LOCATION 5, 4
      call DrawTextToTilesWithBackup
      ld hl, (RAM_Pen_Smoothed)
      ld (RAM_Pen_Smoothed_Backup), hl
      ld hl, (RAM_Pen_Smoothed_Previous)
      ld (RAM_Pen_Smoothed_Previous_Backup), hl
      LD_HL_LOCATION 88, 72
      ld (RAM_Pen_Smoothed), hl
      ld (RAM_Pen_Smoothed_Previous), hl
    ei
    ret
.ends

.section "End implementation" force
; 14th entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode13_EndFunction:
    exx

    ; Check if the timeout has started yet
    bit 7, (hl)
    jp z, +

    ; Wait for timeout to expire
    ld hl, RAM_TitleScreenAndEndTimeout
    dec (hl)
    ret p

    ; Then reset
    call ScreenOff
    jp FullReset

+:  set 7, (hl)
    ; Start waiting (2133ms)
    ld a, 128
    ld (RAM_TitleScreenAndEndTimeout), a
    ; Turn off sprites
    ld a, SpriteTableYTerminator
    ld (RAM_SpriteTable1.y), a
    ret
.ends

.section "Menu button handler" force
CheckMenuButton:
    ; Check for Menu button
    ld a, (RAM_ButtonsNewlyPressed)
    bit GraphicBoardButtonBit_Menu, a
    ret z ; Do nothing if button not just pressed
    ld a, (RAM_Beep)
    or a
    ret nz
    ; Should we show the menu?
    ld a, (RAM_CurrentMode)
    and %00111111
    cp Mode1_Menu
    ret z ; Not if we are already showing it
    cp Mode2_MenuItemSelected
    ret z ; And not if we just chose something in it

    ; Otherwise, show it
    ld a, Mode1_Menu
    ld (RAM_CurrentMode), a
    ret
.ends

.section "Menu drawing" force
DrawTextToTilesWithBackup:
; h = y offset (within drawing area)
; l = x offset (within drawing area)
; b = rows
; c = columns
; de = pointer to data

    ld a, (RAM_MenuShowing)
    or a
    call nz, RestoreTileData_SaveRegisters
    ld a, 1<<7
    ld (RAM_MenuShowing), a
    ld (RAM_GraphicsDataBuffer_Dimensions), bc
    push de
      ; Set the tile attribute
      push hl
        ; calculate de = (h+3) * 64
        push hl
          ld a, h
          add a, 3
          ld l, a
          ld h, 0
          add hl, hl
          add hl, hl
          add hl, hl
          add hl, hl
          add hl, hl
          add hl, hl
          ex de, hl
        pop hl
        ld a, l ; calculate hl = (l + 5) * 2 + 1
        add a, 5
        add a, a
        ld h, 0
        ld l, a
        inc l
        ; Add them together and that's a tilemap address
        add hl, de
        ld de, TileMapAddress
        add hl, de

        ld (RAM_GraphicsDataBuffer_VRAMAddress_Tilemap), hl ; Save that

        ; Set the tile attributes for the second palette
        ex de, hl
        ld h, TileAttribute_Palette2
        call SetAreaTileAttributes
      pop hl

      ; Calculate the VRAM address for the tile to draw over
      push hl
        ld a, h
        ld de, 22*SizeOfTile ; TODO: 22 = drawing area width
        call Multiply_a_de_ahl
        ex de, hl
      pop hl
      ld h, 0 ; hl = l * 32
      add hl, hl
      add hl, hl
      add hl, hl
      add hl, hl
      add hl, hl
      add hl, de
      ex de, hl
      set 6, d ; Set write bit
    pop hl

    ; Backup the tile
    ld (RAM_GraphicsDataBuffer_VRAMAddress_Tiles), de
    call BackupTilesToGraphicsDataBuffer

    ; Get the character count
    ld b, (hl)
    inc hl

    ; Start drawing
    VDP_ADDRESS_TO_DE
-:  ld a, (hl) ; Get the character
    cp $FF ; Indicates end of line
    inc hl
    jp z, +
    exx
      ld de, Font2bpp
      ld l, a
      ld h, 0
      add hl, hl ; offset by 16*a
      add hl, hl
      add hl, hl
      add hl, hl
      add hl, de
      ld b, 1 ; One tile
      call FillTiles2bppCurrentAddress
    exx
--: djnz -
    ret

+:  ; Move VRAM address on by a row
    push hl
      ld hl, 22*SizeOfTile
      add hl, de
      ex de, hl
    pop hl
    VDP_ADDRESS_TO_DE
    jp --
.ends

.section "Menu un-drawing" force
RestoreTileData_SaveRegisters:
    ; Register-protecting version of the below
    push bc
    push de
    push hl
      call RestoreTileData
    pop hl
    pop de
    pop bc
    ret

RestoreTileData:
    ; Parameters:
    ; RAM_GraphicsDataBuffer_VRAMAddress_Tiles = VRAM address of area to write to (in tiles)
    ; RAM_GraphicsDataBuffer_VRAMAddress_Tilemap = VRAM address of area to unset tile attributes
    ; RAM_GraphicsDataBuffer_Dimensions = row, column count
    ; Uses RAM_BytesPerRow
    ; Data comes from RAM_GraphicsDataBuffer
    xor a                     ; Zero ???
    ld (RAM_MenuShowing), a
    ld de, (RAM_GraphicsDataBuffer_VRAMAddress_Tilemap)
    ld bc, (RAM_GraphicsDataBuffer_Dimensions)
    ld h, TileAttribute_None  ; attributes
    call SetAreaTileAttributes
    push bc
    push de
    push hl
      ld de, (RAM_GraphicsDataBuffer_VRAMAddress_Tiles)
      ld bc, (RAM_GraphicsDataBuffer_Dimensions)
      LD_HL_C                 ; Calculate hl = columns * 32 = byte count
      add hl, hl
      add hl, hl
      add hl, hl
      add hl, hl
      add hl, hl
      ld (RAM_BytesPerRow), hl

      ld hl, RAM_GraphicsDataBuffer ; Data source
--:   push bc
        ld bc, (RAM_BytesPerRow)
        VDP_ADDRESS_TO_DE
-:      ld a, (hl)            ; Read a byte
        out (Port_VDPData), a ; Write to VRAM
        inc hl
        dec bc
        ld a, b
        or c
        jp nz, -

        push hl
          ld hl, 22 * SizeOfTile ; Move VRAM pointer on by a row
          add hl, de
          ex de, hl
        pop hl
      pop bc
      djnz --
    pop hl
    pop de
    pop bc
    ret
.ends

.section "Menu drawing data backup" force
BackupTilesToGraphicsDataBuffer:
    ; Args:
    ; RAM_GraphicsDataBuffer_VRAMAddress_Tiles = VRAM address to start at
    ; RAM_GraphicsDataBuffer_Dimensions = row, column count
    ; Uses RAM_BytesPerRow
    ; Saves data to RAM_GraphicsDataBuffer
    push bc
    push de
    push hl
      ld de, (RAM_GraphicsDataBuffer_VRAMAddress_Tiles)
      res 6, d        ; Make it a read address
      ld bc, (RAM_GraphicsDataBuffer_Dimensions)
      LD_HL_C         ; Calculate columns * 32 = number of bytes per row
      add hl, hl
      add hl, hl
      add hl, hl
      add hl, hl
      add hl, hl
      ld (RAM_BytesPerRow), hl
      ld hl, RAM_GraphicsDataBuffer
--:   push bc
        ld bc, (RAM_BytesPerRow)
        VDP_ADDRESS_TO_DE
        push af               ; Delay: 63 cycles from out to in
        pop af
-:      push af               ; Delay: 69 cycles between ins
        pop af
        in a, (Port_VDPData)  ; Read
        ld (hl), a            ; save to RAM
        inc hl
        dec bc
        ld a, b               ; Loop bc times
        or c
        jp nz, -
        push hl
          ld hl, 22 * SizeOfTile ; Move on one row
          add hl, de
          ex de, hl
        pop hl
      pop bc
      djnz --
    pop hl
    pop de
    pop bc
    ret
.ends

.section "Menu data" force
; This is the ASCII mapping for the regular font (outside the title screen).
.asciitable
map ' ' = 0
map 'A' to 'Z' = 1
map '!' = 27
map '.' = 28
map '?' = 29
map '-' = 30
; Menu borders
map '/' = 31      ; /^^^^^, 
map '^' = 32      ; [     ]
map ',' = 33      ; `_____'
map ']' = 34
map ''' = 35
map '_' = 36
map '`' = 37
map '[' = 38
; "Magnify" mode borders
map '~' = 39      ; Same order as above... ran out of sensible characters to use
map '*' = 40      ; ~*****;
map ';' = 41      ; (     )
map ')' = 42      ; %#####@
map '@' = 43
map '#' = 44
map '%' = 45
map '(' = 46
; Second space
map '$' = $ff ; end of line
.enda

MenuText: ; $1a10
.db 13*14
.asc "/^^ MENU ^^,$"
.asc "[  EXIT    ]$"
.asc "[  COLOR   ]$"
.asc "[  ERASE   ]$"
.asc "[  SQUARE  ]$"
.asc "[  CIRCLE  ]$"
.asc "[  ELLIPSE ]$"
.asc "[  PAINT   ]$"
.asc "[  COPY    ]$"
.asc "[  MIRROR  ]$"
.asc "[  MAGNIFY ]$"
.asc "[  DISPLAY ]$"
.asc "[  END     ]$"
.asc "`__________'$"

ModeMenuText: ; $1ac7
.db 11*4
.asc "/^ MODE ^,$"
.asc "[  LINE  ]$"
.asc "[  PAINT ]$"
.asc "`________'$"

ColorMenuText: ; $1af4
.db 17*4
.asc "/^ COLOR MENU ^,$"
.asc "[  COLOR SET   ]$"
.asc "[  BACK COLOR  ]$"
.asc "`______________'$"

MirrorMenuText: ; $1b39
.db 15*4
.asc "/^ MODE SET ^,$"
.asc "[  V-REVERSE ]$"
.asc "[  H-REVERSE ]$"
.asc "`____________'$"

ColorPageMenuText: ; $1b76
.db 14*8
.asc "/^^ COLOR ^^,$"
.asc "[           ]$"
.asc "[           ]$"
.asc "[           ]$"
.asc "[           ]$"
.asc "[  PAGE UP  ]$"
.asc "[  PAGE DOWN]$"
.asc "`___________'$"

ColourSelectionTilemap: ; $1be7
; Tiles showing selectable colours
.dw $0991 $098E $0992 $098E $0993 $098E $0994
.dw $098E $098E $098E $098E $098E $098E $098E
.dw $0995 $098E $0996 $098E $0997 $098E $0998

EraseMenuText: ; $1c11
.db 14*4
.asc "/^ ERASE ? ^,$"
.asc "[  NO       ]$"
.asc "[  YES      ]$"
.asc "`___________'$"
.ends

.section "Drawing implementation" force
; 1st entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode0_DrawingFunction:
    exx
    ld a, (RAM_ButtonsNewlyPressed)
    and $03
    ret nz
    di
    ld a, ($C062)
    or a
    jp nz, +
    ld a, (RAM_ButtonsPressed)
    ld (RAM_DrawingData.ButtonsPressed_virtual), a ; use the real buttons
    bit GraphicBoardButtonBit_Pen, a
    ld a, (RAM_PenStyle)
    ld (RAM_DrawingData.PenStyleForCurrentShape), a
    ld hl, (RAM_Pen_Smoothed_Previous)
    ld de, (RAM_Pen_Smoothed)
    call nz, DrawLine
    ld a, (RAM_Pen_Smoothed.x)
    ld (RAM_Pen_Smoothed_Previous.x), a
    ld a, (RAM_Pen_Smoothed.y)
    ld (RAM_Pen_Smoothed_Previous.y), a
    ei
    ret

+:  di
      ld a, (RAM_ButtonsPressed)
      ld (RAM_DrawingData.ButtonsPressed_virtual), a
      ld hl, (RAM_Pen_Smoothed)
      exx
      ld a, (RAM_PenStyle)
      ld (RAM_DrawingData.PenStyleForCurrentShape), a
      call DrawPenDotIfButtonPressed
      ld a, (RAM_Pen_Smoothed.x)
      ld (RAM_Pen_Smoothed_Previous.x), a
      ld a, (RAM_Pen_Smoothed.y)
      ld (RAM_Pen_Smoothed_Previous.y), a
    ei
    ret
.ends

.section "Line drawing" force
DrawLine:
    ; params: 
    ; hl = x1,y1
    ; de = x2,y2
    ; Bresenham's line drawing algorithm...

    ; First we decide if we are drawing left-to-right or right-to-left...
    ld c, 0 ; 0 if x2>=x1, 1 otherwise.
    ld a, d
    sub h
    jp nc, +
    neg
    ld c, 1
+:  ; Put the result in c', and put hl into hl' - that's our drawing x, y.
    push bc 
    push hl
    exx
    pop hl
    pop bc
    exx
    ld d, a ; d = abs(x2-x1) = dx

    ; Next, decide if we are draing bottom-top-top or top-to-bottom.
    ld a, e
    sub l
    jp c, _DrawLine_TopToBottom

_DrawLine_BottomToTop:
    ; (The code for top-to-bottom is almost identical, later.)

    ld e, a ; e = abs(y2-y1) = dy

    ld a, d ; Shallow or steep line?
    sub e
    jp c, _DrawLine_BottomToTop_Steep

    ; Rectangle is wider than it is tall - shallow line, step in x before y
    ld h, d
    srl h     ; h = dx / 2 = initial "error" accumulator. We subtract dy each time we draw a pixel; when this hits zero, we add on dx and move over by a pixel in the y axis.
    ld l, d
    inc l     ; l = dx + 1 = pixel count in the x axis
-:  call DrawPenDotIfButtonPressed
    dec l     ; decrement pixel counter
    ret z
    call _DrawLine_NextX ; Move left or right by 1 pixel
    ld a, h   ; Update "error" accumulator:
    sub e     ; - subtract dy
    ld h, a
    jp nc, -
    add a, d  ; - If we hit zero, add dx back on again
    ld h, a
    exx
      inc l   ;   - And increment y
    exx
    jp -

_DrawLine_BottomToTop_Steep:    
    ; Rectangle is taller than it is wide - steep line, step in y before x
    ld h, e
    srl h     ; h = dy / 2 = initial "error" accumulator
    ld l, e
    inc l     ; l = dy + 1 = pixel count
-:  call DrawPenDotIfButtonPressed
    dec l
    ret z
    exx
      inc l
    exx
    ld a, h
    sub d     
    ld h, a
    jp nc, -
    add a, e
    ld h, a
    call _DrawLine_NextX
    jp -

_DrawLine_TopToBottom:    
    neg ; fix negative dy
    ld e, a
    ld a, d
    sub e
    jp c, _DrawLine_TopToBottom_Steep
_DrawLine_TopToBottom_Shallow:
    ld h, d
    srl h
    ld l, d
    inc l
-:  call DrawPenDotIfButtonPressed
    dec l
    ret z
    call _DrawLine_NextX
    ld a, h
    sub e
    ld h, a
    jp nc, -
    add a, d
    ld h, a
    exx
      dec l
    exx
    jp -

_DrawLine_TopToBottom_Steep:
    ld h, e
    srl h
    ld l, e
    inc l
-:  call DrawPenDotIfButtonPressed
    dec l
    ret z
    exx
      dec l
    exx
    ld a, h
    sub d
    ld h, a
    jp nc, -
    add a, e
    ld h, a
    call _DrawLine_NextX
    jp -

_DrawLine_NextX:
    ; If low bit of c' is 0, --h' else ++h'
    ; This is used to move over by one pixel in the shorter axis, e.g. for a shallow line, h' is the y coordinate.
    ; c' lets us know if we're drawing upwards or downwards.
    exx
      bit 0, c
      jp nz, +
      inc h
      jp ++
+:    dec h
++: exx
    ret

; $1D3F
    ret ; Unused
.ends

.section "Dot drawing" force
DrawPenDotIfButtonPressed:
    ld a, (RAM_DrawingData.ButtonsPressed_virtual) ; Do nothing if pen bit is not set
    bit GraphicBoardButtonBit_Pen, a
    ret z
    exx
      push bc
      push hl
        call DrawPenDot
      pop hl
      pop bc
    exx
    ret

DrawPenDot:
    ld a, l ; screen y?
    sub 60 ; seems high
    ld (RAM_DrawingData.PixelYPlus0), a ; y
    inc a
    ld (RAM_DrawingData.PixelYPlus1), a ; y + 1
    sub 2
    ld (RAM_DrawingData.PixelYMinus1), a ; y - 1
    ld a, h ; screen x?
    sub 36 ; seems low?
    ld (RAM_DrawingData.PixelXPlus0), a ; x
    inc a
    ld (RAM_DrawingData.PixelXPlus1), a ; x + 1
    sub 2
    ld (RAM_DrawingData.PixelXMinus1), a ; x - 1

    ld a, (RAM_DrawingData.PenStyleForCurrentShape)
    or a ; PenStyle_Thin
    jp z, +
    cp PenStyle_Medium
    jp z, ++
    cp PenStyle_Thick
    jp z, +++
    cp PenStyle_Erase
    jp z, ++ ; medium pen for erase
    ret

+:  ; Draw one pixel (selected)
    ld a, (RAM_DrawingData.PixelXPlus0)
    ld (RAM_DrawingData.PixelXToDraw), a ; x
    ld a, (RAM_DrawingData.PixelYPlus0)
    ld (RAM_DrawingData.PixelYToDraw), a ; y
    jp DrawPixel

++: ; Draw four pixels (top-left = selected)
    ld a, (RAM_DrawingData.PixelXPlus0)
    ld (RAM_DrawingData.PixelXToDraw), a
    ld a, (RAM_DrawingData.PixelYPlus0)
    ld (RAM_DrawingData.PixelYToDraw), a
    call DrawPixel
    ld a, (RAM_DrawingData.PixelXPlus0)
    ld (RAM_DrawingData.PixelXToDraw), a
    ld a, (RAM_DrawingData.PixelYPlus1)
    ld (RAM_DrawingData.PixelYToDraw), a
    call DrawPixel
    ld a, (RAM_DrawingData.PixelXPlus1)
    ld (RAM_DrawingData.PixelXToDraw), a
    ld a, (RAM_DrawingData.PixelYPlus0)
    ld (RAM_DrawingData.PixelYToDraw), a
    call DrawPixel
    ld a, (RAM_DrawingData.PixelXPlus1)
    ld (RAM_DrawingData.PixelXToDraw), a
    ld a, (RAM_DrawingData.PixelYPlus1)
    ld (RAM_DrawingData.PixelYToDraw), a
    jp DrawPixel

+++:; Draw 9 pixels (middle = selected)
    ld a, (RAM_DrawingData.PixelXMinus1)
    ld (RAM_DrawingData.PixelXToDraw), a
    ld a, (RAM_DrawingData.PixelYMinus1)
    ld (RAM_DrawingData.PixelYToDraw), a
    call DrawPixel
    ld a, (RAM_DrawingData.PixelXMinus1)
    ld (RAM_DrawingData.PixelXToDraw), a
    ld a, (RAM_DrawingData.PixelYPlus0)
    ld (RAM_DrawingData.PixelYToDraw), a
    call DrawPixel
    ld a, (RAM_DrawingData.PixelXMinus1)
    ld (RAM_DrawingData.PixelXToDraw), a
    ld a, (RAM_DrawingData.PixelYPlus1)
    ld (RAM_DrawingData.PixelYToDraw), a
    call DrawPixel
    ld a, (RAM_DrawingData.PixelXPlus0)
    ld (RAM_DrawingData.PixelXToDraw), a
    ld a, (RAM_DrawingData.PixelYMinus1)
    ld (RAM_DrawingData.PixelYToDraw), a
    call DrawPixel
    ld a, (RAM_DrawingData.PixelXPlus0)
    ld (RAM_DrawingData.PixelXToDraw), a
    ld a, (RAM_DrawingData.PixelYPlus0)
    ld (RAM_DrawingData.PixelYToDraw), a
    call DrawPixel
    ld a, (RAM_DrawingData.PixelXPlus0)
    ld (RAM_DrawingData.PixelXToDraw), a
    ld a, (RAM_DrawingData.PixelYPlus1)
    ld (RAM_DrawingData.PixelYToDraw), a
    call DrawPixel
    ld a, (RAM_DrawingData.PixelXPlus1)
    ld (RAM_DrawingData.PixelXToDraw), a
    ld a, (RAM_DrawingData.PixelYMinus1)
    ld (RAM_DrawingData.PixelYToDraw), a
    call DrawPixel
    ld a, (RAM_DrawingData.PixelXPlus1)
    ld (RAM_DrawingData.PixelXToDraw), a
    ld a, (RAM_DrawingData.PixelYPlus0)
    ld (RAM_DrawingData.PixelYToDraw), a
    call DrawPixel
    ld a, (RAM_DrawingData.PixelXPlus1)
    ld (RAM_DrawingData.PixelXToDraw), a
    ld a, (RAM_DrawingData.PixelYPlus1)
    ld (RAM_DrawingData.PixelYToDraw), a
    jp DrawPixel ; ### Unnecessary, could fall through
.ends

.section "Pixel drawing" force
DrawPixel:
    ld a, (RAM_DrawingData.PixelYToDraw) ; y
    cp DRAWING_AREA_HEIGHT_PIXELS
    ret nc ; Do nothing if off the bottom
    ld b, a
    ld a, (RAM_DrawingData.PixelXToDraw) ; x
    cp DRAWING_AREA_WIDTH_PIXELS ; Do nothing if off the right
    ret nc

    call CheckForReset ; since we may be in here a long time

    ; Determine the tile for this pixel
    ld c, a ; x
    ld a, b ; y
    and %11111000 ; Clear low three bits = round down to multiple of 8
    ld h, 0 ; calculate de = a * 88. 88 = DRAWING_AREA_WIDTH_TILES * 4. 4 because 8px becomes 32 bytes in VRAM.
    ld l, a
    add hl, hl ; x2
    add hl, hl ; x4
    add hl, hl ; x8
    push hl
      add hl, hl ; x16
      push hl
        add hl, hl ; x32
        add hl, hl ; x64
      pop de
      add hl, de ; x64 + x16
    pop de
    add hl, de ; x64 + x16 + x8 = x88
    ex de, hl
    ; Repeat for x
    ld a, c
    and %11111000
    LD_HL_A ; hl = a * 4 + de
    add hl, hl
    add hl, hl
    add hl, de
    ; Now hl is the tile base address

    ; Adjust for the address within the tile
    ld a, b ; add 4 bytes per y pixel offset
    and %00000111
    add a, a
    add a, a
    LD_DE_A
    add hl, de
    ex de, hl
    ; Copy the 4 bytes for that row to RAM
    push bc
      ld hl, RAM_TileModificationBuffer
      ld bc, 4
      call CopyVRAMToRAM
    pop bc
    ; Then calculate the x offset within the row
    ld a, c
    and %00000111
    push de
      ; Get the bitmask for the pixel in question
      ld hl, Table_BitInPixelRowFromX
      LD_DE_A
      add hl, de
      ld a, (hl)
      ld hl, RAM_TileModificationBuffer
      ld e, a ; e = 1 for the pixel in question
      cpl
      ld d, a ; d = 1 for every other pixel

      ld b, 4 ; Number of bitplanes
      ld c, 0 ; Palette index for erase
      ld a, (RAM_DrawingData.PenStyleForCurrentShape)
      cp PenStyle_Erase
      jp nc, +
      ld a, (RAM_DrawingData.CurrentlySelectedPaletteIndex)
      ld c, a
      and a ; clear carry
+:    
-:    ; For each bitplane...
      rrc c      ; Get the bit for this bitplane
      ld a, (hl) ; Get the existing byte for this bitplane
      jp nc, +   ; If the bit is 1, set the bit in the byte
      or e
      ld (hl), a
      jp ++
+:    and d      ; ...else unset it
      ld (hl), a
++:   inc hl     ; Next byte
      djnz -
    pop de
    ; Write the row back to VRAM
    ld a, d
    or >VDPAddressMask_Write
    ld d, a
    ld hl, RAM_TileModificationBuffer
    ld bc, 4
    jp RawDataToVRAM ; and ret

; Data from 1EDA to 1EE1 (8 bytes)
Table_BitInPixelRowFromX: ; Table_BitInPixelRowFromX
.db %10000000
.db %01000000
.db %00100000
.db %00010000
.db %00001000
.db %00000100
.db %00000010
.db %00000001
.ends

.section "Colour selection menu implementation" force
; 4th entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode3_ColourFunction:
    exx
    bit 7, (hl)
    jp z, + ; could ret nz
    ret
+:  set 7, (hl)
    di

    ; Draw the text
    ld de, ColorPageMenuText
    LD_BC_AREA 13, 8
    LD_HL_LOCATION 5, 4
    call DrawTextToTilesWithBackup

    ; Draw the colour boxes
    ld hl, ColourSelectionTilemap
    LD_DE_TILEMAP 14, 8
    LD_BC_TILEMAP_AREA 7, 3
    call WriteAreaToTilemap

    ; Initialise the colours
    xor a ; start at 0
    ld (RAM_ColourSelectionStartValue), a
    ld a, 1
    ld (RAM_NeedToUpdatePalette), a
    ei
    ret

UpdateColourSelectionLabels:
    ; Updates the labels in the colour selection screen
    ; (numbers only)
    ld a, (RAM_ColourSelectionStartValue)
    and %00111100 ; $3C
    rrca ; Divide by 4
    rrca
    LD_DE_TILE 116 ; Upper digit
    call +
    LD_DE_TILE 160 ; Lower digit
    inc a
    and $0F
+:  push af
      ; Look up a'th entry in table
      ld hl, UpdateColourSelectionLabels_TileOffsets
      add a, a
      ld c, a
      ld b, 0
      add hl, bc
      VDP_ADDRESS_TO_DE
      ld a, (hl) ; Get first byte
      call +
      inc hl
      ld a, (hl) ; Then second
      call +
    pop af
    ret

+:  push hl
      ; Offset into table
      ld c, a
      ld b, 0
      ld hl, Font2bpp + 47 * SizeOfTile / 2 ; space before zero
      add hl, bc
      ld b, 1
      call FillTiles2bppCurrentAddress
    pop hl
    ret

UpdateColourSelectionLabels_TileOffsets:
; Each byte is the offset in bytes of the desired digit from the space before 0
; i.e. 0 = space, 16 = 0, 32 = 1, ...
.macro DIGIT_OFFSET args value
.if value < 10
.db 0 ; space
.else
.db (1 + 1) * SizeOfTile / 2
.endif
.db (value # 10 + 1) * SizeOfTile / 2
.endm
 DIGIT_OFFSET 1
 DIGIT_OFFSET 2
 DIGIT_OFFSET 3
 DIGIT_OFFSET 4
 DIGIT_OFFSET 5
 DIGIT_OFFSET 6
 DIGIT_OFFSET 7
 DIGIT_OFFSET 8
 DIGIT_OFFSET 9
 DIGIT_OFFSET 10
 DIGIT_OFFSET 11
 DIGIT_OFFSET 12
 DIGIT_OFFSET 13
 DIGIT_OFFSET 14
 DIGIT_OFFSET 15
 DIGIT_OFFSET 16
.ends

.section "Rectangle drawing implementation" force
; 6th entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode5_SquareFunction:
      ld a, (RAM_ActionStateFlags)
      cp %00000011 ; Are we ready to draw?
      ret nz

      ; Wait for the beep to end
      ld a, (RAM_Beep)
      or a
      ret nz

    exx
    push hl
      di
        ld a, (RAM_PenStyle) ; Pen width
        ld b, a
        ld a, (RAM_SubmenuSelectionIndex)
        or a
        jp z, +
        ld b, PenStyle_Thin ; fill mode -> pen width thin (no border width, no erase)
+:      ld a, b
        ld (RAM_DrawingData.PenStyleForCurrentShape), a
        ld a, 1<<GraphicBoardButtonBit_Pen ; Simulate pen button bit
        ld (RAM_DrawingData.ButtonsPressed_virtual), a
        ld iy, RAM_DrawingData
        
        ; Draw a line from (x1, y1) to (x1, y2) = left side
        ld h, (iy+DrawingData.SquareCorner1_x)
        ld l, (iy+DrawingData.SquareCorner1_y)
        ld d, h
        ld e, (iy+DrawingData.SquareCorner2_y)
        call DrawLine

        ; Draw a line from (x1, y2) to (x2, y2) = bottom side
        ld h, (iy+DrawingData.SquareCorner1_x)
        ld l, (iy+DrawingData.SquareCorner2_y)
        ld d, (iy+DrawingData.SquareCorner2_x)
        ld e, l
        call DrawLine
        
        ; Draw a line from (x2, y2) to (x2, y1) = right side
        ld h, (iy+DrawingData.SquareCorner2_x)
        ld l, (iy+DrawingData.SquareCorner2_y)
        ld d, h
        ld e, (iy+DrawingData.SquareCorner1_y)
        call DrawLine
        
        ; Draw a line from (x2, y1) to (x1, y1) = top side
        ld h, (iy+DrawingData.SquareCorner2_x)
        ld l, (iy+DrawingData.SquareCorner1_y)
        ld d, (iy+DrawingData.SquareCorner1_x)
        ld e, l
        call DrawLine

        ld a, (RAM_SubmenuSelectionIndex)
        or a
        jp z, _NonVBlankMode5_SquareFunction_Done
        
        ; Fill mode
        ld a, (iy+DrawingData.SquareHeight)
        or a
        jp nz, +
        inc a
+:      ld b, a ; Number of lines to draw = height + 1

        ld a, (iy+DrawingData.SquareCorner2_x)
        sub (iy+DrawingData.SquareCorner1_x)
        cp 8
        jp c, _FillSquare_Small ; For smaller than 8px wide, do it with lines
        
        ; >=8px wide
        ; Check the X coordinates are on-screen...
        ld a, DRAWING_AREA_MIN_X_PIXELS
        cp (iy+DrawingData.SquareCorner1_x)
        jp c, +
        cp (iy+DrawingData.SquareCorner2_x)
        jp nc, _NonVBlankMode5_SquareFunction_Done ; Rectangle is offscreen on the left
+:      ld a, DRAWING_AREA_MAX_X_PIXELS
        cp (iy+DrawingData.SquareCorner1_x)
        jp nc, +
        cp (iy+DrawingData.SquareCorner2_x)
        jp c, _NonVBlankMode5_SquareFunction_Done ; Or right
+:      ; Convert the X coordinates to canvas space (with clamping)
        ld a, (iy+DrawingData.SquareCorner1_x)
        call _ScreenXToCanvasX
        ld (iy+DrawingData.SquareCorner1_x), a
        ld a, (iy+DrawingData.SquareCorner2_x)
        call _ScreenXToCanvasX
        ld (iy+DrawingData.SquareCorner2_x), a
        ; Check the Y coordinates are on-screen...
        ld a, (iy+DrawingData.SquareCorner1_y)
        add a, (iy+DrawingData.SquareHeight)
        sub DRAWING_AREA_MIN_Y_PIXELS
        jp c, _NonVBlankMode5_SquareFunction_Done
        ld a, (iy+DrawingData.SquareCorner1_y)
        cp DRAWING_AREA_MAX_Y_PIXELS
        jp c, +
        ld a, (iy+DrawingData.SquareCorner2_y)
        cp DRAWING_AREA_MAX_Y_PIXELS
        jp nc, _NonVBlankMode5_SquareFunction_Done
+:      ; Convert the Y coordinate and box height to canvas space
        ld a, (iy+DrawingData.SquareCorner1_y)
        sub DRAWING_AREA_MIN_Y_PIXELS
        jp nc, +
        xor a
+:      ld (iy+DrawingData.SquareCorner1_y), a
        ld c, a
        ld a, (iy+DrawingData.SquareCorner2_y)
        sub DRAWING_AREA_MIN_Y_PIXELS
        sub c
        ld b, a
-:      push bc
          ld a, (iy+DrawingData.SquareCorner1_y)
          ; Round down to nearest multiple of 8
          and $F8
          ; Multiply by 88, to make a tile address
          LD_HL_A
          add hl, hl
          add hl, hl
          add hl, hl
          push hl
            add hl, hl
            push hl
              add hl, hl
              add hl, hl
            pop de
            add hl, de
          pop de
          add hl, de
          ; Then add on the correct X offset...
          push hl
            ld a, (iy+DrawingData.SquareCorner1_x)
            and $F8
            LD_HL_A
            add hl, hl
            add hl, hl
          pop de
          add hl, de
          ; Then the Y offset to get the byte
          ld a, (iy+DrawingData.SquareCorner1_y)
          and $07
          add a, a
          add a, a
          LD_DE_A
          add hl, de
          ex de, hl
          ; Then draw it
          call _DrawLongHorizontalLine
        pop bc
        ; Move on to the next row
        inc (iy+DrawingData.SquareCorner1_y)
        ; Check if it's got to the bottom of the screen
        ld a, (iy+DrawingData.SquareCorner1_y)
        cp DRAWING_AREA_HEIGHT_PIXELS
        jp nc, _NonVBlankMode5_SquareFunction_Done
        ; If not, loop
        djnz -

_NonVBlankMode5_SquareFunction_Done:
        ; Hide the second sprite offscreen
        ld a, $F0
        ld (RAM_SpriteTable1.y + 3), a
        ; Signal that drawing has finished
        xor a
        ld (RAM_ActionStateFlags), a
      pop hl
    ei
    ret

_FillSquare_Small:
    ; b = number of lines to draw
    ; Use DrawLine to do it
    ld h, (iy+DrawingData.SquareCorner1_x)
    ld l, (iy+DrawingData.SquareCorner1_y)
    ld d, (iy+DrawingData.SquareCorner2_x)
-:  push bc
    push de
    push hl
      ld e, l
      call DrawLine
    pop hl
    pop de
    pop bc
    inc l
    djnz -
    jp _NonVBlankMode5_SquareFunction_Done

_ScreenXToCanvasX:
    ; args: a = x coordinate in screen space
    ; returns: a = x coordinate in canvas space
    sub DRAWING_AREA_MIN_X_PIXELS
    jp nc, +
    xor a
+:  cp DRAWING_AREA_WIDTH_PIXELS
    ret c
    ld a, DRAWING_AREA_WIDTH_PIXELS
    ret

_DrawLongHorizontalLine:
    ; Inputs:
    ; de = VRAM address of byte containing starting point
    ; Draws a multi-tile horizontal line into VRAM
    ; Does it in three parts:
    ; 1. Draw fractional tile on the left, if needed
    ; 2. Draw solid tiles in the middle, if any
    ; 3. Draw fractional tile at the right, if needed
    
    ; Part 1
    ; Get the x offset within the tile
    ld a, (iy+DrawingData.SquareCorner1_x)
    ld c, a
    and $07
    ; If zero, we may as well skip ahead to the faster (unmasked) code
    jp z, +
    ; Else look up the bitmask
    push de
      ld hl, Table_SetPixelsInRow
      LD_DE_A
      add hl, de
      ld a, (hl)
    pop de
    ; Modify the VRAM for this tile
    call ModifyVRAMByteUsingBitmask
    ; Move on to the next tile
    ld hl, SizeOfTile
    add hl, de
    ex de, hl
    ; Bump up x1 to the next multiple of 8px
    ld a, (iy+DrawingData.SquareCorner1_x)
    add a, 8
    and $F8
    ld c, a

+:  ; Part 2
    ; How many solid tiles can we draw?
    ld a, (iy+DrawingData.SquareCorner2_x)
    and $F8
    sub c
    jp c, + ; We hit the right side, skip on

    ; Draw solid tiles
    and $F8 ; Shouldn't be necessary?
    ; Divide by 8 to get a tile count
    rrca
    rrca
    rrca
    or a
    jp z, +
    ld b, a
-:  call DrawSelectedPaletteIndexToTileRow ; Draw that many solid tiles
    ld hl, SizeOfTile
    add hl, de
    ex de, hl
    djnz -

+:  ; Part 3
    ld a, (iy+DrawingData.SquareCorner2_x)
    and $07
    ret z
    push de
      ld hl, Table_SetPixelsInRow
      LD_DE_A
      add hl, de
    pop de
    ld a, (hl)
    cpl ; Invert the bitmask
    jp ModifyVRAMByteUsingBitmask ; And ret

DrawSelectedPaletteIndexToTileRow:
    ; Inputs: de = VRAM address
    ; Set VRAM address for writing (no need to read)
    ld a, e
    out (Port_VDPAddress), a
    ld a, d
    or >VDPAddressMask_Write
    out (Port_VDPAddress), a
    ; For each bit in the palette index, we fill a whole bitplane-row in the tile.
    ld hl, $00FF ; For a zero bit, we write $00; for a 1 bit, we write $ff.
    ld c, Port_VDPData
    ld a, (iy+DrawingData.CurrentlySelectedPaletteIndex)
    rrca
    jp c, +
    out (c), h
    jp ++
+:  out (c), l
++: nop
    rrca
    jp c, +
    out (c), h
    jp ++
+:  out (c), l
++: nop
    rrca
    jp c, +
    out (c), h
    jp ++
+:  out (c), l
++: nop
    rrca
    jp c, +
    out (c), h
    jp ++
+:  out (c), l
++: ret

ModifyVRAMByteUsingBitmask:
    ; Inputs:
    ; de = VRAM address
    ; a = bitmask
    ; RAM_DrawingData.CurrentlySelectedPaletteIndex = palette index to write

    ; Get the bytes into RAM
    ld hl, RAM_TileModificationBuffer
    push de
      push af
        ld bc, 4
        call CopyVRAMToRAM
      pop af
      ; Invert the bitmask so it's 1 for the bits to keep, 0 for the ones to change
      cpl
      ld b, a
      ld hl, RAM_TileModificationBuffer
      ; Get byte
      ld a, (hl)
      ; Unset the bits fo rthe pixels we want to change
      and b
      ld (hl), a
      ; repeat for all four bytes
      inc hl
      ld a, (hl)
      and b
      ld (hl), a
      inc hl
      ld a, (hl)
      and b
      ld (hl), a
      inc hl
      ld a, (hl)
      and b
      ld (hl), a
    pop de

    ; Set VRAM address for writing
    ld a, e
    out (Port_VDPAddress), a
    ld a, d
    or >VDPAddressMask_Write
    out (Port_VDPAddress), a

    ; Invert the bitmask back to its original form
    ld a, b
    cpl
    ld hl, RAM_TileModificationBuffer
    push de
      ld b, a ; bitmask
      ld e, (iy+DrawingData.CurrentlySelectedPaletteIndex) ; Value to write
      rrc e
      ld a, (hl)
      inc hl
      jp nc, + ; If a zero bit is wanted, there's nothing to do
      or b     ; Else we set the bit (for all pixels)
+:    out (Port_VDPData), a ; And write it immediately to VRAM
      ; Repeat for all four pixels
      ld a, (hl)
      inc hl
      rrc e
      jp nc, +
      or b
+:    out (Port_VDPData), a
      ld a, (hl)
      inc hl
      rrc e
      jp nc, +
      or b
+:    out (Port_VDPData), a
      ld a, (hl)
      rrc e
      jp nc, +
      or b
+:    out (Port_VDPData), a
    pop de
    ret

Table_SetPixelsInRow: ; $219a
; For a given index i, holds the bitmask for setting the pixels i..7
.db %11111111
.db %01111111
.db %00111111
.db %00011111
.db %00001111
.db %00000111
.db %00000011
.db %00000001
.ends

.section "Ellipse drawing implementation" force
; 7th entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode6_CircleAnd7_EllipseFunction:
      ; Wait for beep to finish
      ld a, (RAM_Beep)
      or a
      ret nz
    exx
    ; Check low bit is set
    ld a, (RAM_ActionStateFlags)
    rlca
    ret nc
    push hl
      ; b = minor radius
      ld a, (RAM_EllipseMinorRadius)
      ld b, a
      ; Simulate pen held down
      ld a, 1 << GraphicBoardButtonBit_Pen
      ld (RAM_DrawingData.ButtonsPressed_virtual), a
      ; a = pen style
      ld a, (RAM_PenStyle)
      ld d, a
      ld a, (RAM_SubmenuSelectionIndex)
      ld e, a
      or a
      jp z, +
      ld d, PenStyle_Thin ; Fill mode -> thin pen, no erase
+:    ld a, d
      ld (RAM_DrawingData.PenStyleForCurrentShape), a
      ld a, e
      ld de, (RAM_CircleEllipseCentre)
      ld hl, (RAM_EllipseRatio)
      di
      call DrawEllipse
    pop hl
    ; Clear action flags
    xor a
    ld (RAM_ActionStateFlags), a
    ; Hide second cursor
    ld a, $F0
    ld (RAM_SpriteTable1.y + 3), a
    ; Done
    ei
    ret

DrawEllipse:
; Inputs:
; a = pen style
; b = minor radius
; de = centre coordinates
; hl = ratio of radii (ry/rx), *256
    
      ex af, af' ; Save pen style for later

      ; Do nothing for r<2
      ld a, b
      cp 2
      ret c

      ; Check for a circle (hl = $0100)
      ld (RAM_EllipseRatio), hl
      ld a, h
      dec a
      or l
      jp z, DrawEllipse_Circle

      ; It's an ellipse
      ; Wide or tall?
      ld a, h
      or a
      jp z, DrawEllipse_Wide
      
      ; Calculate the reciprocal of the ratio
      ex de, hl
      push hl
      push bc
        call Reciprocal_de_a
      pop bc
      pop hl
      ex de, hl
      ld (RAM_EllipseRatio), a
      jp DrawEllipse_Tall

DrawEllipse_Circle:      
      ; Inputs:
      ; a' = pen style
      ; b = radius
      ; de = centre coordinates
      ; This implements a slightly unusual algorithm, because it produces "fat" diagonals: 
      ; each point is either one pixel movement in the x or y axis, never both.
      ; Start from the line equation x^2 + y^2 = r^2
      ; Then for a given point x, y that has been drawn, the error is
      ; |x^2 + y^2 - r^2|
      ; When drawing the first quadrant, we start at (r, 0) and have error 0.
      ; Then, for each pixel, we decide to step up or left. We choose which to do by choosing which has the least error.
      ; We start with the inequality for deciding to go left:
      ;     Error(x-1, y) > Error(x, y-1)
      ; Square both sides:
      ;     Error^2(x-1, y) > Error^2(x, y-1)
      ; This lets us replace the mod with the squared contents:
      ;     ((x-1)^2 + y^2 - r^2)^2 > (x^2 + (y-1)^2 - r^2)^2
      ; We expand it out a bit:
      ;     (x^2 + y^2 - r^2 - 2x + 1)^2 > (x^2 + y^2 - r^2 - 2y + 1)^2
      ; We factor for the contents of the right part on the left:
      ;     ((x^2 + y^2 - r^2 - 2y + 1) + 2(y - x))^2 > (x^2 + y^2 - r^2 - 2y + 1)^2
      ; Expand the square on the left:
      ;     (x^2 + y^2 - r^2 - 2y + 1)^2 + 4(y - x)(x^2 + y^2 - r^2 - 2y + 1) + 4(y - x)^2 > (x^2 + y^2 - r^2 - 2y + 1)^2
      ; Subtract the RHS from both sides:
      ;     4(y - x)(x^2 + y^2 - r^2 + 2y + 1) + 4(y - x)^2 > 0
      ; Divide through by 4(y - x). This is always negative, so the inequality flips.
      ;     (x^2 + y^2 - r^2 - 2y + 1) + (y - x) < 0
      ; Simplify:
      ;     (x^2 + y^2 - r^2) - x - y + 1 < 0
      ; Now we have the error value from the current point:
      ;     Error(x, y) - x - y + 1 < 0
      ; So we should go left if this is true, and up if it is not.
      ; If it is true, we want to update the error:
      ;     Error(x-1, y) = Error(x, y) - 2x + 1
      ; If it is false:
      ;     Error(x, y-1) = Error(x, y) - 2y + 1
      ; The code has to operate in 16 bits because the accumulated values may exceed the 8-bit range.
      ld l, e
      ld h, 0
      ld (RAM_EllipseCurrentY), hl ; Centre y as 16 bits
      ld l, d ; Centre x
      ld e, b ; Radius ("x")
      ld d, h ; = 0
      add hl, de
      ld (RAM_EllipseCurrentX), hl ; Initial x point as 16 bits
      ld bc, 0       ; y
      ld hl, 0
      ld (RAM_EllipseLastPointError), hl
      
      ; First segment: upper-right quadrant
-:    call Ellipse_DrawPoint ; Draw a point
      ld hl, (RAM_EllipseLastPointError)  ; radiusError +=...
      inc hl          ; ...1
      or a
      sbc hl, de      ; ...-x
      or a
      sbc hl, bc      ; ...-y
      jp m, +
      ; Didn't overflow
      add hl, bc      ; ...+y
      or a
      sbc hl, de      ; ...-x
      ld (RAM_EllipseLastPointError), hl  ; So overall += 1-2x
      dec de          ; --x
      ld hl, (RAM_EllipseCurrentX)
      dec hl
      ld (RAM_EllipseCurrentX), hl
      jp ++

+:    ; Overflowed
      add hl, de      ; ...+x
      or a
      sbc hl, bc      ; ...-y
      ld (RAM_EllipseLastPointError), hl ; So overall += 1-2y
      dec bc          ; ++y
      ld hl, (RAM_EllipseCurrentY)
      dec hl
      ld (RAM_EllipseCurrentY), hl

++:   ld a, d ; Repeat until de = 0
      or e
      jp nz, -

      ; Second segment: upper-left quadrant
      ; Similar to before, but with some things swapped around
-:    call Ellipse_DrawPoint
      ld hl, (RAM_EllipseLastPointError)
      inc hl
      add hl, bc
      or a
      sbc hl, de
      jp p, +
      or a
      sbc hl, bc
      or a
      sbc hl, de
      ld (RAM_EllipseLastPointError), hl
      dec de
      ld hl, (RAM_EllipseCurrentX)
      dec hl
      ld (RAM_EllipseCurrentX), hl
      jp ++

+:    add hl, bc
      add hl, de
      ld (RAM_EllipseLastPointError), hl
      inc bc
      ld hl, (RAM_EllipseCurrentY)
      inc hl
      ld (RAM_EllipseCurrentY), hl
++:   ld a, b
      or c
      jp nz, -

      ; Third segment: lower-left quadrant
-:    call Ellipse_DrawPoint
      ld hl, (RAM_EllipseLastPointError)
      inc hl
      add hl, bc
      or a
      adc hl, de
      jp m, +
      or a
      sbc hl, bc
      add hl, de
      ld (RAM_EllipseLastPointError), hl
      inc de
      ld hl, (RAM_EllipseCurrentX)
      inc hl
      ld (RAM_EllipseCurrentX), hl
      jp ++
+:    or a
      sbc hl, de
      add hl, bc
      ld (RAM_EllipseLastPointError), hl
      inc bc
      ld hl, (RAM_EllipseCurrentY)
      inc hl
      ld (RAM_EllipseCurrentY), hl
++:   ld a, d
      or e
      jp nz, -
      
      ; Fourth segment: lower-right quadrant
-:    call Ellipse_DrawPoint
      ld hl, (RAM_EllipseLastPointError)
      inc hl
      add hl, de
      or a
      sbc hl, bc
      jp p, +
      add hl, bc
      add hl, de
      ld (RAM_EllipseLastPointError), hl
      inc de
      ld hl, (RAM_EllipseCurrentX)
      inc hl
      ld (RAM_EllipseCurrentX), hl
      jp ++
+:    or a
      sbc hl, de
      or a
      sbc hl, bc
      ld (RAM_EllipseLastPointError), hl
      dec bc
      ld hl, (RAM_EllipseCurrentY)
      dec hl
      ld (RAM_EllipseCurrentY), hl
++:   ld a, b
      or c
      jp nz, -
      ret

DrawEllipse_Wide:
      ; Inputs:
      ; a' = pen style
      ; b = minor radius
      ; de = centre coordinates
      ; hl = ratio of radii (ry/rx), *256
      ; This works much like drawing a circle internally, but it also has some logic to only decrement y 1/(radius ratio) of the time, thus producing a wide ellipse.
      ld a, $7F ; 127/255 = highest positive value, roughly +0.5. We subtract the ratio from this each time we decrement y, and only decrement the screen y when it carries.
      ld (RAM_EllipseRatio+1), a ; We use the high byte as an accumulator, intially set to just under 0.5. The low byte is always kept. We only draw when its value is negative.
      ld l, e ; centre y
      ld h, 0
      ld (RAM_EllipseCurrentY), hl
      ld l, d
      ld e, b ; de = radius = ellipse x
      ld d, h
      add hl, de ; centre x + radius
      ld (RAM_EllipseCurrentX), hl
      ld bc, 0 ; Ellipse y
      ld hl, 0
      ld (RAM_EllipseLastPointError), hl

      ; First quadrant
--:   call Ellipse_DrawPoint
-:    ld hl, (RAM_EllipseLastPointError) ; Error + 1 - x - y < 0?
      inc hl
      or a
      sbc hl, de
      or a
      sbc hl, bc
      jp m, +
      ; Not less than 0, so:
      add hl, bc
      or a
      sbc hl, de
      ld (RAM_EllipseLastPointError), hl ; Error += 1 - 2x
      dec de ; --x
      ld hl, (RAM_EllipseCurrentX)
      dec hl
      ld (RAM_EllipseCurrentX), hl
      jp ++
+:    ; Less than 0, so:
      add hl, de
      or a
      sbc hl, bc
      ld (RAM_EllipseLastPointError), hl ; Error += 1 - 2y
      dec bc ; --y

      ; Check whether to change the screen y...
      ld hl, (RAM_EllipseRatio) ; Subtract the radius ratio from the accumulator
      ld a, h
      sub l
      ld (RAM_EllipseRatio+1), a
      jp nc, +

      ; When the accumulator hits 0, we move the screen y. (We don't reset the accumulator, so we effectively add 1.0 to it.)
      ld hl, (RAM_EllipseCurrentY)
      dec hl
      ld (RAM_EllipseCurrentY), hl
      ; Fall through

++:   ld a, d ; Check for x = 0
      or e
      jp nz, -- ; Loop until it is (and draw)
      jp ++ ; When it hits zero, this quadrant is finished

+:    ld a, d ; Check for x = 0 again - this time in "do not draw" mode
      or e
      jp nz, - ; Loop until it is, but do not draw the current point
      jp + ; When it hits zero, we move on to the next quadrant, but do not draw the current point

++:   ; Second quadrant - same as before, only tweaked
--:    call Ellipse_DrawPoint
-:
+:    ld hl, (RAM_EllipseLastPointError)
      inc hl
      add hl, bc
      or a
      sbc hl, de
      jp p, +
      or a
      sbc hl, bc
      or a
      sbc hl, de
      ld (RAM_EllipseLastPointError), hl
      dec de
      ld hl, (RAM_EllipseCurrentX)
      dec hl
      ld (RAM_EllipseCurrentX), hl
      jp ++
+:    add hl, bc
      add hl, de
      ld (RAM_EllipseLastPointError), hl
      inc bc
      ld hl, (RAM_EllipseRatio)
      ld a, h
      add a, l
      ld (RAM_EllipseRatio+1), a
      jp nc, +
      ld hl, (RAM_EllipseCurrentY)
      inc hl
      ld (RAM_EllipseCurrentY), hl
++:   ld a, b
      or c
      jp nz, --
      ld hl, RAM_EllipseRatio+1
      inc (hl)
      jp ++
  +:  ld a, b
      or c
      jp nz, -
      ld hl, RAM_EllipseRatio+1
      inc (hl)
      jp +

++:   ; Third quadrant
--:   call Ellipse_DrawPoint
+:
-:    ld hl, (RAM_EllipseLastPointError)
      inc hl
      add hl, bc
      or a
      adc hl, de
      jp m, +
      or a
      sbc hl, bc
      add hl, de
      ld (RAM_EllipseLastPointError), hl
      inc de
      ld hl, (RAM_EllipseCurrentX)
      inc hl
      ld (RAM_EllipseCurrentX), hl
      jp ++

  +:  or a
      sbc hl, de
      add hl, bc
      ld (RAM_EllipseLastPointError), hl
      inc bc
      ld hl, (RAM_EllipseRatio)
      ld a, h
      add a, l
      ld (RAM_EllipseRatio+1), a
      jp nc, +
      ld hl, (RAM_EllipseCurrentY)
      inc hl
      ld (RAM_EllipseCurrentY), hl
++:   ld a, d
      or e
      jp nz, --
      jp ++

+:    ld a, d
      or e
      jp nz, -
      jp +

++:   ; Fourth quadrant
--:   call Ellipse_DrawPoint
-:
+:    ld hl, (RAM_EllipseLastPointError)
      inc hl
      add hl, de
      or a
      sbc hl, bc
      jp p, +
      add hl, bc
      add hl, de
      ld (RAM_EllipseLastPointError), hl
      inc de
      ld hl, (RAM_EllipseCurrentX)
      inc hl
      ld (RAM_EllipseCurrentX), hl
      jp ++
+:    or a
      sbc hl, de
      or a
      sbc hl, bc
      ld (RAM_EllipseLastPointError), hl
      dec bc
      ld hl, (RAM_EllipseRatio)
      ld a, h
      sub l
      ld (RAM_EllipseRatio+1), a
      jp nc, +
      ld hl, (RAM_EllipseCurrentY)
      dec hl
      ld (RAM_EllipseCurrentY), hl
++:   ld a, b
      or c
      jp nz, --
      ret
+:    ld a, b
      or c
      jp nz, -
      ret

DrawEllipse_Tall:
      ; Presumably similar to wide ellipses...
      push bc
        push de
          ld l, b
          ld de, (RAM_EllipseRatio)
          call Multiply_l_e_hl
          ld a, l
          ld b, h
          add a, $80
          jp nc, +
          inc b
+:      pop de
        ld (RAM_EllipseRatio+1), a
        ld l, e
        ld h, $00
        ld (RAM_EllipseCurrentY), hl
        ld l, d
        ld e, b
        ld d, h
        add hl, de
        ld (RAM_EllipseCurrentX), hl
      pop de
      ld e, d
      ld d, 0
      ld bc, 0
      ld hl, 0
      ld (RAM_EllipseLastPointError), hl
--:   call Ellipse_DrawPoint
-:    ld hl, (RAM_EllipseLastPointError)
      inc hl
      or a
      sbc hl, de
      or a
      sbc hl, bc
      jp m, +
      add hl, bc
      or a
      sbc hl, de
      ld (RAM_EllipseLastPointError), hl
      dec de
      ld hl, (RAM_EllipseRatio)
      ld a, h
      sub l
      ld (RAM_EllipseRatio+1), a
      jp nc, +++
      ld hl, (RAM_EllipseCurrentX)
      dec hl
      ld (RAM_EllipseCurrentX), hl
      jp ++
+:    add hl, de
      or a
      sbc hl, bc
      ld (RAM_EllipseLastPointError), hl
      dec bc
      ld hl, (RAM_EllipseCurrentY)
      dec hl
      ld (RAM_EllipseCurrentY), hl
++:   ld a, d
      or e
      jp nz, --
      ld hl, RAM_EllipseRatio+1
      dec (hl)
      jp ++
+++:  ld a, d
      or e
      jp nz, -
      ld hl, RAM_EllipseRatio+1
      dec (hl)
      jp +

++:   ; Second quadrant
--:   call Ellipse_DrawPoint
+:
-:    ld hl, (RAM_EllipseLastPointError)
      inc hl
      add hl, bc
      or a
      sbc hl, de
      jp p, +
      or a
      sbc hl, bc
      or a
      sbc hl, de
      ld (RAM_EllipseLastPointError), hl
      dec de
      ld hl, (RAM_EllipseRatio)
      ld a, h
      sub l
      ld (RAM_EllipseRatio+1), a
      jp nc, +++
      ld hl, (RAM_EllipseCurrentX)
      dec hl
      ld (RAM_EllipseCurrentX), hl
      jp ++
+:    add hl, bc
      add hl, de
      ld (RAM_EllipseLastPointError), hl
      inc bc
      ld hl, (RAM_EllipseCurrentY)
      inc hl
      ld (RAM_EllipseCurrentY), hl
++:   ld a, b
      or c
      jp nz, --
      jp ++
+++:  ld a, b
      or c
      jp nz, -
      jp +

++:   ; Third quadrant
--:   call Ellipse_DrawPoint
-:
+:    ld hl, (RAM_EllipseLastPointError)
      inc hl
      add hl, bc
      or a
      adc hl, de
      jp m, +
      or a
      sbc hl, bc
      add hl, de
      ld (RAM_EllipseLastPointError), hl
      inc de
      ld hl, (RAM_EllipseRatio)
      ld a, h
      add a, l
      ld (RAM_EllipseRatio+1), a
      jp nc, +++
      ld hl, (RAM_EllipseCurrentX)
      inc hl
      ld (RAM_EllipseCurrentX), hl
      jp ++
+:    or a
      sbc hl, de
      add hl, bc
      ld (RAM_EllipseLastPointError), hl
      inc bc
      ld hl, (RAM_EllipseCurrentY)
      inc hl
      ld (RAM_EllipseCurrentY), hl
++:   ld a, d
      or e
      jp nz, --
      ld hl, RAM_EllipseRatio+1
      inc (hl)
      jp ++
+++:  ld a, d
      or e
      jp nz, -
      ld hl, RAM_EllipseRatio+1
      inc (hl)
      jp +

++:   ; Fourth quadrant
--:   call Ellipse_DrawPoint
+:
-:    ld hl, (RAM_EllipseLastPointError)
      inc hl
      add hl, de
      or a
      sbc hl, bc
      jp p, +
      add hl, bc
      add hl, de
      ld (RAM_EllipseLastPointError), hl
      inc de
      ld hl, (RAM_EllipseRatio)
      ld a, h
      sub l
      ld (RAM_EllipseRatio+1), a
      jp nc, +++
      ld hl, (RAM_EllipseCurrentX)
      inc hl
      ld (RAM_EllipseCurrentX), hl
      jp ++
+:    or a
      sbc hl, de
      or a
      sbc hl, bc
      ld (RAM_EllipseLastPointError), hl
      dec bc
      ld hl, (RAM_EllipseCurrentY)
      dec hl
      ld (RAM_EllipseCurrentY), hl
++:   ld a, b
      or c
      jp nz, --
      ret
+++:  ld a, b
      or c
      jp nz, -
      ret

Ellipse_DrawPoint:
    ; Inputs:
    ; RAM_EllipseCurrentX
    ; RAM_EllipseCurrentY
    ; RAM_CircleEllipseCentre
    ; Draws a point at X,Y using the current pen, or a line from X,Y to the centre
    ; Truncates values outside the 8-bit range. These are outside the canvas anyway, so the work to draw them
    ; is not saved, but it avoids dumb truncation from drawing into unwanted places on the canvas. You can see this if you
    ; draw a large filled circle that would overlap 0,0 or 256,0 - the line rotation speed fluctuates as the destination
    ; points get truncated, causing them either to snap to somewhere away from the radius being drawn. This doesn't seem to
    ; cause any artefacts.
    push bc
    push de
    push hl
      ld hl, (RAM_EllipseCurrentX)
      ; If hl<0, we set l=0
      ; If hl>255, we set l=255
      ; Else we leave l alone (it's the true value in the range 0..255)
      ld a, h
      or a
      jp z, ++  ; Nothing to do when high byte is zero
      xor a     ; Set l=0...
      bit 7, h  ; ...unless it's positive...
      ld h, a   ; (This is unused)
      jp nz, +
      cpl       ; ...in which case set it to 255
+:    ld l, a
++:   ld de, (RAM_EllipseCurrentY)
      ; Same for Y
      ld a, d
      or a
      jp z, ++
      xor a
      bit 7, d
      ld d, a
      jp nz, +
      cpl
+:    ld e, a
++:   ex af, af'
        ; Check for line or fill mode
        or a
        jp nz, +
      ex af, af'
      ; Line mode: draw using dots
      ld a, l ; Could just ld h,l; ld l,e
      ld l, e ; x
      ld h, a ; y
      call DrawPenDot
    pop hl
    pop de
    pop bc
    ret

; push/pop matching, ignore
.endasm
push af
push af
push af
.asm

+:    ex af, af'
      ; Fill mode: draw a line from the centre to the point
      ld d, l ; de = x1,y1
      ld hl, (RAM_CircleEllipseCentre) ; hl = x2,y2
      call DrawLine
    pop hl
    pop de
    pop bc
    ret

; Unused function? $25dd
Multiply_bc_de_hl:
    ; Inputs: bc, de
    ; Outputs: hl = bc * de
    ; Trashes a, bc
    ld a, 16  ; Bits
    ld hl, 0  ; Accumulator
-:  add hl, hl ; Shift accumulator left
    rl c      ; Shift bc left
    rl b
    jr nc, +   ; If no carry out, nothing to do
    add hl, de ; Else add de
    jr nc, +   ; If that carried, increment bc (I guess this is necessary)
    inc bc
+:  dec a     ; Loop over 16 bits
    jp nz, -
    ret

Reciprocal_de_a:
    ; Inputs: de = fixed-point number
    ; Outputs: a = 1/(that number)
    ; Trashes hl, bc
    ld hl, $0100 ; 1.0
    ld b, 8     ; Bits
    xor a       ; Accumulator
-:  add a, a    ; Shift accumulator left
    inc a       ; Put a bit in on the right
    add hl, hl
    sbc hl, de
    jp nc, +
    dec a
    add hl, de
+:  djnz -
    ret
.ends

.section "Flood filling implementation" force
; 9th entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode8_PaintFunction:
      ; Wait for the beep to finish
      ld a, (RAM_Beep)
      or a
      ret nz
      ; Wait for the action state flags to be non-zero
      ld a, (RAM_ActionStateFlags)
      or a
      ret z
      ; Convert the selected point to canvas coordinates, taking into account the cursor size. Do nothing if out of bounds.
      ld de, (RAM_PaintStartingPoint)
      ld a, d
      sub DRAWING_AREA_MIN_X_PIXELS+4
      ld d, a
      ld a, e
      sub DRAWING_AREA_MIN_Y_PIXELS+4
      cp DRAWING_AREA_HEIGHT_PIXELS
      jp nc, +
      ld h, a
      ld a, d
      cp DRAWING_AREA_WIDTH_PIXELS
      jp nc, +
      ld l, a
      ex de, hl
      ; Get the selected pixel colour
      di
        call GetPixelColour
      ei
      ld a, (RAM_SelectedPixelColour)
      ld b, a
      ; If we are flood filling the same colour, then there's nothing to do
      ld a, (RAM_DrawingData.CurrentlySelectedPaletteIndex)
      cp b
      jp z, +
      ; Use the thin pen
      xor a
      ld (RAM_DrawingData.PenStyleForCurrentShape), a
      ; Do it...
      di
        call FloodFill
      ei
+:    ; Clear the action state
      xor a
      ld (RAM_ActionStateFlags), a
      ret

FloodFill:
    ; Inputs:
    ; de = pixel x,y on canvas
    ; RAM_SelectedPixelColour = colour at that location ("From" colour)
    ; RAM_DrawingData.CurrentlySelectedPaletteIndex = colour to replace it with ("To" colour)
    ; Finds all pixels of the same colour that are connected by an edge, and changes their colour.
    ; Algorithm:
    ; 1. Move left until you find the last pixel of the "From" colour
    ; 2. If the pixel above is the "From" colour, and is either at the left edge or the one to its left 
    ;    is a different colour, push its location onto the stack
    ; 3. If the pixel below is the "From" colour, and is either at the left edge or the one to its left 
    ;    is a different colour, push its location onto the stack
    ; 4. Set this pixel to the "To" colour
    ; 5. If the pixel to the right is the "From" colour, move right and go to #1
    ; 6. Pop a location from the stack and go to #1
    ; Uses a lot of stack. Presumably one could blow the stack with a carefully crafted picture.
    push af
    push bc
    push de
    push hl
      ld a, d ; pixel y
      cp DRAWING_AREA_HEIGHT_PIXELS
      jp nc, FloodFill_Done
      ld a, e ; pixel x
      ld (RAM_FloodFillXY.x), a
      ld a, d ; pixel y
      ld (RAM_FloodFillXY.y), a

      ; If the selected pixel is already our colour, there's nothing to do
      ; (We already ruled this out...)
      call FloodFill_DoesPixelMatch
      or a
      jp nz, FloodFill_Done

      ld hl, 0
      ld (RAM_FloodFill_StackCounter), hl
---:
-:    ; Move left until we find the leftmost pixel
      ld a, (RAM_FloodFillXY.x)
      or a
      jr z, +
      dec a
      ld e, a
      ld a, (RAM_FloodFillXY.y)
      ld d, a
      call FloodFill_DoesPixelMatch
      or a
      jr nz, +
      ld a, e
      ld (RAM_FloodFillXY.x), a
      jr -

+:    ; Initialise variables so the "previous" values are sensible
      ld a, 1
      ld (RAM_FloodFill_PixelAboveMatches), a
      ld (RAM_FloodFill_PixelBelowMatches), a
-:    ; Copy last colour check results to "previous" values
      ld a, (RAM_FloodFill_PixelAboveMatches)
      ld (RAM_FloodFill_PreviousPixelAboveMatches), a
      ld a, (RAM_FloodFill_PixelBelowMatches)
      ld (RAM_FloodFill_PreviousPixelBelowMatches), a
      ; Does the pixel below match? Say no if we are at the bottom
      ld a, (RAM_FloodFillXY.y)
      cp DRAWING_AREA_HEIGHT_PIXELS-1
      ld a, 1
      jr z, +
      ld a, (RAM_FloodFillXY.x)
      ld e, a
      ld a, (RAM_FloodFillXY.y)
      inc a
      ld d, a
      call FloodFill_DoesPixelMatch
+:    ld (RAM_FloodFill_PixelBelowMatches), a
      ; Does the pixel above match? Say no if we are at the top
      ld a, (RAM_FloodFillXY.y)
      cp 0 ; optimise: or a
      ld a, 1
      jr z, +
      ld a, (RAM_FloodFillXY.x)
      ld e, a
      ld a, (RAM_FloodFillXY.y)
      dec a
      ld d, a
      call FloodFill_DoesPixelMatch
+:    ld (RAM_FloodFill_PixelAboveMatches), a
      ; Check if:
      ; - The previous pixel above didn't match
      ; - AND the current pixel above does
      ; That means we found the left edge of a "fork".
      ld a, (RAM_FloodFill_PreviousPixelAboveMatches)
      ld b, a
      ld a, (RAM_FloodFill_PixelAboveMatches)
      xor 1
      and b
      jr z, +
      ; We found a fork. We push its location onto the stack so we can come back to it.
      ld hl, (RAM_FloodFill_StackCounter)
      inc hl
      ld (RAM_FloodFill_StackCounter), hl
      ld a, (RAM_FloodFillXY.x)
      ld l, a
      ld a, (RAM_FloodFillXY.y)
      dec a
      ld h, a
      push hl
.endasm
      pop hl ; For push/pop matching
.asm

+:    ; Then check for "forks" below as well
      ld a, (RAM_FloodFill_PreviousPixelBelowMatches)
      ld b, a
      ld a, (RAM_FloodFill_PixelBelowMatches)
      xor 1
      and b
      jr z, +
      ; We found a fork. We push its location onto the stack so we can come back to it.
      ld hl, (RAM_FloodFill_StackCounter)
      inc hl
      ld (RAM_FloodFill_StackCounter), hl
      ld a, (RAM_FloodFillXY.x)
      ld l, a
      ld a, (RAM_FloodFillXY.y)
      inc a
      ld h, a
      push hl
.endasm
      pop hl ; For push/pop matching
.asm
+:    ld a, (RAM_FloodFillXY.x)
      ld e, a
      ld a, (RAM_FloodFillXY.y)
      ld d, a
      ld a, 1 ; Unused
      call FloodFill_DrawPixel ; Set the pixel
      ; If we've got to the right margin, go check for more work to do
      ld a, (RAM_FloodFillXY.x)
      cp DRAWING_AREA_WIDTH_PIXELS-1
      jr z, FloodFill_EndOfRow
      ; Else, check the pixel to the right
      inc a
      ; If the colour doesn't match, we have finished this run
      ld e, a
      ld a, (RAM_FloodFillXY.y)
      ld d, a
      call FloodFill_DoesPixelMatch
      or a
      jr nz, FloodFill_EndOfRow
      ; Else move right and loop
      ld a, (RAM_FloodFillXY.x)
      inc a
      ld (RAM_FloodFillXY.x), a
      jp -

FloodFill_EndOfRow:
      ; If we've finished recursing, we are done
      ld hl, (RAM_FloodFill_StackCounter)
      ld a, h
      or l
      jr z, FloodFill_Done
      
      ; Else pop one of the locations we pushed earlier (if any) and check if it still needs to be filled
.endasm
      push hl ; For push/pop matching
.asm
      pop hl
      ld a, l
      ld (RAM_FloodFillXY.x), a
      ld a, h
      ld (RAM_FloodFillXY.y), a
      ld hl, (RAM_FloodFill_StackCounter)
      dec hl
      ld (RAM_FloodFill_StackCounter), hl
      ; We check the pixel colour again, in case we already got to this area
      ld a, (RAM_FloodFillXY.x)
      ld e, a
      ld a, (RAM_FloodFillXY.y)
      ld d, a
      call FloodFill_DoesPixelMatch
      or a
      jr nz, FloodFill_EndOfRow
      ; Else start again
      ; TODO: this includes looking left from X,Y to see if we have the leftmost point, but is that necessary? We only captured left edges...
      jp ---

FloodFill_Done:
    pop hl
    pop de
    pop bc
    pop af
    ret

FloodFill_DoesPixelMatch:
    ; Inputs: de = x,y location in canvas
    ; Reads the pixel at RAM_FloodFillXY and returns 0 if the pixel matches the originally selected pixel, 1 if not
    ; This seems terribly inefficient!
    push bc
    push de
    push hl
      push de
        call GetPixelColour_GetRowColourMismatchBitmask
        ex af, af' ; Save it
      pop de
      ; We want to check the state of the bit corresponding to the x coordinate...
      ld a, e ; Pixel x
      cpl     ; Calculate 7 - (x % 8) = number of pixels in the row to the right of x (0..7)
      and 7
      ld c, a
      ex af, af' ; Get the bitmask back
      ; Rotate left through carry 8 - (7 - x % 8) times. That's the same as 1 + x % 8, so the relevant bit is the last to go into carry.
      inc c
      dec c
      jp z, Rotates+0
      dec c
      jp z, Rotates+1
      dec c
      jp z, Rotates+2
      dec c
      jp z, Rotates+3
      dec c
      jp z, Rotates+4
      dec c
      jp z, Rotates+5
      dec c
      jp z, Rotates+6
      jp Rotates+7
Rotates:
.rept 8
      rlca
.endr
    pop hl
    pop de
    pop bc
    ; Then we examine the carry, which is the same as the LSB, and set a to 1 or 0 accordingly.
    ld a, 0
    ret nc
    ld a, 1
    ret

GetPixelColour:
    ; Inputs: de = x,y location
    ; Outputs: RAM_SelectedPixelColour holds the colour (palette index) of that pixel
    ; Trashes: hl, bc, af (notably, not de)
    push de
      call GetPixelColour_LoadTileRow
      ld a, l ; pixel x
      ld hl, RAM_TileModificationBuffer
      push hl
        and $07 ; Modulo 8
        ; Get the relevant bitmask in d
        ld de, Table_BitInPixelRowFromX
        LD_HL_A
        add hl, de
        ld d, (hl)
      pop hl
      ld c, 0 ; We accumulate the bits frmo the bitplanes into c
      ld a, (hl) ; Bitplane 1
      and d
      jp z, +
      set 0, c
+:    inc hl
      ld a, (hl) ; Bitplane 2
      and d
      jp z, +
      set 1, c
+:    inc hl
      ld a, (hl) ; Bitplane 3
      and d
      jp z, +
      set 2, c
+:    inc hl
      ld a, (hl) ; Bitplane 4
      and d
      jp z, +
      set 3, c
+:    ld a, c
      ld (RAM_SelectedPixelColour), a ; Save it
    pop de
    ret

GetPixelColour_GetRowColourMismatchBitmask:
    ; Inputs: de = x,y location in canvas
    ; Returns a bitmask in a, containing 0 for each pixel in the tile row containing x,y which matches the originally selected pixel
    ; Trashes everything but bc?
    ; Load the data
    call GetPixelColour_LoadTileRow
    ; Analyse it
    ld a, (RAM_SelectedPixelColour)
    call GetPixelColour_GetRowColourMatchBitmask
    ; Invert the logic so 1 = mismatch
    cpl
    ret

GetPixelColour_GetRowColourMatchBitmask:
    ; Returns a bitmask in a, containing 1 for each pixel in RAM_TileModificationBuffer which is of the given colour
    ld hl, RAM_TileModificationBuffer
    push bc
      ld b, 4 ; Bitplane count
      ld a, (RAM_SelectedPixelColour)
      or a
      jp z, ++ ; Special case zero
      ld c, a
      ; First we check for matching zeroes
      push bc
      push hl
        xor a     ; Initialise result
-:      rrc c     ; Get a bit out
        jp c, +   ; If it's a 0, then we OR the bitplane, as that will set the bit for mismatches
        or (hl)
+:      inc hl
        djnz -
      pop hl
      pop bc
      cpl         ; Invert
      ld d, a     ; Save
      ; Then we check for matching ones
      ld a, $FF   ; Initialise result again
-:    rrc c       ; Get a bit out
      jp nc, +    ; If it's a 1, then we AND the bitplane, as that will unset the bit for mismatches
      and (hl)
+:    inc hl
      djnz -
      and d       ; Then we combine the two halves
      jp +

++:   push hl
        ; We OR all four bytes together. Any non-zero pixel will become 1.
        xor a
        or (hl)
        inc hl
        or (hl)
        inc hl
        or (hl)
        inc hl
        or (hl)
      pop hl
      cpl ; We invert the bits
+:  pop bc
    ret

GetPixelColour_LoadTileRow:
    ; Inputs: de = x,y location in canvas
    ; Outputs: RAM_TileModificationBuffer is filled from the appropriate row and a is the pixel offset within it
    ; Trashes: af, hl, bc
    ld a, d ; y
    and $F8 ; Round down
    ld h, 0 ; Multiply by 88 to make a tile row address
    ld l, a
    add hl, hl
    add hl, hl
    add hl, hl
    push hl
      add hl, hl
      push hl
        add hl, hl
        add hl, hl
      pop bc
      add hl, bc
    pop bc
    add hl, bc
    push hl
      ld a, e ; x
      and $F8 ; Round down
      LD_HL_A
      add hl, hl ; Multiply by 4 to make an offset within the row
      add hl, hl
    pop bc
    ; Add it on to tmake the tile address
    add hl, bc
    ld a, d ; y
    and $07 ; Row within tile
    add a, a
    add a, a
    ld c, a
    ld b, 0
    add hl, bc
    ex de, hl ; now de = address of row data
    ; Read it into RAM
    push hl
    push de
    push bc
      ld hl, RAM_TileModificationBuffer
      ld bc, 4
      call CopyVRAMToRAM
    pop bc
    pop de
    pop hl
    ld a, l ; x
    and $F8 ; clear low 3 bits
    or l ; set them again?!
    cpl ; Invert
    and $07 ; Mask to just the low 3 bits. Could just ld a,l; and $07? Does the calling code even care?
    ret

FloodFill_DrawPixel:
    ; Draws into pixel at de
    push de
    push hl
    push bc
      ld a, d
      ld (RAM_DrawingData.PixelYToDraw), a
      ld a, e
      ld (RAM_DrawingData.PixelXToDraw), a
      call DrawPixel
    pop bc
    pop hl
    pop de
    ret
.ends

.section "Copy implementation" force
; 10th entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode9_CopyFunction:
      exx
    ; Wait for the signal that there is work to be done
    ld a, (RAM_ActionStateFlags)
    bit 3, a
    jp z, NotLocal_LABEL_3932_ ; If not, ???
    ; Wait for the beep to end
    ld a, (RAM_Beep)
    or a
    ret nz

    ; Get dimensions
    ld ix, $C15D
    ld a, (RAM_Copy_FirstPoint.y)
    sub 23
    ld d, a
    ld ($C15E), a ; Source Y
    ld a, (RAM_Copy_SecondPoint.y)
    sub 23
    sub d
    inc a
    ld ($C160), a ; Height in pixels
    ld a, (RAM_Copy_FirstPoint.x)
    sub 40
    ld ($C15F), a ; Source X
    ld e, a
    ld a, (RAM_Copy_SecondPoint.x)
    sub 40
    sub e
    inc a
    ld ($C161), a ; Width in pixels

    ld hl, RAM_GraphicsDataBuffer
    ld ($C171), hl ; Buffer pointer
    ld a, (RAM_Copy_Destination.y)
    sub 23
    ld ($C162), a ; Destination y
    ld a, (RAM_Copy_Destination.x)
    sub 40
    ld (RAM_TitleScreenAndEndTimeout), a ; Destination X
    di
      bit 0, (ix+0)
      call z, NotLoca_LABEL_2BD8_
      ld b, (ix+3)
      ld c, (ix+4)
      ld a, (ix+1)
      and $07
      ld d, a
      ld a, (ix+2)
      and $07
      ld e, a
      ld h, (ix+5)
      ld l, (ix+6)
--:   push bc
      push de
      push hl
        ld b, c
-:      ld a, e
        and $07
        ld c, a
        ld a, l
        and $07
        sub c
        jp nc, +
        add a, $08
+:      ld ($C166), a
        push bc
          call NotLocal_LABEL_2AB1_
          call NotLocal_LABEL_2ADC_
        pop bc
        inc e
        inc l
        ld a, l
        cp $B0
        jp nc, +
        djnz -
+:    pop hl
      pop de
      pop bc
      inc d
      inc h
      ld a, h
      cp $90
      jp nc, +
      djnz --
+:  ei
    ld a, (RAM_ActionStateFlags)
    and $06
    ld (RAM_ActionStateFlags), a
    ret
.ends

.section "Mirror implementation" force
; 11th entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode10_MirrorFunction:
    exx
    xor a
    ld a, (RAM_ActionStateFlags)
    rra
    jp nc, NotLocal_LABEL_3ACE_
    rra
    ret nc
    rra
    jp nc, NotLocal_LABEL_3932_
    ld a, (RAM_Beep)
    or a
    ret nz
    ld ix, $C15D
    ld (ix+0), $00
    ld a, (ix+18)
    sub $17
    ld (ix+18), a
    ld a, (ix+19)
    sub $28
    ld (ix+19), a
    ld a, (RAM_Copy_FirstPoint.y)
    sub $17
    ld d, a
    ld ($C15E), a
    ld a, (RAM_Copy_SecondPoint.y)
    sub $17
    sub d
    inc a
    ld ($C160), a
    ld a, (RAM_Copy_FirstPoint.x)
    sub $28
    ld ($C15F), a
    ld e, a
    ld a, (RAM_Copy_SecondPoint.x)
    sub $28
    sub e
    inc a
    ld ($C161), a
    ld hl, RAM_GraphicsDataBuffer
    ld ($C171), hl
    ld a, (RAM_SubmenuSelectionIndex)
    or a
    jp nz, _LABEL_2A0B_
    ld hl, _LABEL_29B3_ ; will ret to here
    push hl
      ld a, ($C15E)
      cp (ix+18)
      jp nc, _LABEL_29A1_
      add a, (ix+3)
      cp (ix+18)
      jp nc, _LABEL_2994_
      ld a, ($C15E)
      add a, (ix+3)
      sub (ix+18)
      neg
      add a, (ix+18)
      ld ($C162), a
      ret

.endasm ; Unmatched push matching
pop hl
.asm

_LABEL_2994_:
      ld a, ($C16F)
      ld ($C162), a
      sub (ix+1)
      ld ($C160), a
      ret

_LABEL_29A1_:
      ld a, ($C15E)
      add a, (ix+3)
      sub (ix+18)
      ld b, a
      ld a, ($C16F)
      sub b
      ld ($C162), a
      ret

_LABEL_29B3_:
    di
      bit 0, (ix+0)
      call z, NotLoca_LABEL_2BD8_
      ld b, (ix+3)
      ld c, (ix+4)
      ld a, (ix+1)
      and $07
      add a, b
      dec a
      ld d, a
      ld a, (ix+2)
      and $07
      ld e, a
      ld h, (ix+5)
      ld l, (ix+2)
--:   push bc
      push de
      push hl
        ld a, h
        cp $90
        jp nc, ++
        ld b, c
-:      ld a, e
        and $07
        ld c, a
        ld a, l
        and $07
        sub c
        jp nc, +
        add a, 8
+:      ld ($c166), a
        push bc
          call NotLocal_LABEL_2AB1_
          call NotLocal_LABEL_2ADC_
        pop bc
        inc e
        inc l
        ld a, l
        cp $b0
        jp nc, ++
        djnz   -
++:   pop hl
      pop de
      pop bc
      dec d
      inc h
      djnz --
    jp _LABEL_2AA9_

_LABEL_2A0B_:
    ld hl, _LABEL_2A52_ ; will ret to here
    push hl
      ld a, ($C15F)
      cp (ix+19)
      jp nc, ++
      add a, (ix+4)
      cp (ix+19)
      jp nc, +
      ld a, ($C15F)
      add a, (ix+4)
      sub (ix+19)
      neg
      add a, (ix+19)
      ld (RAM_TitleScreenAndEndTimeout), a
      ret

+:    ld a, ($C170)
      ld (RAM_TitleScreenAndEndTimeout), a
      sub (ix+2)
      ld ($C161), a
      ret

++:   ld a, ($C15F)
      add a, (ix+4)
      sub (ix+19)
      ld b, a
      ld a, ($C170)
      sub b
      ld (RAM_TitleScreenAndEndTimeout), a
      ret

.endasm ; Unmatched push matching
pop hl
.asm

_LABEL_2A52_:
    di
      bit 0, (ix+0)
      call z, NotLoca_LABEL_2BD8_
      ld b, (ix+3)
      ld c, (ix+4)
      ld a, (ix+1)
      and $07
      ld d, a
      ld a, (ix+2)
      and $07
      add a, (ix+4)
      dec a
      ld e, a
      ld h, (ix+1)
      ld l, (ix+6)
--:   push bc
      push de
      push hl
        ld b, c
-:      ld a, e
        and $07
        ld c, a
        ld a, l
        and $07
        sub c
        jp nc, +
        add a, $08
+:      ld ($c166), a
        ld a, l
        cp $b0
        jp nc, +
        push bc
          call NotLocal_LABEL_2AB1_
          call NotLocal_LABEL_2ADC_
        pop bc
+:      dec e
        inc l
        djnz -
      pop hl
      pop de
      pop bc
      inc d
      inc h
      ld a, h
      cp $90
      jp nc, ++
      djnz --
_LABEL_2AA9_:
++: ei
    xor a
    ld ($c089), a
    jp EnableOnlyThreeSprites ; and ret

NotLocal_LABEL_2AB1_:
    push hl
    push de
      push bc
        ld a, e
        and $07
        ld c, a
        ld b, $00
        ld hl, _LABEL_2BD0_
        add hl, bc
        ld a, (hl)
      pop bc
      ld hl, $C167
      ld c, a
      call _LABEL_2BA0_
      ld a, (de)
      and c
      ld (hl), a
      inc hl
      inc de
      ld a, (de)
      and c
      ld (hl), a
      inc hl
      inc de
      ld a, (de)
      and c
      ld (hl), a
      inc hl
      inc de
      ld a, (de)
      and c
      ld (hl), a
    pop de
    pop hl
    ret

NotLocal_LABEL_2ADC_:
    call CheckForReset
    push hl
    push de
    push bc
      ld a, e
      ex af, af'
      ex de, hl
      call _LABEL_2B74_
      ld hl, $C16B
      VDP_ADDRESS_TO_DE
      push af
      pop af
      push de
        in a, (Port_VDPData)
        ld (hl), a
        inc hl
      pop de
      in a, (Port_VDPData)
      ld (hl), a
      inc hl
      push de
        in a, (Port_VDPData)
        ld (hl), a
        inc hl
      pop de
      in a, (Port_VDPData)
      ld (hl), a
      dec hl
      dec hl
      dec hl
      ex af, af'
      push hl
      push de
      push bc
        add a, (ix+9)
        and $07
        ld c, a
        ld b, $00
        ld hl, _LABEL_2BD0_
        add hl, bc
        ld a, (hl)
        cpl
      pop bc
      pop de
      pop hl
      ld c, a
      ld a, (hl)
      and c
      ld (hl), a
      inc hl
      ld a, (hl)
      and c
      ld (hl), a
      inc hl
      ld a, (hl)
      and c
      ld (hl), a
      inc hl
      ld a, (hl)
      and c
      ld (hl), a
      dec hl
      dec hl
      dec hl
      push hl
        push bc
          ld hl, $C167
          ld a, (ix+9)
          or a
          jp z, +
          ld b, a
-:        push hl
            rrc (hl)
            inc hl
            rrc (hl)
            inc hl
            rrc (hl)
            inc hl
            rrc (hl)
          pop hl
          djnz -
+:      pop bc
        push hl
        pop iy
      pop hl
      ld a, e
      out (Port_VDPAddress), a
      ld a, d
      or >VDPAddressMask_Write
      out (Port_VDPAddress), a
      ld a, (hl)
      or (iy+0)
      out (Port_VDPData), a
      inc hl
      ld a, (hl)
      or (iy+1)
      out (Port_VDPData), a
      inc hl
      ld a, (hl)
      or (iy+2)
      out (Port_VDPData), a
      inc hl
      ld a, (hl)
      or (iy+3)
      out (Port_VDPData), a
    pop bc
    pop de
    pop hl
    ret

_LABEL_2B74_:
    push hl
    push bc
      ld a, d
      and $F8
      LD_HL_A
      add hl, hl
      add hl, hl
      add hl, hl
      push hl
        add hl, hl
        push hl
          add hl, hl
          add hl, hl
        pop bc
        add hl, bc
      pop bc
      add hl, bc
      push hl
        ld a, e
        and $F8
        LD_HL_A
        add hl, hl
        add hl, hl
      pop bc
      add hl, bc
      ld a, d
      and $07
      add a, a
      add a, a
      ld b, $00
      ld c, a
      add hl, bc
      ex de, hl
    pop bc
    pop hl
    ret

_LABEL_2BA0_:
    push bc
    push hl
      ld a, d
      and $F8
      LD_HL_A
      add hl, hl
      add hl, hl
      push hl
        add hl, hl
        add hl, hl
        push hl
          add hl, hl
        pop bc
        add hl, bc
      pop bc
      add hl, bc
      push hl
        ld a, e
        and $F8
        LD_HL_A
        add hl, hl
        add hl, hl
      pop bc
      add hl, bc
      ld a, d
      and $07
      add a, a
      add a, a
      ld c, a
      ld b, $00
      add hl, bc
      ld bc, ($C171)
      add hl, bc
      ex de, hl
    pop hl
    pop bc
    ret

; Data from 2BD0 to 2BD7 (8 bytes)
_LABEL_2BD0_:
.db %10000000
.db %01000000
.db %00100000
.db %00010000
.db %00001000
.db %00000100
.db %00000010
.db %00000001

NotLoca_LABEL_2BD8_:
    ; Set the flag to say this function has been called
    set 0, (ix+0)
    push hl
    push de
    push bc
      ; Get the Y points
      ld a, (RAM_Copy_FirstPoint.y)
      sub 23
      ld d, a ; Y1
      ld a, (RAM_Copy_SecondPoint.y)
      sub 24
      ld e, a ; Y2
      sub d
      and $F8
      rrca
      rrca
      rrca
      inc a
      ld b, a ; Height in tiles
      ld a, d
      and $07
      jp z, +
      inc b
+:    ; Then the X points
      ld a, (RAM_Copy_FirstPoint.x)
      sub 40
      ld e, a ; X1
      ld a, (RAM_Copy_SecondPoint.x)
      sub 41
      ld d, a ; X2
      sub e
      and $F8
      rrca
      rrca
      rrca
      inc a
      ld c, a ; Width in tiles
      ld a, e
      and $07
      jp z, +
      inc c
+:    ld a, (ix+1) ; Source Y
      and $F8
      ld d, a
      ld e, (ix+2) ; Source X
      call _LABEL_2B74_
      ld hl, ($C171)
--:   VDP_ADDRESS_TO_DE
      push bc
        ld b, 0
        sla c
        rl b
        sla c
        rl b
        sla c
        rl b
        sla c
        rl b
        sla c
        rl b
        push hl
-:        in a, (Port_VDPData)
          ld (hl), a
          inc hl
          push af
          pop af
          dec bc
          ld a, b
          or c
          jp nz, -
          ld hl, $02C0
          add hl, de
          ex de, hl
        pop hl
        ld bc, $01A0
        add hl, bc
      pop bc
      djnz --
    pop bc
    pop de
    pop hl
    ret
.ends

.section "Magnify implementation" force
; 12th entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode11_MagnifyFunction:
    exx
    bit 7, (hl)
    jp z, _LABEL_2D05_
    call NotLocal_LABEL_3932_
    ld ix, $C15D
    ld a, (RAM_ActionStateFlags)
    bit 2, a
    ret z
    bit 6, a
    ld iy, $C172
    jp z, _LABEL_2D0D_
    bit 7, a
    jp nz, _LABEL_2CF6_
    ld a, (RAM_SpriteTable1.y)
    add a, $04
    cp (iy+1)
    ret c
    cp (iy+2)
    ret nc
    sub $18
    ld l, a
    ld a, (RAM_SpriteTable1.xn)
    add a, $03
    cp (iy+3)
    ret c
    cp (iy+4)
    ret nc
    sub $28
    ld h, a
    ld a, (RAM_ButtonsPressed)
    bit GraphicBoardButtonBit_Pen, a
    ret z
    di
    ld a, (RAM_PenStyle)
    cp PenStyle_Erase
    jp z, +
    ld a, PenStyle_Medium
+:  ld (RAM_DrawingData.PenStyleForCurrentShape), a
    push hl
      ld a, $24
      add a, h
      ld h, a
      ld a, $3C
      add a, l
      ld l, a
      call DrawPenDot
    pop hl
    ld a, l
    sub (ix+5)
    and $FE
    rrca
    add a, (ix+1)
    add a, $3C
    ld l, a
    ld a, h
    sub (ix+6)
    and $FE
    rrca
    add a, (ix+2)
    add a, $24
    ld h, a
    xor a
    ld (RAM_DrawingData.PenStyleForCurrentShape), a
    ld a, (RAM_DrawingData.CurrentlySelectedPaletteIndex)
    ld b, a
    push af
      ld a, (RAM_PenStyle)
      cp PenStyle_Erase
      jp nz, +
      ld b, PenStyle_Thin
+:    ld a, b
      ld (RAM_DrawingData.CurrentlySelectedPaletteIndex), a
      call DrawPenDot
    pop af
    ld (RAM_DrawingData.CurrentlySelectedPaletteIndex), a
    ei
    ret

_LABEL_2CF6_:
    di
      push hl
        call EnableOnlyThreeSprites
        call RestoreTileData
      pop hl
    ei
    ld a, 1
    ld (RAM_Beep), a
_LABEL_2D05_:
    set 7, (hl)
    ld a, $02
    ld (RAM_ActionStateFlags), a
    ret

_LABEL_2D0D_:
    ex af, af'
      ld a, (RAM_Beep)
      or a
      ret nz
    ex af, af'
    or $40
    ld (RAM_ActionStateFlags), a
    di
    ld a, ($C172)
    add a, a
    ld c, a
    add a, a
    add a, a
    add a, c
    ld c, a
    ld b, $00
    ld hl, $2F6A
    add hl, bc
    ld a, (hl)
    ld (iy+1), a
    inc hl
    ld a, (hl)
    ld (iy+2), a
    inc hl
    ld a, (hl)
    ld (iy+3), a
    inc hl
    ld a, (hl)
    ld (iy+4), a
    inc hl
    ld e, (hl)
    inc hl
    ld d, (hl)
    inc hl
    push de
      ; Pointer to text
      ld e, (hl)
      inc hl
      ld d, (hl)
      inc hl
      ; Location to draw it
      ld a, (hl)
      inc hl
      ld h, (hl)
      ld l, a
      LD_BC_AREA 9, 9
      call DrawTextToTilesWithBackup
    pop de
    ld h, TileAttribute_None
    LD_BC_AREA 8, 8
    call SetAreaTileAttributes
    ld a, (RAM_Copy_FirstPoint.y)
    sub $16
    ld d, a
    ld ($C15E), a
    ld a, (RAM_Copy_SecondPoint.y)
    sub $17
    sub d
    ld ($C160), a
    ld a, (RAM_Copy_FirstPoint.x)
    sub $27
    ld ($C15F), a
    ld e, a
    ld a, (RAM_Copy_SecondPoint.x)
    sub $28
    sub e
    ld ($C161), a
    ld hl, $D000
    ld ($C171), hl
    bit 0, (ix+0)
    call z, NotLoca_LABEL_2BD8_
    ld b, (ix+3)
    ld c, (ix+4)
    ld a, (ix+1)
    and $07
    ld d, a
    ld a, (ix+2)
    and $07
    ld e, a
    ld h, (ix+5)
    ld l, (ix+6)
--: push bc
    push de
    push hl
      ld b, c
-:    push bc
      push hl
        call _LABEL_2DED_
        call NotLocal_LABEL_2AB1_
        call NotLocal_LABEL_2ADC_
        inc l
        call _LABEL_2DED_
        call NotLocal_LABEL_2AB1_
        call NotLocal_LABEL_2ADC_
        dec l
        inc h
        call _LABEL_2DED_
        call NotLocal_LABEL_2AB1_
        call NotLocal_LABEL_2ADC_
        inc l
        call _LABEL_2DED_
        call NotLocal_LABEL_2AB1_
        call NotLocal_LABEL_2ADC_
      pop hl
      pop bc
      inc e
      inc l
      inc l
      ld a, l
      cp $B0
      jp nc, +
      djnz -
+:  pop hl
    pop de
    pop bc
    inc d
    inc h
    inc h
    ld a, h
    cp $90
    jp nc, +
    djnz --
+:  ei
    ret

_LABEL_2DED_:
    ld a, e
    and $07
    ld c, a
    ld a, l
    and $07
    sub c
    jp nc, +
    add a, $08
+:  ld ($C166), a
    ret

; Data from 2DFE to 2F91 (404 bytes)
MagnifyBoxData:
.db 10*9
.asc "        )$"
.asc "        )$"
.asc "        )$"
.asc "        )$"
.asc "        )$"
.asc "        )$"
.asc "        )$"
.asc "        )$"
.asc "########@$"
.db 10*9
.asc "********;$"
.asc "        )$"
.asc "        )$"
.asc "        )$"
.asc "        )$"
.asc "        )$"
.asc "        )$"
.asc "        )$"
.asc "        )$"
.db 10*9
.asc "(        $"
.asc "(        $"
.asc "(        $"
.asc "(        $"
.asc "(        $"
.asc "(        $"
.asc "(        $"
.asc "(        $"
.asc "%########$"
.db 10*9
.asc "~********$"
.asc "(        $"
.asc "(        $"
.asc "(        $"
.asc "(        $"
.asc "(        $"
.asc "(        $"
.asc "(        $"
.asc "(        $"
.ends

; TBC
.db $18 $57 $28 $67 $CB $38 
.dw $2DFE 
.db $00 $00 $68 $A7 $28 $67 $4B $3B $59 $2E $00 $09 $18 $57 $98 $D7 $E7 $38 $B4 $2E $0D $00 $68 $A7 $98 $D7 $67 $3B $0F $2F $0D $09

.section "Graphic board/sprite update handlers dispatcher" force
CallNonVBlankModeGraphicBoardHandler: ; Functions that deal with the pen position and buttons
    ld hl, RAM_ButtonsNewlyPressed ; used in functions later
    ld a, (RAM_Pen_Smoothed.y)
    ld b, a                        ; used in functions later
    ld a, (RAM_CurrentMode)
    and %00111111
    cp Mode12_Display
    ret z ; Do nothing
    cp Mode9_Copy
    jp c, +
    cp Mode11_Magnify
    jp c, ++
    cp Mode13_End
    jp z, ++
+:  ; Function 0-8 or 14+ only
    ld a, b ; Pen Y
    cp 47
    jp c, NotLocal_LABEL_36A5_ ; Less than 47 = near top of screen
++: ld a, b
    sub 40
    jp nc, +
    xor a ; Zero if it carried
+:  ld b, a
    ld a, $A8
    ld ($C241), a ; Write only?

    ld a, (RAM_CurrentMode)
    and %00111111
    exx
      ; shadow regs
      ld hl, ModeGraphicBoardHandlerJumpTable
      jp JumpToFunction

; 3rd entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
DoNothing:
    ret

ModeGraphicBoardHandlerJumpTable:
; Jump Table from 2FD0 to 2FF3 (18 entries, indexed by RAM_CurrentMode)
; Called inside shadow registers
; Non-shadow regs have b = pen Y, (hl) = buttons newly pressed, a = mode
.dw Mode0_DrawingGraphicBoardHandler
.dw Mode1_MenuGraphicBoardHandler
.dw DoNothing ; Menu button press
.dw Mode3_ColourGraphicBoardHandler
.dw DoNothing ; Erase
.dw Mode5_SquareGraphicBoardHandler
.dw Mode6_CircleGraphicBoardHandler
.dw Mode7_EllipseGraphicBoardHandler
.dw Mode8_PaintGraphicBoardHandler
.dw Mode9_CopyGraphicBoardHandler
.dw Mode10_MirrorGraphicBoardHandler
.dw Mode11_MagnifyGraphicBoardHandler
.dw DoNothing ; Display
.dw DoNothing ; End
.dw SubmenuGraphicBoardHandler
.dw SubmenuGraphicBoardHandler
.dw SubmenuGraphicBoardHandler
.dw SubmenuGraphicBoardHandler
.ends

; 1st entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
Mode0_DrawingGraphicBoardHandler:
      ; shadow regs
      call CheckMenuButton
    exx
    ; Set sprite 0 to the pen location and the crosshair sprite
    ld a, (RAM_Pen_Smoothed.x)
    ld (RAM_SpriteTable1.xn + 0), a
    ld a, b
    ld (RAM_SpriteTable1.y + 0), a
    xor a ; CursorIndex_Crosshair
    jp SetCursorIndex ; and ret

; 2nd entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
Mode1_MenuGraphicBoardHandler:
    exx
    ; Set sprite 0 to x = 88...
    ld a, 88
    ld (RAM_SpriteTable1.xn), a
    ; ...y  = pen Y rounded to 8px, in the range 64..152 (menu area)
    ld a, b ; Pen Y
    and %11111000 ; Round down to nearest 8px
    cp 64         ; If <64, set to 64
    jp nc, +
    ld a, 64
+:  cp 152
    jp c, +
    ld a, 152
+:  ld (RAM_SpriteTable1.y), a
    sub 64 ; Now it's relative to the menu area
    bit GraphicBoardButtonBit_Do, (hl) ; Check for DO button
    jp z, ++

    rrca ; Divide by 8 and add 2
    rrca
    rrca
    add a, 2 ; Now it's a mode index!
    cp Mode2_MenuItemSelected
    ld b, a
    jp z, +
    ld ($C03D), a ; For everything except mode 2, set this with what was selected and then go to mode 2
    ld a, Mode2_MenuItemSelected
+:  ld (RAM_CurrentMode), a ; Change mode based on pen location when DO is pressed
    ld a, b
    dec a ; Text index is mode - 1
    ld (RAM_StatusBarTextIndex), a

++: ld a, CursorIndex_MenuArrowRight
    jp SetCursorIndex ; and ret

; 4th entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
Mode3_ColourGraphicBoardHandler:
      call CheckMenuButton
    exx
    ld a, b ; Pen Y
    and $f0 ; Round down to nearest 16px
    cp 88 ; Colour section or page up/down section?
    jp nc, Mode3_ColourGraphicBoardHandler_LowerSection

    ; Already did this!
    and $f0

    ; We want to select one of the colour boxes, which means:
    ; 1. Truncate to the range of Y values they're in. We already did the upper limit.
    cp 64
    jr nc, +
    ld a, 64
+:  ld b, a
    dec a
    ld (RAM_SpriteTable1.y + 0), a ; That's our Y coordinate

    ; Now examine x, round down to 16px and limit to 112..160
    ld a, (RAM_Pen_Smoothed.x)
    and $F0
    cp 112
    jp nc, +
    ld a, 112
+:  cp 160
    jp c, +
    ld a, 160
+:  ld (RAM_SpriteTable1.xn + 0), a ; That's our X coordinate
    ld c, a
    ld a, CursorIndex_Square ; Highlight the colour
    call SetCursorIndex

    ; Check if Do button is pressed
    bit GraphicBoardButtonBit_Do, (hl)
    ret z

    ; Beep when pressed
    ld a, 1
    ld (RAM_Beep), a

    ld a, b ; Y coordinate
    ; Relative to the area of the colour boxes
    sub 64
    ; Divide by 4
    rrca
    rrca
    ld b, a
    ld a, c ; X coordinate
    ; Relative to area of the colour boxes
    sub 112
    ; Divide by 16
    rrca
    rrca
    rrca
    rrca
    ; Merge bits which makes it into a colour index in the range 0-8
    or b
    ld c, a

    ld a, (RAM_ColourSelectionStartValue) ; Offset from the right point, so now it's a palette value (!)
    add a, c
    and $3F
    ld c, a

    ld a, (RAM_SubmenuSelectionIndex) ; Are we setting the background or the selected palette entry?
    or a
    jp nz, +
    ; Selected palette entry: update RAM_Palette
    ld a, (RAM_DrawingData.CurrentlySelectedPaletteIndex)
    LD_DE_A
    ld hl, RAM_Palette
    add hl, de
    ld (hl), c
    ld a, 1
    ld (RAM_NeedToUpdatePalette), a
    ret

+:  ld a, c
    ld (RAM_Palette + 16), a ; Sprite palette index 0 = background colour
    ld a, 1
    ld (RAM_NeedToUpdatePalette), a
    ret

Mode3_ColourGraphicBoardHandler_LowerSection:
    ; Truncate range to 96..104. We are already a multiple of 16 so this gets us to the right 8px boundary.
    cp 96
    jp nc, +
    ld a, 96
+:  cp 104
    jp c, +
    ld a, 104
+:  dec a
    ld (RAM_SpriteTable1.y + 0), a ; Set cursor Y
    ex af, af' ; save value
    ld a, 88
    ld (RAM_SpriteTable1.xn + 0), a ; ...and X
    ld a, CursorIndex_MenuArrowRight
    call SetCursorIndex

    bit GraphicBoardButtonBit_Do, (hl) ; Check for Do button
    ret z

    ; If set...
    ex af, af' ; Restore Y value
    ld b, 4 ; Amount to offset for page down
    cp 96
    jp c, +
    ld b, -4 ; Amount to offset for page up
+:  ld a, (RAM_ColourSelectionStartValue)
    add a, b ; Offset
    and $3F
    ld (RAM_ColourSelectionStartValue), a
    ld a, 1
    ld (RAM_NeedToUpdatePalette), a
    ld a, 1
    ld (RAM_Beep), a
    ret

; 6th entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
Mode5_SquareGraphicBoardHandler:
      call CheckMenuButton
    exx
    ; Do nothing if we're drawing
    ld a, (RAM_ActionStateFlags)
    bit 1, a
    ret nz

    ld d, a ; RAM_ActionStateFlags
    ; Make the cursor follow the pen
    ld a, b ; Pen Y
    ld (RAM_SpriteTable1.y + 0), a
    ld a, (RAM_Pen_Smoothed.x)
    ld (RAM_SpriteTable1.xn + 0), a
    push hl
      bit 0, d ; Are we in phase 1 or 2?
      jp nz, +
      ; Phase 1 (first corner)
      ld a, CursorIndex_ArrowBottomRight
      call SetCursorIndex
    pop hl

    bit GraphicBoardButtonBit_Do, (hl) ; Wait for Do button
    ret z

    ld a, 1
    ld (RAM_Beep), a
    ld a, (RAM_SpriteTable1.y + 0)
    ld (RAM_SpriteTable1.y + 3), a
    ld a, (RAM_SpriteTable1.xn + 0)
    ld (RAM_SpriteTable1.xn + 3*2), a
    ld a, $A9 ; Second cursor tile index
    ld (RAM_SpriteTable1.xn + 3*2+1), a

    ; Offset position by 4, 3
    ld de, (RAM_Pen_Smoothed)
    ld a, 4
    add a, e
    ld e, a
    ld a, 3
    add a, d
    ld d, a
    ; ...and save here
    ld (RAM_DrawingData.SquareCorner1), de

    ; Set the flag to move on to the next stage
    ld a, (RAM_ActionStateFlags)
    set 0, a
    ld (RAM_ActionStateFlags), a

    ; Write into the second cursor
    ld a, SetCursorIndex_Second | CursorIndex_ArrowBottomRight
    jp SetCursorIndex ; and ret

.endasm ; Unmatched push matching
push hl
.asm

+:    ; Phase 2 (second corner)
      ld a, CursorIndex_ArrowTopLeft
      call SetCursorIndex
    pop hl

    bit GraphicBoardButtonBit_Do, (hl) ; Wait for Do button
    ret z

    ; Beep
    ld a, 1
    ld (RAM_Beep), a

    ld hl, (RAM_DrawingData.SquareCorner1)
    ; Calculate the width from the sprites' positions
    ld a, (RAM_SpriteTable1.xn + 3*2) ; x1
    ld b, a
    ld a, (RAM_SpriteTable1.xn + 0)   ; x2
    sub b
    sub 7 ; Account for sprite widths
    ld b, a
    add a, h ; Add to corner1.x. No need to fix overflow, since the maximum width is 256.
    ld (RAM_DrawingData.SquareCorner2_x), a
    ; Likewise for the height
    ld a, (RAM_SpriteTable1.y + 3) ; y1
    ld d, a
    ld a, (RAM_SpriteTable1.y) ; y2
    sub 7 ; Account for the first sprite's height
    sub d
    ld d, a
    push af
      add a, l
      ld (RAM_DrawingData.SquareCorner2_y), a
    pop af
    ld a, d ; unnecessary?
    jp nc, +
    neg ; fix overflow 
+:  ld (RAM_DrawingData.SquareHeight), a

    ; Juggle the corners if they are not top-left, bottom-right
    ld hl, (RAM_DrawingData.SquareCorner1)
    ld de, (RAM_DrawingData.SquareCorner2)
    ld a, l ; compare y
    cp e
    jp c, +
    ld b, l ; swap if wrong way round
    ld l, e
    ld e, b
+:  ld a, h ; compare x
    cp d
    jp c, + ; swap if wrong way round
    ld b, h
    ld h, d
    ld d, b
+:  ld (RAM_DrawingData.SquareCorner1), hl
    ld (RAM_DrawingData.SquareCorner2), de
    ; Enter "drawing mode"
    ld a, (RAM_ActionStateFlags)
    set 1, a
    ld (RAM_ActionStateFlags), a
    ret

; 8th entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
Mode7_EllipseGraphicBoardHandler:
      ; Set a' and clear z'
      ld a, 1
      and a
      ex af, af'
      jp +

; 7th entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
Mode6_CircleGraphicBoardHandler:
      ; Clear a' and set z'
      xor a
      ex af, af'

+:    ; Common code for circles and ellipses
      call CheckMenuButton
      ld a, CursorIndex_X
      call SetCursorIndex
      
      ld hl, RAM_ActionStateFlags ; 0 = get centre, 1 = get edge, 2+ = fill
      bit 0, (hl)
      jp z, _CircleEllipseGraphicBoardHandler_GetCentre
      bit 1, (hl)
      ret nz
    exx
    ; Get centre point Y
    ld a, (RAM_SpriteTable1.y + 3)
    ld c, a
    ex af, af'
    ; If we are in circle mode, use that for the cursor y; else, use the real y
    jp nz, +
    ld b, c 
+:  ex af, af'
    ; Update the cursor position
    ld a, b
    ld (RAM_SpriteTable1.y), a
    ld a, (RAM_Pen_Smoothed.x)
    ld (RAM_SpriteTable1.xn + 0), a

    ; Wait for the Do button
    bit GraphicBoardButtonBit_Do, (hl)
    ret z
    
    ld a, 1
    ld (RAM_Beep), a
    
    ; Get the x-difference of the two cursors
    ld a, (RAM_SpriteTable1.xn + 3*2)
    ld b, a
    ld a, (RAM_SpriteTable1.xn + 0)
    sub b
    jp nc, +
    neg
+:  ld (RAM_EllipseMinorRadius), a
    ld hl, $0100 ; Circle ratio = 1.0
    ld (RAM_EllipseRatio), hl
    ex af, af'
    call nz, _CircleEllipseGraphicBoardHandler_Ellipse
    exx
      ld (hl), $83 ; Go into drawing mode
      ret

_CircleEllipseGraphicBoardHandler_GetCentre:
    exx
    ld a, b
    ld (RAM_SpriteTable1.y), a
    ld a, (RAM_Pen_Smoothed.x)
    ld (RAM_SpriteTable1.xn + 0), a
    bit 1, (hl)
    ret z
    ld a, 1
    ld (RAM_Beep), a
    ; Fix sprite 3 where sprite 0 is
    ld a, (RAM_SpriteTable1.y + 0)
    ld (RAM_SpriteTable1.y + 3), a
    ld a, (RAM_SpriteTable1.xn + 0)
    ld (RAM_SpriteTable1.xn + 3*2), a
    ld a, $A9 ; Second cursor tile index
    ld (RAM_SpriteTable1.xn + 3*2+1), a

    ; Store the location
    ld de, (RAM_Pen_Smoothed)
    ld (RAM_CircleEllipseCentre), de

    ld a, SetCursorIndex_Second | CursorIndex_X
    call SetCursorIndex
    exx
    set 0, (hl)
    ret

_CircleEllipseGraphicBoardHandler_Ellipse:
    ; Adjusts settings for ellipse mode
    ; b = dx
    ld a, (RAM_SpriteTable1.xn + 3*2)
    ld b, a
    ld a, (RAM_SpriteTable1.xn + 0)
    sub b
    jr nc, +
    cpl
+:  ld b, a
    ; h = dy
    ld a, (RAM_SpriteTable1.y + 3)
    ld c, a
    ld a, (RAM_SpriteTable1.y)
    sub c
    jr nc, +
    cpl
+:  ld h, a
    ; Store the smaller one in the radius location
    cp b
    jr nc, +
    ld a, b
+:  ld (RAM_EllipseMinorRadius), a
    ; Calculate the fixed-point ratio of dy/dx
    ld e, b
    ld l, 0
    call DivMod_hl_e_hl_a
    ld (RAM_EllipseRatio), hl
    ret

; 9th entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
Mode8_PaintGraphicBoardHandler:
      call CheckMenuButton
      ; Only when action state = 0
      ld hl, RAM_ActionStateFlags
      ld a, (hl)
      or a
      ret nz
    exx
    ; Update the cursor
    ld a, b ; Pen Y
    ld (RAM_SpriteTable1.y), a
    ld a, (RAM_Pen_Smoothed.x)
    ld (RAM_SpriteTable1.xn + 0), a
    ld a, CursorIndex_ArrowTopLeft
    call SetCursorIndex
    ; Wait for DO
    bit GraphicBoardButtonBit_Do, (hl)
    ret z
    ; Beep
    ld a, 1
    ld (RAM_Beep), a
    ; Save the point
    ld a, (RAM_Pen_Smoothed.x)
    ld h, a
    ld a, (RAM_Pen_Smoothed.y)
    ld l, a
    ld (RAM_PaintStartingPoint), hl
    exx
      ; Set all the action state bits
      ld (hl), %11111111
    ret

; 10th entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
Mode9_CopyGraphicBoardHandler:
      call CheckMenuButton
    exx
    ld a, (RAM_ActionStateFlags)
    ; Only if bit 3 is unset...
    bit 3, a
    ret nz
    ; Bit 2 set = choosing destination
    bit 2, a
    jp nz, Mode9_CopyGraphicBoardHandler_ChooseDestination
    ; Bit 1 set = selecting second point
    bit 1, a
    jp nz, Mode9_CopyGraphicBoardHandler_SecondPoint
    ; No bits set = selecting first point
    ; Clamp Y to 16..152
    ld c, 16
    ld a, b ; Pen Y
    cp c
    jp nc, +
    ld a, c
+:  ld c, 152
    cp c
    jp c, +
    ld a, c
+:  ld (RAM_SpriteTable1.y), a
    ; Clamp X to 33..201
    ld a, (RAM_Pen_Smoothed.x)
    ld c, 33
    cp c
    jp nc, +
    ld a, c
+:  ld c, 201
    cp c
    jp c, +
    ld a, c
+:  ld (RAM_SpriteTable1.xn + 0), a
    ld a, CursorIndex_ArrowBottomRight
    call SetCursorIndex
    ; Wait for the Do button
    bit GraphicBoardButtonBit_Do, (hl)
    ret z
    ; Beep
    ld a, 1
    ld (RAM_Beep), a
    ; Go to the next state
    ld a, (RAM_ActionStateFlags)
    set 1, a
    ld (RAM_ActionStateFlags), a
    ; Copy the sprite to slot 3
    ld a, (RAM_SpriteTable1.y)
    ld (RAM_SpriteTable1.y + 3), a
    ; Save Y+7 = bottom of sprite
    add a, 7
    ld (RAM_Copy_FirstPoint.y), a
    ; Start next cursor 8px away
    add a, 8
    ld (RAM_SpriteTable1.y), a
    ; Same for X
    ld a, (RAM_SpriteTable1.xn + 0)
    ld (RAM_SpriteTable1.xn + 3*2), a
    add a, 7
    ld (RAM_Copy_FirstPoint.x), a
    add a, 8
    ld (RAM_SpriteTable1.xn + 0), a
    ld a, $A9 ; Second cursor tile index
    ld (RAM_SpriteTable1.xn + 3*2+1), a
    xor a
    ld ($C15D), a ; ???
    ld a, SetCursorIndex_Second | CursorIndex_ArrowBottomRight
    jp SetCursorIndex ; and ret

Mode9_CopyGraphicBoardHandler_SecondPoint:
.define COPY_MAXIMUM_SIZE 88
    ; Clamp Y to (point 1 Y)+7..min((point 1 Y)+7+88, 255)
    ; to enforce a "positive" area, in the screen bounds, no more than 88px tall
    ld a, (RAM_Copy_FirstPoint.y)
    add a, 7
    ld c, a
    cp b
    jp c, +
    ld b, c
+:  ld a, c
    add a, COPY_MAXIMUM_SIZE
    jp nc, +
    ld a, 255
+:  ld c, a
    cp b
    jp nc, +
    ld b, c
+:  ld a, b ; Thnen we also limit it to 166? Not sure why
    ld c, 166 ; 166 = screen coordinate of bottom of canvas?
    cp c
    jp c, +
    ld a, c
+:  ld (RAM_SpriteTable1.y), a
    ld (RAM_Copy_SecondPoint.y), a
    ; Similar for X TODO
    ld a, (RAM_Pen_Smoothed.x)
    ld b, a
    ld a, (RAM_Copy_FirstPoint.x)
    add a, 7
    ld c, a
    cp b
    jp c, +
    ld b, c
+:  ld a, c
    add a, COPY_MAXIMUM_SIZE
    jp nc, +
    ld a, 255
+:  ld c, a
    cp b
    jp nc, +
    ld b, c
+:  ld a, b
    ld c, 215
    cp c
    jp c, +
    ld a, c
+:  ld (RAM_SpriteTable1.xn + 0), a
    ld (RAM_Copy_SecondPoint.x), a
    ld a, CursorIndex_ArrowTopLeft
    call SetCursorIndex
    ; Wait for the Do button
    bit GraphicBoardButtonBit_Do, (hl)
    ret z
    ; Beep
    ld a, 1
    ld (RAM_Beep), a
    ; Enter the third phase
    ld a, (RAM_ActionStateFlags)
    set 2, a
    ld (RAM_ActionStateFlags), a
    ; This is unnecessary?
    ld a, (RAM_SpriteTable1.y)
    ld (RAM_Copy_SecondPoint.y), a
    ld a, (RAM_SpriteTable1.xn + 0)
    ld (RAM_Copy_SecondPoint.x), a
    ret

Mode9_CopyGraphicBoardHandler_ChooseDestination:
    ; Clamp y to the range 16..151
    ld c, 16
    ld a, b
    cp c
    jp nc, +
    ld a, c
+:  ld c, 151
    cp c
    jp c, +
    ld a, c
+:  ld (RAM_SpriteTable1.y + 0), a ; Sprite 0 there
    ; Clamp X to the range 33..201
    ld a, (RAM_Pen_Smoothed.x)
    ld c, 33
    cp c
    jp nc, +
    ld a, c
+:  ld c, 201
    cp c
    jp c, +
    ld a, c
+:  ld (RAM_SpriteTable1.xn + 0), a ; Sprite 0 there
    ld a, CursorIndex_ArrowBottomRight
    call SetCursorIndex
    ; Wait for the Do button
    bit GraphicBoardButtonBit_Do, (hl)
    ret z
    ; Beep
    ld a, 1
    ld (RAM_Beep), a
    ; Retrieve the cursor position
    ld a, (RAM_SpriteTable1.y + 0)
    ; Offset to the pointed pixel
    add a, 7
    ; Save it
    ld (RAM_Copy_Destination.y), a
    ; Same for X
    ld a, (RAM_SpriteTable1.xn + 0)
    add a, 7
    ld (RAM_Copy_Destination.x), a
    ld a, (RAM_ActionStateFlags)
    ; Signal for work to be done
    set 3, a
    ld (RAM_ActionStateFlags), a
    ret

; 11th entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
Mode10_MirrorGraphicBoardHandler:
      call CheckMenuButton
    exx
    ld a, (RAM_ActionStateFlags)
    ld d, a
    rrca
    jp nc, _LABEL_3527_
    ld iy, RAM_SubmenuSelectionIndex
    rrca
    jp nc, _LABEL_3469_
    rrca
    ret c
    ld a, (RAM_Copy_FirstPoint.y)
    add a, $07
    ld c, a
    cp b
    jp c, +
    ld b, c
+:  ld a, c
    add a, $58
    jp nc, +
    ld a, $FF
+:  ld c, a
    cp b
    jp nc, +
    ld b, c
+:  ld a, b
    ld c, $A6
    cp c
    jp c, +
    ld a, c
+:  bit 0, (iy+0)
    call nz, _LABEL_34E0_
    ld (RAM_SpriteTable1.y), a
    ld (RAM_Copy_SecondPoint.y), a
    ld a, (RAM_Pen_Smoothed.x)
    ld b, a
    ld a, (RAM_Copy_FirstPoint.x)
    add a, $07
    ld c, a
    cp b
    jp c, +
    ld b, c
+:  ld a, c
    add a, $58
    jp nc, +
    ld a, $FF
+:  ld c, a
    cp b
    jp nc, +
    ld b, c
+:  ld a, b
    ld c, $D7
    cp c
    jp c, +
    ld a, c
+:  bit 0, (iy+0)
    call z, _LABEL_3501_
    ld (RAM_SpriteTable1.xn), a
    ld (RAM_Copy_SecondPoint.x), a
    ld a, CursorIndex_ArrowTopLeft
    call SetCursorIndex
    bit 1, (hl)
    ret z
    ld a, 1
    ld (RAM_Beep), a
    ld a, (RAM_ActionStateFlags)
    set 2, a
    ld (RAM_ActionStateFlags), a
    ld a, (RAM_SpriteTable1.y)
    ld (RAM_Copy_SecondPoint.y), a
    ld a, (RAM_SpriteTable1.xn)
    ld (RAM_Copy_SecondPoint.x), a
    ret

_LABEL_3469_:
    ld c, $10
    ld a, b
    cp c
    jp nc, +
    ld a, c
+:  ld c, $98
    cp c
    jp c, +
    ld a, c
+:  bit 0, (iy+0)
    call nz, _LABEL_34ED_
    ld (RAM_SpriteTable1.y), a
    ld a, (RAM_Pen_Smoothed.x)
    ld c, $21
    cp c
    jp nc, +
    ld a, c
+:  ld c, $C9
    cp c
    jp c, +
    ld a, c
+:  bit 0, (iy+0)
    call z, _LABEL_3513_
    ld (RAM_SpriteTable1.xn), a
    ld a, CursorIndex_ArrowBottomRight
    call SetCursorIndex
    bit 1, (hl)
    ret z
    ld a, 1
    ld (RAM_Beep), a
    ld a, (RAM_ActionStateFlags)
    set 1, a
    ld (RAM_ActionStateFlags), a
    ld a, (RAM_SpriteTable1.y)
    ld (RAM_SpriteTable1.y + 3), a
    add a, $07
    ld (RAM_Copy_FirstPoint.y), a
    add a, $08
    ld (RAM_SpriteTable1.y), a
    ld a, (RAM_SpriteTable1.xn)
    ld (RAM_SpriteTable1.xn + 3*2), a
    add a, $07
    ld (RAM_Copy_FirstPoint.x), a
    add a, $08
    ld (RAM_SpriteTable1.xn), a
    ld a, $A9
    ld (RAM_SpriteTable1.xn + 3*2+1), a
    xor a
    ld ($C15D), a
    ld a, SetCursorIndex_Second | CursorIndex_ArrowBottomRight
    jp SetCursorIndex ; and ret

_LABEL_34E0_:
    ld c, a
    ld a, ($C16F)
    add a, $60
    cp c
    jp nc, +
    ld c, a
+:  ld a, c
    ret

_LABEL_34ED_:
    ld c, a
    ld a, ($C16F)
    sub $07
    cp c
    jp c, +
    ld c, a
+:  add a, $58
    cp c
    jp nc, _LABEL_34FF_
    ld c, a
_LABEL_34FF_:
    ld a, c
    ret

_LABEL_3501_:
    ld c, a
    ld a, ($C170)
    cp c
    jp c, _LABEL_350A_
    ld c, a
_LABEL_350A_:
    add a, $60
    cp c
    jp nc, _LABEL_3511_
    ld c, a
_LABEL_3511_:
    ld a, c
    ret

_LABEL_3513_:
    ld c, a
    ld a, ($C170)
    sub $07
    cp c
    jp c, _LABEL_351E_
    ld c, a
_LABEL_351E_:
    add a, $58
    cp c
    jp nc, _LABEL_3525_
    ld c, a
_LABEL_3525_:
    ld a, c
    ret

_LABEL_3527_:
    push hl
      ld hl, $3586
      ld a, (RAM_SubmenuSelectionIndex)
      dec a
      jp nz, +
      ld hl, Data_357F
+:    ld a, b
      cp (hl)
      jp nc, +
      ld a, (hl)
+:    inc hl
      cp (hl)
      jp c, +
      ld a, (hl)
+:    ld (RAM_SpriteTable1.y), a
      inc hl
      ld a, (RAM_Pen_Smoothed.x)
      cp (hl)
      jp nc, +
      ld a, (hl)
+:    inc hl
      cp (hl)
      jp c, +
      ld a, (hl)
+:    ld (RAM_SpriteTable1.xn), a
      inc hl
      ld a, (hl)
      inc hl
      call SetCursorIndex
      ex de, hl
    pop hl
    bit 1, (hl)
    ret z
    ex de, hl
    ld a, 1
    ld (RAM_Beep), a
    ld a, (RAM_SpriteTable1.y)
    add a, (hl)
    ld ($C16F), a
    inc hl
    ld a, (RAM_SpriteTable1.xn)
    add a, (hl)
    ld ($C170), a
    ld a, (RAM_ActionStateFlags)
    or $01
    ld (RAM_ActionStateFlags), a
    ret

Data_357F: ; $357F
.db $10 $40 $2B $CC $06 $07 $04 $1B $9C $20 $70 $07 $03 $08

; 12th entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
Mode11_MagnifyGraphicBoardHandler:
      call CheckMenuButton
    exx
    ld a, (RAM_ActionStateFlags)
    bit 2, a
    jp nz, _LABEL_363C_
    ld c, $0F
    ld a, b
    cp c
    jp nc, +
    ld a, c
+:  ld c, $7F
    cp c
    jp c, +
    ld a, c
+:  ld (RAM_SpriteTable1.y), a
    add a, $07
    ld (RAM_Copy_FirstPoint.y), a
    add a, $21
    ld (RAM_Copy_SecondPoint.y), a
    ld a, (RAM_Pen_Smoothed.x)
    ld c, $20
    cp c
    jp nc, +
    ld a, c
+:  ld c, $B0
    cp c
    jp c, +
    ld a, c
+:  ld (RAM_SpriteTable1.xn), a
    add a, $07
    ld (RAM_Copy_FirstPoint.x), a
    add a, $21
    ld (RAM_Copy_SecondPoint.x), a
    ld a, CursorIndex_ArrowBottomRight
    call SetCursorIndex
    bit 1, (hl)
    ret z
    ld a, 1
    ld (RAM_Beep), a
    ld a, (RAM_ActionStateFlags)
    or $04
    ld (RAM_ActionStateFlags), a
    ld a, (RAM_SpriteTable1.y)
    ld (RAM_SpriteTable1.y + 3), a
    add a, $08
    ld (RAM_SpriteTable1.y), a
    ld a, (RAM_SpriteTable1.xn)
    ld (RAM_SpriteTable1.xn + 3*2), a
    add a, $08
    ld (RAM_SpriteTable1.xn), a
    ld a, $A9
    ld (RAM_SpriteTable1.xn + 3*2+1), a
    xor a
    ld ($C15D), a
    ld a, SetCursorIndex_Second | CursorIndex_ArrowBottomRight
    call SetCursorIndex
    ld c, $00
    ld a, (RAM_SpriteTable1.y)
    sub $17
    cp $40
    jp nc, +
    set 0, c
+:  ld a, (RAM_SpriteTable1.xn)
    sub $20
    cp $50
    jp nc, +
    set 1, c
+:  ld a, c
    ld ($C172), a
    rlc c
    ld b, 0
    ld hl, Magnify_WindowLocations
    add hl, bc
    ld a, (hl)
    ld ($C162), a
    inc hl
    ld a, (hl)
    ld (RAM_TitleScreenAndEndTimeout), a
    ret

_LABEL_363C_:
    bit 1, (hl)
    jp nz, +
    ld a, (RAM_Pen_Smoothed.x)
    and $FE
    dec a
    ld (RAM_SpriteTable1.xn), a
    ld a, b
    and $FE
    ld (RAM_SpriteTable1.y), a
    ld a, CursorIndex_ZoomedPixel
    jp SetCursorIndex ; and ret

+:  ld a, (RAM_ActionStateFlags)
    or $80 ; Set high bit
    ld (RAM_ActionStateFlags), a
    ret

Magnify_WindowLocations:
.db  0,   0
.db 80,   0
.db  0, 112
.db 80, 112

; 15th entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
SubmenuGraphicBoardHandler:
      call CheckMenuButton
    exx
    ; Sprite to 88, (y % 8) (in the range 64..72), menu arrow cursor
    ld a, 88
    ld (RAM_SpriteTable1.xn + 0), a
    ld a, b ; Pen Y
    and $F8 ; Modulo 8
    cp 64 ; Minimum 64
    jp nc, +
    ld a, 64
+:  cp 72
    jp c, +
    ld a, 72
+:  ld (RAM_SpriteTable1.y), a
    ld b, a
    ld a, CursorIndex_MenuArrowRight
    call SetCursorIndex

    ; Wait for button
    bit GraphicBoardButtonBit_Do, (hl)
    ret z
    
    ; Beep
    ld a, 1
    ld (RAM_Beep), a
    
    ; Figure out our index
    ld a, b
    sub 64
    jp z, +
    ld a, 1
+:  ld (RAM_SubmenuSelectionIndex), a ; 0 for top item, 1 for bottom one

    ld a, (RAM_CurrentMode)
    set 6, a ; Set bit 6 (?)
    ld (RAM_CurrentMode), a
    ret

NotLocal_LABEL_36A5_:
    ld a, $0F
    ld (RAM_SpriteTable1.y), a
    ld a, (RAM_Pen_Smoothed.x)
    and $F8
    ld b, $28
    cp b
    jp c, +
    ld b, $D0
    cp b
    jp nc, +
    ld b, a
+:  ld a, b
    ld (RAM_SpriteTable1.xn), a
    sub $28
    rrca
    rrca
    rrca
    cp $10
    jp c, +
    sub $11
    jp m, ++
    bit 2, (hl)
    jp z, ++
    cp $04
    jp z, +++
    ld (RAM_PenStyle), a
    ld a, 1
    ld (RAM_Beep), a
    jp ++

+++:ld a, 1
    ld (RAM_Beep), a
    ld a, ($C062)
    xor $01
    ld ($C062), a
    jp ++

+:  bit 2, (hl)
    jp z, ++
    ld (RAM_DrawingData.CurrentlySelectedPaletteIndex), a
    ld a, b
    ld ($C242), a
    ld a, 1
    ld (RAM_Beep), a
    ld a, (RAM_PenStyle)
    cp PenStyle_Erase
    jp nz, ++
    xor a ; PenStyle_Thin
    ld (RAM_PenStyle), a
++: ld a, $A8
    ld ($C241), a
    ld a, CursorIndex_PaletteSelect
    call SetCursorIndex
    jp CheckMenuButton ; and ret

UpdateCursorColourCycling:
    ld hl, RAM_CursorColourCycle_Delay
    dec (hl)
    ret p
    ld (hl), 4 ; Frame count between changes
    inc hl ; RAM_CursorColourCycle_Index
    ld a, (hl)
    inc a
    cp CursorColourCycleEnd-CursorColourCycle
    jp c, +
    xor a
+:  ld (hl), a
    ld hl, CursorColourCycle
    ld d, 0
    ld e, a
    add hl, de
    LD_DE_PALETTE 31
    VDP_ADDRESS_TO_DE

    ; Delay: 4350 cycles - seems unnecessary?
    ld b, 0
    djnz -3 ; This actually jumps to the 0 parameter of the preceding opcode, which decodes as "nop".

    ld a, (hl)
    out (Port_VDPData), a
    ret

CursorColourCycle:
    COLOUR 0, 0, 0 ; Black
    COLOUR 3, 0, 0 ; Red
    COLOUR 0, 3, 0 ; Green
    COLOUR 3, 3, 0 ; Yellow
    COLOUR 0, 0, 3 ; Blue
    COLOUR 3, 3, 3 ; White
CursorColourCycleEnd:

InitialiseCursorSprites:
    ld hl, InitialiseCursorSprites_Y
    ld de, RAM_SpriteTable1.y
    ld bc, 4
    ldir
    ld hl, InitialiseCursorSprites_XN
    ld de, RAM_SpriteTable1.xn
    ld bc, 8
    ldir
    ret

InitialiseCursorSprites_Y:
.db 64, 0, SpriteTableYTerminator, SpriteTableYTerminator
InitialiseCursorSprites_XN:
.db 64 $A8, 40 $A7, 0 $A7, 0 $A9 ; TODO: enum or something for tile indices

UpdateCursorGraphics:
    ld hl, RAM_CurrentCursorIndex
    bit 7, (hl) ; Bit 7 unset -> nothing to do
    ret z
    res 7, (hl) ; Clear it
    set 6, (hl) ; Set bit 6 (?)
    LD_DE_TILE $1a8 ; Bit 5 determines which sprite to write
    bit 5, (hl)
    jp z, +
    LD_DE_TILE $1a9
+:  ld hl, (RAM_CurrentCursorDataAddress)
    VDP_ADDRESS_TO_DE
    ld c, Port_VDPData
.rept SizeOfTile
    outi
.endr
    ret

SetCursorIndex:
    ; Arguments:
    ; a = cursor index, with bit 5 set if the second cursor is to be changed
    push bc
    push hl
      ld b, a ; Check if the argument matches the current value
      ld hl, RAM_CurrentCursorIndex
      ld a, (hl)
      and %00111111 ; Mask off unused high bits
      cp b
      jp z, + ; If so, there's nothing to do

      ld a, b
      ld (hl), a ; Store the new value

      res 5, a ; Might be set, determines which sprite to set
      push hl
        ; Calculate de + a * 32
        LD_HL_A
        add hl, hl
        add hl, hl
        add hl, hl
        add hl, hl
        add hl, hl
        ld de, CursorTiles
        add hl, de
        ; Store it
        ld (RAM_CurrentCursorDataAddress), hl
      pop hl
      set 7, (hl) ; Mark it as needing to be written to VRAM
+:  pop hl
    pop bc
    ret

UpdateButtonGraphics:
    ld hl, RAM_ButtonStateShownOnScreen
    ld a, (RAM_ButtonsPressed)
    ld c, a

    ld b, 3 ; Tile count
    LD_DE_TILE $1aa
    bit GraphicBoardButtonBit_Menu, c ; Menu button
    jp nz, MenuPressed
    bit GraphicBoardButtonBit_Menu, (hl)
    jp z, MenuNothingToUpdate
    res GraphicBoardButtonBit_Menu, (hl)
    push hl
      ld hl, ButtonTiles + SizeOfTile/2 * 3 * 0 ; Menu not pressed
      call Write2bppToVRAMSlowly
    pop hl
MenuNothingToUpdate:

    ld b, 3
    LD_DE_TILE $1ad
    bit GraphicBoardButtonBit_Do, c ; Do button
    jp nz, DoPressed
    bit GraphicBoardButtonBit_Do, (hl)
    jp z, DoNothingToUpdate
    res GraphicBoardButtonBit_Do, (hl)
    push hl
      ld hl, ButtonTiles + SizeOfTile/2 * 3 * 2 ; Do not pressed
      call Write2bppToVRAMSlowly
    pop hl
DoNothingToUpdate:

    ld b, 3
    LD_DE_TILE $1b0
    bit GraphicBoardButtonBit_Pen, c ; Pen button
    jp nz, PenPressed
    bit GraphicBoardButtonBit_Pen, (hl)
    ret z ; Nothing to update
    res GraphicBoardButtonBit_Pen, (hl)
    ld hl, ButtonTiles + SizeOfTile/2 * 3 * 4 ; Pen not pressed
    jp Write2bppToVRAMSlowly ; and ret


MenuPressed:
    bit GraphicBoardButtonBit_Menu, (hl)
    jp nz, MenuNothingToUpdate
    set GraphicBoardButtonBit_Menu, (hl)
    push hl
      ld hl, ButtonTiles + SizeOfTile/2 * 3 * 1 ; Menu pressed
      call Write2bppToVRAMSlowly
    pop hl
    jp MenuNothingToUpdate

DoPressed:
    bit GraphicBoardButtonBit_Do, (hl)
    jp nz, DoNothingToUpdate
    set GraphicBoardButtonBit_Do, (hl)
    push hl
      ld hl, ButtonTiles + SizeOfTile/2 * 3 * 3 ; Do pressed
      call Write2bppToVRAMSlowly
    pop hl
    jp DoNothingToUpdate

PenPressed:
    bit GraphicBoardButtonBit_Pen, (hl)
    ret nz ; Nothing to update
    set GraphicBoardButtonBit_Pen, (hl)
    ld hl, ButtonTiles + SizeOfTile/2 * 3 * 5 ; Pen pressed
    jp Write2bppToVRAMSlowly

UpdatePenGraphics:
    ld ix, RAM_PenMode
    ld a, (RAM_PenStyle) ; Desired new pen mode
    cp (ix+PenMode.Current)
    jp z, PenModeNotChanged

    ; Check if we need to unset the old mode
    bit 0, (ix+PenMode.IsSet)
    push af
      call nz, TurnOffCurrentPenIcon
    pop af

    ; Draw current pen mode icon in red
    ld (ix+PenMode.Current), a ; Set pen mode
    ld b, 1 ; Tile count
    ld hl, +
    jp JumpToFunction

+:
.dw DrawThinPenOn
.dw DrawMediumPenOn
.dw DrawThickPenOn
.dw DrawEraserOn

; 1st entry of Jump Table from 388C (indexed by RAM_PenStyle)
DrawThinPenOn:
    set 0, (ix+PenMode.IsSet)
    LD_DE_TILE $19d
    LD_HL_PEN_TILE_GRAPHICS PenTile_Thin_On
    jp FillTiles2bpp

; 2nd entry of Jump Table from 388C (indexed by RAM_PenStyle)
DrawMediumPenOn:
    set 0, (ix+PenMode.IsSet)
    LD_DE_TILE $19e
    LD_HL_PEN_TILE_GRAPHICS PenTile_Medium_On
    jp FillTiles2bpp

; 3rd entry of Jump Table from 388C (indexed by RAM_PenStyle)
DrawThickPenOn:
    set 0, (ix+PenMode.IsSet)
    LD_DE_TILE $19f
    LD_HL_PEN_TILE_GRAPHICS PenTile_Thick_On
    jp FillTiles2bpp

; 4th entry of Jump Table from 388C (indexed by RAM_PenStyle)
DrawEraserOn:
    set 0, (ix+PenMode.IsSet)
    LD_DE_TILE $1a0
    LD_HL_PEN_TILE_GRAPHICS PenTile_Erase_On
    jp FillTiles2bpp

TurnOffCurrentPenIcon:
    ld b, 1 ; Tile count
    ld a, (ix+PenMode.Current)
    ld hl, +
    jp JumpToFunction

+:
.dw DrawThinPenOff
.dw DrawMediumPenOff
.dw DrawThickPenOff
.dw DrawEraserOff

DrawThinPenOff:
    res 0, (ix+PenMode.IsSet)
    LD_DE_TILE $19d
    LD_HL_PEN_TILE_GRAPHICS PenTile_Thin_Off
    jp FillTiles2bpp ; and ret

DrawMediumPenOff:
    res 0, (ix+PenMode.IsSet)
    LD_DE_TILE $19e
    LD_HL_PEN_TILE_GRAPHICS PenTile_Medium_Off
    jp FillTiles2bpp ; and ret

DrawThickPenOff:
    res 0, (ix+PenMode.IsSet)
    LD_DE_TILE $19f
    LD_HL_PEN_TILE_GRAPHICS PenTile_Thick_Off
    jp FillTiles2bpp ; and ret

DrawEraserOff:
    res 0, (ix+PenMode.IsSet)
    LD_DE_TILE $1a0
    LD_HL_PEN_TILE_GRAPHICS PenTile_Erase_Off
    jp FillTiles2bpp ; and ret

PenModeNotChanged:
    ; Check something else...
    ld a, ($C062) ; desired new dot mode?
    cp (ix+PenMode.Dots)
    ret z

    ld (ix+PenMode.Dots), a
    ld b, 1 ; Tile count

    bit 0, a
    jp z, +

    ; Dots mode
    LD_DE_TILE $1a1
    LD_HL_PEN_TILE_GRAPHICS PenTile_DotMode_On
    jp FillTiles2bpp ; and ret

+:  ; Line mode
    LD_DE_TILE $1a1
    LD_HL_PEN_TILE_GRAPHICS PenTile_DotMode_Off
    jp FillTiles2bpp ; and ret

NotLocal_LABEL_3932_:
    ld a, (RAM_ActionStateFlags)
    and $07
    ret z
    cp $07
    jp nc, _LABEL_39C0_
    ld a, (RAM_Copy_FirstPoint.y)
    ld b, a
    ld a, (RAM_Copy_SecondPoint.y)
    sub b
    ret c
    ld ($C0BE), a
    ld b, a
    and $F8
    rrca
    rrca
    rrca
    inc a
    cp $0D
    jp c, +
    ld a, $0C
+:  ld ($C0BF), a
    ld a, b
    and $07
    ld ($C0C0), a
    ld a, (RAM_Copy_FirstPoint.x)
    ld b, a
    ld a, (RAM_Copy_SecondPoint.x)
    sub b
    ret c
    ld ($C0C1), a
    ld c, a
    and $F8
    rrca
    rrca
    rrca
    inc a
    cp $0D
    jp c, +
    ld a, $0C
+:  ld ($C0C2), a
    ld a, c
    and $07
    ld ($C0C3), a
    ld ix, $C0FA
    ld de, $C0CA
    ld a, (RAM_Copy_FirstPoint.x)
    ld c, a
    ld a, ($C0C2)
    ld b, a
    ld a, (RAM_Copy_FirstPoint.y)
    call _LABEL_3A83_
    exx
      ld a, (RAM_Copy_FirstPoint.y)
      ld b, a
      ld a, ($C0BE)
      add a, b
    exx
    call _LABEL_3A83_
    ld a, (RAM_Copy_FirstPoint.y)
    ld c, a
    ld a, ($C0BF)
    ld b, a
    ld a, (RAM_Copy_FirstPoint.x)
    call _LABEL_3A35_
    exx
      ld a, (RAM_Copy_FirstPoint.x)
      ld b, a
      ld a, ($C0C1)
      add a, b
    exx
    call _LABEL_3A35_
_LABEL_39C0_:
    di
      ld ix, $C210
      ld iy, $C260
      ld a, ($C0BF)
      add a, a
      ld b, a
      ld a, ($C0C2)
      add a, a
      add a, b
      ld b, a
      ld a, $30
      sub b
      ld c, a
      ld de, $C0FA
      ld hl, $C0CA
-:    ld a, (hl)
      ld (ix+0), a
      inc ix
      inc hl
      ld a, (de)
      ld (iy+0), a
      inc iy
      inc de
      ld a, (de)
      ld (iy+0), a
      inc iy
      inc de
      djnz -
      jp +

; Skipped-over code?
      push bc
        ld a, b
        dec a
        ld c, a
        ld b, 0
        ld hl, $c0ca
        add hl, bc
        ex de, hl
        push bc
        pop hl
        add hl, hl
        ld bc, $c0fa
        add hl, bc
        inc hl
        ex de, hl
      pop bc
  -:  ld a, (hl)
      ld (ix+0), a
      inc ix
      dec hl
      ld a, (de)
      ld (iy+1), a
      dec de
      ld a, (de)
      ld (iy+0), a
      inc iy
      inc iy
      dec de
      djnz -

+:    ld a, c
      or a
      jp z, +
-:    ld (ix+0), $E0
      inc ix
      dec a
      jp nz, -
+:  ei
    ret

_LABEL_3A35_:
    ld hl, $3B21
    push bc
      dec b
      jp z, _LABEL_3A6F_
      call _LABEL_3A5A_
      ex af, af'
      dec de
      ld a, (de)
      inc de
      ld b, a
      ld a, ($C0C0)
      add a, b
      inc a
      ld (de), a
      inc de
      ex af, af'
      ld (ix+0), a
      inc ix
      ld (ix+0), $A5
      inc ix
    pop bc
    ret

_LABEL_3A5A_:
-:  ex af, af'
    ld a, (hl)
    add a, c
    ld (de), a
    inc de
    inc hl
    ex af, af'
    ld (ix+0), a
    inc ix
    ld (ix+0), $A5
    inc ix
    djnz -
    ret

.endasm ; Unmatched push matching
push bc
.asm

_LABEL_3A6F_:
      ex af, af'
      ld a, (hl)
      add a, c
      ld (de), a
      inc de
      inc hl
      ex af, af'
      ld (ix+0), a
      inc ix
      ld (ix+0), $A5
      inc ix
    pop bc
    ret

_LABEL_3A83_:
    push bc
      ld hl, $3B21
      dec b
      jp z, _LABEL_3ABA_
      call _LABEL_3AA5_
      ld (de), a
      inc de
      ld a, ($C0C3)
      add a, (ix+-2)
      add a, b
      inc a
      ld (ix+0), a
      inc ix
      ld (ix+0), $A6
      inc ix
    pop bc
    ret

_LABEL_3AA5_:
-:  ld (de), a
    ex af, af'
    inc de
    ld a, (hl)
    add a, c
    ld (ix+0), a
    inc hl
    inc ix
    ld (ix+0), $A6
    inc ix
    ex af, af'
    djnz -
    ret

.endasm ; Unmatched push matching
push bc
.asm
_LABEL_3ABA_:
      ld (de), a
      ex af, af'
      inc de
      ld a, (hl)
      add a, c
      ld (ix+0), a
      inc hl
      inc ix
      ld (ix+0), $A6
      inc ix
      ex af, af'
    pop bc
    ret

NotLocal_LABEL_3ACE_:
    ld b, $0C
    ld a, (RAM_SubmenuSelectionIndex)
    rrca
    jp nc, +
    ld a, (RAM_SpriteTable1.y)
    add a, $07
    ld ($C16F), a
    ld c, a
    ld a, (RAM_SpriteTable1.xn)
    add a, $04
    ld ($C170), a
    ld ix, $C248
    ld de, $C204
    ld hl, $3B21
    jp _LABEL_3A5A_

+:  ld a, (RAM_SpriteTable1.xn)
    add a, $08
    ld ($C170), a
    ld c, a
    ld a, (RAM_SpriteTable1.y)
    add a, $03
    ld ($C16F), a
    ld ix, $C248
    ld de, $C204
    ld hl, $3B21
    jp _LABEL_3AA5_

EnableOnlyThreeSprites:
    ; Could only set the terminator once?
    ld hl, RAM_SpriteTable1.y + 3
    ld de, RAM_SpriteTable1.y + 4
    ld bc, 64 - 3 - 1
    ld (hl), SpriteTableYTerminator
    ldir
    ret

; Data from 3B21 to 3B2D (13 bytes)
.db $00 $08 $10 $18 $20 $28 $30 $38 $40 $48 $50 $58 $60

UpdateStatusBarText:
    ; Called in VBlank
    ld a, (RAM_StatusBarTextIndex)
    or a
    ret z
    dec a
    ld l, a ; hl = a
    xor a
    ld h, a
    ld (RAM_StatusBarTextIndex), a ; Unset it
    ; Look up text from index
    push hl ; hl *= 13
      add hl, hl
      add hl, hl
      push hl
        add hl, hl
      pop bc
      add hl, bc
    pop bc
    add hl, bc
    ld bc, StatusBarText ;$3B66
    add hl, bc
    LD_DE_TILE $1B3
    VDP_ADDRESS_TO_DE
    ld b, STATUS_BAR_TEXT_LENGTH ; String lengths
-:  push bc
      ld a, (hl) ; Get character
      push hl
        ld h, 0
        ld l, a ; Convert to address of letter in font
        add hl, hl ; x16
        add hl, hl
        add hl, hl
        add hl, hl
        ld bc, Font2bpp
        add hl, bc
        ld b, 1 ; Tile count
        call FillTiles2bppCurrentAddress
      pop hl
      inc hl
    pop bc
    djnz -
    ret

StatusBarText:
; All must be 13 characters long
.define STATUS_BAR_TEXT_LENGTH 13
;     1234567890123
.asc "             "
.asc " COLOR MODE  "
.asc " ERASE MODE  "
.asc " SQUARE MODE "
.asc " CIRCLE MODE "
.asc " ELLIPSE MODE"
.asc " PAINT MODE  "
.asc " COPY MODE   "
.asc " MIRROR MODE "
.asc " MAGNIFY MODE"
.asc " DISPLAY MODE"
.asc "   THE END   "

TitleScreenFont:
.incbin "Graphics/Font tiles.pscompr"

CursorTiles: ; $3eb2
.incbin "Graphics/Cursor tiles.4bpp"

PenTiles: ; $3ff2
.incbin "Graphics/Pen tiles.2bpp"    ; Pen widths, E and D, selected or not

ButtonTiles: ; $4092
.incbin "Graphics/Button tiles.2bpp" ; MENU, DO, PEN
Font2bpp:     ; $41b2
.incbin "Graphics/Font.2bpp"         ; Includes frames
ControlTiles: ; $4552
.incbin "Graphics/Control tiles.pscompr"

; Lots of unused space (14615 bytes)

; Header
.smstag ; TMR SEGA, checksum, region, size
.orga $7ff8
.db "WK"
.orga $7ffc
.dw $4009 ; product code - not valid?
.db $02 ; version 2

