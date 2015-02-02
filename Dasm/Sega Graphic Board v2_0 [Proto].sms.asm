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
CursorTile_Crosshair:         db ; Drawing cursor
CursorTile_PaletteSelect:     db ; Used when selecting stuff up top
CursorTile_Square:            db ; Unused?
CursorTile_MenuArrowRight:    db ; Used in menu
CursorTile_ArrowTopLeft:      db ; Used for defining one corner of something rectangular
CursorTile_ArrowBottomRight:  db ; Used for defining opposite corner of something rectangular
CursorTile_ArrowDown:         db ; Used for defining H-flip axis
CursorTile_ArrowRight:        db ; Used for defining V-flip axis
CursorTile_ZoomedPixel:       db ; Used to show snapped pixel when in Zoom mode
CursorTile_X:                 db ; Used for defining points for circles/ellipses
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

.bank 0 slot 0
.org $0000
FullReset:
    jp Start

.dsb 5, 0 ; 5 bytes blank

.org $0008
VDPAddressToDE:
    ld a, e
    out (Port_VDPAddress), a
    ld a, d
    out (Port_VDPAddress), a
    ret

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

; Data from 19 to 37 (31 bytes)
.db "PROGRAM By K.WAKIHARA"

.org $0038
InterruptHandler:
    jp InterruptHandlerImpl

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

.org $0066
NMIHandler:
    retn

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
    ld a, $01
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
    ld hl, $4858
    ld ($C03E), hl

    ld a, 1
    ld (RAM_PenMode.IsSet), a

    ; Main loop
-:  ei
    ld a, $03
    call SetVBlankFunctionAndWait
    call CallNonVBlankModeSpritesHandler
    call CallNonVBlankModeFunction
    call SpriteTable1to2
    jp -
    
; Various functions for manipulating graphics

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
    ld (RAM_Write1bppToVRAMWithExtensionMask_Mask),a ; save the bitmask
    VDP_ADDRESS_TO_DE
--: ld a,(hl) ; Read a byte
    exx
      ld c, Port_VDPData    
      ld b, 4 ; Counter
      ld h, a ; Hold read byte
      ld a, (RAM_Write1bppToVRAMWithExtensionMask_Mask)
-:    rra
      ld d,h
      jr c,+
      ld d,0
+:    out (c),d
      djnz -
    exx
    inc hl
    dec bc
    ld a,b
    or c
    jp nz,--
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
      add hl,hl
      add hl,hl
      add hl,hl
      ld b,h
      ld c,l
    pop hl
    call +
    push hl
      ld hl,22 * SizeOfTile ; Presumably 22 is the "stride"? TODO
      add hl,de
      ex de,hl
    pop hl
    pop bc
    djnz -
    ret

+:  VDP_ADDRESS_TO_DE
--: push hl
    push bc
      ld b,4      ; Counter
-:    rrc h       ; Rotate a bit into carry
      ld a,l      ; 1 = use l, 0 = use 0
      jp c,+
      xor a
+:    out (Port_VDPData),a
      nop         ; delay
      djnz -
    pop bc
    pop hl
    dec bc
    ld a,b
    or c
    jp nz,--
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
    and $7F
    ld b, a
    ld a, c
    and $80
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

Beep:
    ld a, (RAM_Beep)
    or a
    ret z ; Do nothing while zero
    
    inc a ; Else make sound for four more frames
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

; Data from 4FD to 602 (262 bytes)
; Unused palette? Black with blue and white
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
  COLOUR 0,0,3
  COLOUR 0,0,0
  COLOUR 3,3,3
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00

;.org $051d
DrawingPalette:
  COLOUR 3,3,3 ; White
  COLOUR 0,0,0 ; Black
  COLOUR 1,0,0 ; Reds
  COLOUR 2,0,0
  COLOUR 3,0,0
  COLOUR 0,1,0 ; Greens
  COLOUR 0,2,0
  COLOUR 0,3,0
  COLOUR 0,0,1 ; Blues
  COLOUR 0,0,2
  COLOUR 0,0,3
  COLOUR 0,2,3 ; Sky blue
  COLOUR 3,1,0 ; Orange
  COLOUR 3,3,0 ; Yellows
  COLOUR 3,3,1
  COLOUR 3,3,2
  ; Sprites
  COLOUR 0,0,0 ; Tranparent/black
  COLOUR 0,0,0 ; Black for sprites
  COLOUR 3,3,3 ; White
  COLOUR 3,0,0 ; Red
  COLOUR 0,0,0 ; Unused blacks
  COLOUR 0,0,0
  COLOUR 0,0,0
  COLOUR 0,0,0
  COLOUR 0,0,0
  COLOUR 0,0,0
  COLOUR 0,0,0
  COLOUR 0,0,0
  COLOUR 0,0,0
  COLOUR 0,0,0
  COLOUR 0,0,0
  COLOUR 3,0,0 ; Cycling colour

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
      bit 7, a
      jp nz, VBlank_TitleScreen ; Bit 7 set -> we are in the title screen, jump to a specialised handler
      rrca
      jp nc, + ; Bit 0 set -> ???
      
      ; Regular VBlank
      call CopySpriteTable2ToVRAM
      call UpdateCursorGraphics
      call UpdateCursorColourCycling

      ld a, (RAM_CurrentMode)
      cp ModeHighBit | Mode3_Colour
      jp nz, +
      ld a, ($C054)
      or a
      jp z, +

      ; When $c054 is non-zero...
      ; ...for one frame only...
      xor a
      ld ($C054), a
      
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
      bit 1, a
      jp z, +
      push af
        call ReadGraphicBoard
      pop af
+:    bit 0, a
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
      and $10 ; check for reset button
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

.endasm
; Some pushes to match the pops below - ignore!
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

VBlank_TitleScreen:
    ld a, (RAM_VBlankFunctionControl)
    or a
    jp z, VBlank_CheckResetAndExit
    push af
      call TitleScreenAnimationVBlankEntry ; Always update title screen animation
    pop af
    bit 1, a ; Bit 1 set -> read graphic board
    push af
      call nz, ReadGraphicBoard
    pop af
    bit 2, a ; Bit 2 set -> update title screen text
    call nz, TitleScreenTextUpdate
    jp VBlank_CheckResetAndExit

SetVBlankFunctionAndWait:
    ld (RAM_VBlankFunctionControl), a
-:  ld a, (RAM_VBlankFunctionControl)
    or a
    jp nz, -
    ret

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

.include "graphicboard.asm"

.include "maths.asm"

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
    ld a, $8A
    jp z, +
    ld a, $88
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
      jp z, TitleScreenAnimate_Bit1Zero ; Piel slide/flip animation
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
    ld a, $84
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
-:  ld a, $86
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

;.orga $b8a
Tilemap_Logo:
.incbin "Graphics/Logo tilemap.bin"
;.orga $c2a
Palette_Logo:
.db $10 $3f $15 $2a $00 $00 $00 $00
;.orga $c32
Tiles_Logo:
.incbin "Graphics/Logo tiles.2bpp"
;.orga $14a2
Tilemap_SegaLogo:
.incbin "Graphics/Sega logo.lsbtilemap"
;.orga $14ca
Tiles_SegaLogo:
.incbin "Graphics/Sega logo.pscompr"

CallNonVBlankModeFunction:
    ld hl, RAM_CurrentMode
    ld a, (hl)
    and %00111111
    exx
      ld hl, CallNonVBlankModeFunction_JumpTable
      jp JumpToFunction

; Jump Table from 165C to 167F (18 entries, indexed by RAM_CurrentMode)
CallNonVBlankModeFunction_JumpTable:
.dw NonVBlankMode0_DrawingFunction 
.dw NonVBlankMode1_MenuFunction 
.dw NonVBlankMode2_MenuItemSelectedFunction
.dw NonVBlankMode3_ColourFunction
.dw NonVBlankMode4_EraseFunction
.dw NonVBlankMode5_SquareFunction
.dw NonVBlankMode6_CircleAnd7Function
.dw NonVBlankMode6_CircleAnd7Function
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

; Note: menu-showing handlers could be refactored as they are all the same except for the parameters...

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
      ld ($C089), a
      inc a
      ld (RAM_StatusBarTextIndex), a ; Set it to blank
      call EnableOnlyThreeSprites
      call SetDrawingAreaTilemap

      LD_BC_AREA 12, 14
      ld de, MenuText
      LD_HL_LOCATION 5, 4
      call DrawTextToTilesWithBackup

      ld hl, (RAM_Pen_Smoothed)
      ld ($C08D), hl
      ld hl, (RAM_Pen_Backup)
      ld ($C08F), hl
      ld hl, ($C03E)
      ld (RAM_Pen_Smoothed), hl
      ld (RAM_Pen_Backup), hl
      ld a, $01
      ld (RAM_Beep), a
    ei
    ret

; 3rd entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode2_MenuItemSelectedFunction:
      di
        ; Restore the tiles under the menu
        call SetDrawingAreaTilemap
        ld hl, (RAM_Pen_Smoothed)
        ld a, $48
        cp l
        jp c, +
        ld hl, $4858
+:      ld ($C03E), hl
        call RestoreTileData
        ld hl, ($C08D)
        ld (RAM_Pen_Smoothed), hl
        ld hl, ($C08F)
        ld (RAM_Pen_Backup), hl
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
      ld a, $01
      ld (RAM_Beep), a
    ei
    ret

; 5th entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode4_EraseFunction:
      ld a, ($C0BA)
      or a
      jp z, +

      ld a, (RAM_Beep)
      or a
      ret nz

      ; Zero all tile data
      di
        LD_DE_TILE 0
        ld h, 0
        ld bc, 18 * 22 * SizeOfTile ; All tiles
        call FillVRAMWithH
      ei
    ; fall through
+:  exx
    ld (hl), Mode0_Drawing ; RAM_CurrentMode
    ld a, 1 ; Blank text
    ld (RAM_StatusBarTextIndex), a
    ret

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
      ld hl, ($C08D)
      ld (RAM_Pen_Smoothed), hl
      ld hl, ($C08F)
      ld (RAM_Pen_Backup), hl
      ld a, Mode1_Menu
      ld (RAM_CurrentMode), a
    ei
    ret

+:  ; Enter "display mode"
    set 7, (hl) ; RAM_CurrentMode
    ld hl, (RAM_Pen_Smoothed)
    ld ($C08D), hl
    ld hl, (RAM_Pen_Backup)
    ld ($C08F), hl
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
        ld hl, ($C08D)
        ld (RAM_Pen_Smoothed), hl
        ld hl, ($C08F)
        ld (RAM_Pen_Backup), hl
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
      ld ($C08D), hl
      ld hl, (RAM_Pen_Backup)
      ld ($C08F), hl
      LD_HL_LOCATION 88,72
      ld (RAM_Pen_Smoothed), hl
      ld (RAM_Pen_Backup), hl
    ei
    ret

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
        ld hl, ($C08D)
        ld (RAM_Pen_Smoothed), hl
        ld hl, ($C08F)
        ld (RAM_Pen_Backup), hl
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
      LD_BC_AREA 16, 4
      ld de, ColorMenuText
      LD_HL_LOCATION 5, 4
      call DrawTextToTilesWithBackup
      ld hl, (RAM_Pen_Smoothed)
      ld ($C08D), hl
      ld hl, (RAM_Pen_Backup)
      ld ($C08F), hl
      LD_HL_LOCATION 88,72
      ld (RAM_Pen_Smoothed), hl
      ld (RAM_Pen_Backup), hl
    ei
    ret

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
        ld hl, ($C08D)
        ld (RAM_Pen_Smoothed), hl
        ld hl, ($C08F)
        ld (RAM_Pen_Backup), hl
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
      ld ($C08D), hl
      ld hl, (RAM_Pen_Backup)
      ld ($C08F), hl

      LD_HL_LOCATION 88,72
      ld (RAM_Pen_Smoothed), hl
      ld (RAM_Pen_Backup), hl
    ei
    ret

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
        ld hl, ($C08D)
        ld (RAM_Pen_Smoothed), hl
        ld hl, ($C08F)
        ld (RAM_Pen_Backup), hl
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
      ld ($C08D), hl
      ld hl, (RAM_Pen_Backup)
      ld ($C08F), hl
      LD_HL_LOCATION 88,72
      ld (RAM_Pen_Smoothed), hl
      ld (RAM_Pen_Backup), hl
    ei
    ret

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
    ret z ; And not if we are just hiding it
    
    ; Otherwise, show it
    ld a, Mode1_Menu
    ld (RAM_CurrentMode), a
    ret

DrawTextToTilesWithBackup:
; h = y offset (within drawing area)
; l = x offset (within drawing area)
; b = rows
; c = columns

    ld a, ($C082)
    or a
    call nz, RestoreTileData_SaveRegisters
    ld a, $80
    ld ($C082), a
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
    ld ($C082), a
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
.enda

MenuText: ; $1a10
.db $B6 
.asc "/^^ MENU ^^,"
.db $FF 
.asc "[  EXIT    ]"
.db $FF 
.asc "[  COLOR   ]"
.db $FF 
.asc "[  ERASE   ]"
.db $FF 
.asc "[  SQUARE  ]"
.db $FF 
.asc "[  CIRCLE  ]"
.db $FF 
.asc "[  ELLIPSE ]"
.db $FF 
.asc "[  PAINT   ]"
.db $FF 
.asc "[  COPY    ]"
.db $FF 
.asc "[  MIRROR  ]"
.db $FF 
.asc "[  MAGNIFY ]"
.db $FF 
.asc "[  DISPLAY ]"
.db $FF 
.asc "[  END     ]"
.db $FF 
.asc "`__________'"
.db $FF 

ModeMenuText: ; $1ac7
.db $2C
.asc "/^ MODE ^,"
.db $FF
.asc "[  LINE  ]"
.db $FF
.asc "[  PAINT ]"
.db $FF
.asc "`________'"
.db $FF

ColorMenuText: ; $1af4
.db $44
.asc "/^ COLOR MENU ^,"
.db $FF
.asc "[  COLOR SET   ]"
.db $FF
.asc "[  BACK COLOR  ]"
.db $FF
.asc "`______________'"
.db $FF

MirrorMenuText: ; $1b39
.db $3C
.asc "/^ MODE SET ^,"
.db $FF
.asc "[  V-REVERSE ]"
.db $FF
.asc "[  H-REVERSE ]"
.db $FF
.asc "`____________'"
.db $FF

ColorPageMenuText: ; $1b76
.db $70
.asc "/^^ COLOR ^^,"
.db $FF
.asc "[           ]"
.db $FF
.asc "[           ]"
.db $FF
.asc "[           ]"
.db $FF
.asc "[           ]"
.db $FF
.asc "[  PAGE UP  ]"
.db $FF
.asc "[  PAGE DOWN]"
.db $FF
.asc "`___________'"
.db $FF

ColourSelectionTilemap: ; $1be7
; Tiles showing selectable colours
.dw $0991 $098E $0992 $098E $0993 $098E $0994 
.dw $098E $098E $098E $098E $098E $098E $098E 
.dw $0995 $098E $0996 $098E $0997 $098E $0998 

EraseMenuText: ; $1c11
.db $38
.asc "/^ ERASE ? ^,"
.db $FF
.asc "[  NO       ]"
.db $FF
.asc "[  YES      ]"
.db $FF
.asc "`___________'"
.db $FF

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
    ld ($C06D), a
    bit 2, a
    ld a, ($C08A)
    ld ($C06B), a
    ld hl, (RAM_Pen_Backup)
    ld de, (RAM_Pen_Smoothed)
    call nz, _LABEL_1CA1_
    ld a, (RAM_Pen_Smoothed.x)
    ld (RAM_Pen_Backup.x), a
    ld a, (RAM_Pen_Smoothed.y)
    ld (RAM_Pen_Backup.y), a
    ei
    ret

+:  di
    ld a, (RAM_ButtonsPressed)
    ld ($C06D), a
    ld hl, (RAM_Pen_Smoothed)
    exx
    ld a, ($C08A)
    ld ($C06B), a
    call _LABEL_1D40_
    ld a, (RAM_Pen_Smoothed.x)
    ld (RAM_Pen_Backup.x), a
    ld a, (RAM_Pen_Smoothed.y)
    ld (RAM_Pen_Backup.y), a
    ei
    ret

_LABEL_1CA1_:
    ld c, $00
    ld a, d
    sub h
    jp nc, +
    neg
    ld c, $01
+:  push bc
    push hl
      exx
    pop hl
    pop bc
    exx
    ld d, a
    ld a, e
    sub l
    jp c, _LABEL_1CF4_
    ld e, a
    ld a, d
    sub e
    jp c, +
    ld h, d
    srl h
    ld l, d
    inc l
-:  call _LABEL_1D40_
    dec l
    ret z
    call _LABEL_1D32_
    ld a, h
    sub e
    ld h, a
    jp nc, -
    add a, d
    ld h, a
    exx
      inc l
    exx
    jp -

+:  ld h, e
    srl h
    ld l, e
    inc l
-:  call _LABEL_1D40_
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
    call _LABEL_1D32_
    jp -

_LABEL_1CF4_:
    neg
    ld e, a
    ld a, d
    sub e
    jp c, +
    ld h, d
    srl h
    ld l, d
    inc l
-:  call _LABEL_1D40_
    dec l
    ret z
    call _LABEL_1D32_
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

+:  ld h, e
    srl h
    ld l, e
    inc l
-:  call _LABEL_1D40_
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
    call _LABEL_1D32_
    jp -

_LABEL_1D32_:
    ; If low bit of c' is 0, --h' else ++h'
    exx
    bit 0, c
    jp nz, +
    inc h
    jp ++
+:  dec h
++: exx
    ret

; Data from 1D3F to 1D3F (1 bytes)
    ret ; Unused

_LABEL_1D40_:
    ld a, ($C06D)
    bit 2, a
    ret z
    exx
    push bc
    push hl
      call _LABEL_1D50_
    pop hl
    pop bc
    exx
    ret

_LABEL_1D50_:
    ld a, l
    sub $3C
    ld ($C067), a
    inc a
    ld ($C069), a
    sub $02
    ld ($C06A), a
    ld a, h
    sub $24
    ld ($C063), a
    inc a
    ld ($C065), a
    sub $02
    ld ($C066), a
    ld a, ($C06B)
    or a
    jp z, +
    cp $01
    jp z, ++
    cp $02
    jp z, +++
    cp $03
    jp z, ++
    ret

+:  ld a, ($C063)
    ld ($C064), a
    ld a, ($C067)
    ld ($C068), a
    jp _LABEL_1E57_

++: ld a, ($C063)
    ld ($C064), a
    ld a, ($C067)
    ld ($C068), a
    call _LABEL_1E57_
    ld a, ($C063)
    ld ($C064), a
    ld a, ($C069)
    ld ($C068), a
    call _LABEL_1E57_
    ld a, ($C065)
    ld ($C064), a
    ld a, ($C067)
    ld ($C068), a
    call _LABEL_1E57_
    ld a, ($C065)
    ld ($C064), a
    ld a, ($C069)
    ld ($C068), a
    jp _LABEL_1E57_

+++:ld a, ($C066)
    ld ($C064), a
    ld a, ($C06A)
    ld ($C068), a
    call _LABEL_1E57_
    ld a, ($C066)
    ld ($C064), a
    ld a, ($C067)
    ld ($C068), a
    call _LABEL_1E57_
    ld a, ($C066)
    ld ($C064), a
    ld a, ($C069)
    ld ($C068), a
    call _LABEL_1E57_
    ld a, ($C063)
    ld ($C064), a
    ld a, ($C06A)
    ld ($C068), a
    call _LABEL_1E57_
    ld a, ($C063)
    ld ($C064), a
    ld a, ($C067)
    ld ($C068), a
    call _LABEL_1E57_
    ld a, ($C063)
    ld ($C064), a
    ld a, ($C069)
    ld ($C068), a
    call _LABEL_1E57_
    ld a, ($C065)
    ld ($C064), a
    ld a, ($C06A)
    ld ($C068), a
    call _LABEL_1E57_
    ld a, ($C065)
    ld ($C064), a
    ld a, ($C067)
    ld ($C068), a
    call _LABEL_1E57_
    ld a, ($C065)
    ld ($C064), a
    ld a, ($C069)
    ld ($C068), a
    jp _LABEL_1E57_ ; ### Unnecessary, could fall through

_LABEL_1E57_:
    ld a, ($C068)
    cp $90
    ret nc
    ld b, a
    ld a, ($C064)
    cp $B0
    ret nc
    call CheckForReset
    ld c, a
    ld a, b
    and $F8
    ld h, 0 ; calculate de = a * 88
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
    ld a, c
    and $F8
    LD_HL_A ; hl = a * 4 + de
    add hl, hl
    add hl, hl
    add hl, de
    ld a, b
    and $07
    add a, a
    add a, a
    LD_DE_A
    add hl, de
    ex de, hl
    push bc
      ld hl, RAM_TileModificationBuffer
      ld bc, 4
      call CopyVRAMToRAM
    pop bc
    ld a, c
    and $07
    push de
      ld hl, $1EDA
      LD_DE_A
      add hl, de
      ld a, (hl)
      ld hl, RAM_TileModificationBuffer
      ld e, a
      cpl
      ld d, a
      ld b, $04
      ld c, $00
      ld a, ($C06B)
      cp $03
      jp nc, _f
      ld a, ($C06C)
      ld c, a
      and a
__:   rrc c
      ld a, (hl)
      jp nc, +
      or e
      ld (hl), a
      jp ++
+:    and d
      ld (hl), a
++:   inc hl
      djnz _b
    pop de
    ld a, d
    or >VDPAddressMask_Write
    ld d, a
    ld hl, RAM_TileModificationBuffer
    ld bc, 4
    jp RawDataToVRAM

; Data from 1EDA to 1EE1 (8 bytes)
.db %10000000
.db %01000000
.db %00100000
.db %00010000
.db %00001000
.db %00000100
.db %00000010
.db %00000001

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
    ld a, $01
    ld ($C054), a
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

; Data from 1F46 to 1F65 (32 bytes)
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

; 6th entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode5_SquareFunction:
    ld a, ($C089)
    cp $03
    ret nz
    ld a, (RAM_Beep)
    or a
    ret nz
    exx
    push hl
      di
      ld a, ($C08A)
      ld b, a
      ld a, ($C0BA)
      or a
      jp z, +
      ld b, $00
+:    ld a, b
      ld ($C06B), a
      ld a, $04
      ld ($C06D), a
      ld iy, $C062
      ld h, (iy+13)
      ld l, (iy+12)
      ld d, h
      ld e, (iy+14)
      call _LABEL_1CA1_
      ld h, (iy+13)
      ld l, (iy+14)
      ld d, (iy+15)
      ld e, l
      call _LABEL_1CA1_
      ld h, (iy+15)
      ld l, (iy+14)
      ld d, h
      ld e, (iy+12)
      call _LABEL_1CA1_
      ld h, (iy+15)
      ld l, (iy+12)
      ld d, (iy+13)
      ld e, l
      call _LABEL_1CA1_
      ld a, ($C0BA)
      or a
      jp z, _LABEL_2079_
      ld a, (iy+16)
      or a
      jp nz, +
      inc a
+:    ld b, a
      ld a, (iy+15)
      sub (iy+13)
      cp $08
      jp c, _LABEL_2085_
      ld a, $24
      cp (iy+13)
      jp c, +
      cp (iy+15)
      jp nc, _LABEL_2079_
+:    ld a, $D4
      cp (iy+13)
      jp nc, +
      cp (iy+15)
      jp c, _LABEL_2079_
+:    ld a, (iy+13)
      call _LABEL_209E_
      ld (iy+13), a
      ld a, (iy+15)
      call _LABEL_209E_
      ld (iy+15), a
      ld a, (iy+12)
      add a, (iy+16)
      sub $3C
      jp c, _LABEL_2079_
      ld a, (iy+12)
      cp $CC
      jp c, +
      ld a, (iy+14)
      cp $CC
      jp nc, _LABEL_2079_
+:    ld a, (iy+12)
      sub $3C
      jp nc, +
      xor a
+:    ld (iy+12), a
      ld c, a
      ld a, (iy+14)
      sub $3C
      sub c
      ld b, a
-:    push bc
        ld a, (iy+12)
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
          pop de
          add hl, de
        pop de
        add hl, de
        push hl
          ld a, (iy+13)
          and $F8
          LD_HL_A
          add hl, hl
          add hl, hl
        pop de
        add hl, de
        ld a, (iy+12)
        and $07
        add a, a
        add a, a
        LD_DE_A
        add hl, de
        ex de, hl
        call _LABEL_20AA_
      pop bc
      inc (iy+12)
      ld a, (iy+12)
      cp $90
      jp nc, _LABEL_2079_
      djnz -

_LABEL_2079_:
      ld a, $F0
      ld ($C203), a
      xor a
      ld ($C089), a
    pop hl
    ei
    ret

_LABEL_2085_:
    ld h, (iy+13)
    ld l, (iy+12)
    ld d, (iy+15)
-:  push bc
    push de
    push hl
      ld e, l
      call _LABEL_1CA1_
    pop hl
    pop de
    pop bc
    inc l
    djnz -
    jp _LABEL_2079_

_LABEL_209E_:
    sub $24
    jp nc, +
    xor a
+:  cp $B0
    ret c
    ld a, $B0
    ret

_LABEL_20AA_:
    ld a, (iy+13)
    ld c, a
    and $07
    jp z, +
    push de
      ld hl, $219A
      LD_DE_A
      add hl, de
      ld a, (hl)
    pop de
    call _LABEL_213E_
    ld hl, $0020
    add hl, de
    ex de, hl
    ld a, (iy+13)
    add a, $08
    and $F8
    ld c, a
+:  ld a, (iy+15)
    and $F8
    sub c
    jp c, +
    and $F8
    rrca
    rrca
    rrca
    or a
    jp z, +
    ld b, a
-:  call ++
    ld hl, $0020
    add hl, de
    ex de, hl
    djnz -
+:  ld a, (iy+15)
    and $07
    ret z
    push de
      ld hl, $219A
      LD_DE_A
      add hl, de
    pop de
    ld a, (hl)
    cpl
    jp _LABEL_213E_

++: ld a, e
    out (Port_VDPAddress), a
    ld a, d
    or >VDPAddressMask_Write
    out (Port_VDPAddress), a
    ld hl, $00FF
    ld c, Port_VDPData
    ld a, (iy+10)
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

_LABEL_213E_:
    ld hl, RAM_TileModificationBuffer
    push de
      push af
        ld bc, 4
        call CopyVRAMToRAM
      pop af
      cpl
      ld b, a
      ld hl, RAM_TileModificationBuffer
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
      inc hl
      ld a, (hl)
      and b
      ld (hl), a
    pop de
    ld a, e
    out (Port_VDPAddress), a
    ld a, d
    or >VDPAddressMask_Write
    out (Port_VDPAddress), a
    ld a, b
    cpl
    ld hl, RAM_TileModificationBuffer
    push de
      ld b, a
      ld e, (iy+10)
      rrc e
      ld a, (hl)
      inc hl
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

; Data from 219A to 21A1 (8 bytes)
.db %11111111
.db %01111111
.db %00111111
.db %00011111
.db %00001111
.db %00000111
.db %00000011
.db %00000001

; 7th entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode6_CircleAnd7Function:
    ld a, (RAM_Beep)
    or a
    ret nz
    exx
    ld a, ($C089)
    rlca
    ret nc
    push hl
      ld a, ($C0AC)
      ld b, a
      ld a, $04
      ld ($C06D), a
      ld a, ($C08A)
      ld d, a
      ld a, ($C0BA)
      ld e, a
      or a
      jp z, +
      ld d, 0
+:    ld a, d
      ld ($C06B), a
      ld a, e
      ld de, ($C0AA)
      ld hl, ($C0A8)
      di
      call _LABEL_21E1_
    pop hl
    xor a
    ld ($C089), a
    ld a, $F0
    ld ($C203), a
    ei
    ret

_LABEL_21E1_:
      ex af, af'
      ld a, b
      cp $02
      ret c
      ld ($C0A8), hl
      ld a, h
      dec a
      or l
      jp z, +
      ld a, h
      or a
      jp z, _LABEL_22EC_
      ex de, hl
      push hl
      push bc
        call _LABEL_25F2_
      pop bc
      pop hl
      ex de, hl
      ld ($C0A8), a
      jp _LABEL_2435_

+:    ld l, e
      ld h, $00
      ld ($C0A4), hl
      ld l, d
      ld e, b
      ld d, h
      add hl, de
      ld ($C0A2), hl
      ld bc, $0000
      ld hl, $0000
      ld ($C0A6), hl
-:    call _LABEL_259B_
      ld hl, ($C0A6)
      inc hl
      or a
      sbc hl, de
      or a
      sbc hl, bc
      jp m, +
      add hl, bc
      or a
      sbc hl, de
      ld ($C0A6), hl
      dec de
      ld hl, ($C0A2)
      dec hl
      ld ($C0A2), hl
      jp ++

+:    add hl, de
      or a
      sbc hl, bc
      ld ($C0A6), hl
      dec bc
      ld hl, ($C0A4)
      dec hl
      ld ($C0A4), hl

++:   ld a, d
      or e
      jp nz, -

-:    call _LABEL_259B_
      ld hl, ($C0A6)
      inc hl
      add hl, bc
      or a
      sbc hl, de
      jp p, +
      or a
      sbc hl, bc
      or a
      sbc hl, de
      ld ($C0A6), hl
      dec de
      ld hl, ($C0A2)
      dec hl
      ld ($C0A2), hl
      jp ++

+:    add hl, bc
      add hl, de
      ld ($C0A6), hl
      inc bc
      ld hl, ($C0A4)
      inc hl
      ld ($C0A4), hl
++:   ld a, b
      or c
      jp nz, -

-:    call _LABEL_259B_
      ld hl, ($C0A6)
      inc hl
      add hl, bc
      or a
      adc hl, de
      jp m, +
      or a
      sbc hl, bc
      add hl, de
      ld ($C0A6), hl
      inc de
      ld hl, ($C0A2)
      inc hl
      ld ($C0A2), hl
      jp ++
+:    or a
      sbc hl, de
      add hl, bc
      ld ($C0A6), hl
      inc bc
      ld hl, ($C0A4)
      inc hl
      ld ($C0A4), hl
++:   ld a, d
      or e
      jp nz, -
-:    call _LABEL_259B_
      ld hl, ($C0A6)
      inc hl
      add hl, de
      or a
      sbc hl, bc
      jp p, +
      add hl, bc
      add hl, de
      ld ($C0A6), hl
      inc de
      ld hl, ($C0A2)
      inc hl
      ld ($C0A2), hl
      jp ++
+:    or a
      sbc hl, de
      or a
      sbc hl, bc
      ld ($C0A6), hl
      dec bc
      ld hl, ($C0A4)
      dec hl
      ld ($C0A4), hl
++:   ld a, b
      or c
      jp nz, -
      ret

_LABEL_22EC_:
    ld a, $7F
    ld ($C0A9), a
    ld l, e
    ld h, $00
    ld ($C0A4), hl
    ld l, d
    ld e, b
    ld d, h
    add hl, de
    ld ($C0A2), hl
    ld bc, $0000
    ld hl, $0000
    ld ($C0A6), hl
-:  call _LABEL_259B_
--: ld hl, ($C0A6)
    inc hl
    or a
    sbc hl, de
    or a
    sbc hl, bc
    jp m, +
    add hl, bc
    or a
    sbc hl, de
    ld ($C0A6), hl
    dec de
    ld hl, ($C0A2)
    dec hl
    ld ($C0A2), hl
    jp ++

+:  add hl, de
    or a
    sbc hl, bc
    ld ($C0A6), hl
    dec bc
    ld hl, ($C0A8)
    ld a, h
    sub l
    ld ($C0A9), a
    jp nc, +
    ld hl, ($C0A4)
    dec hl
    ld ($C0A4), hl
++: ld a, d
    or e
    jp nz, -
    jp _LABEL_2353_

+:  ld a, d
    or e
    jp nz, --
    jp _LABEL_2356_

_LABEL_2353_:
    call _LABEL_259B_
_LABEL_2356_:
    ld hl, ($C0A6)
    inc hl
    add hl, bc
    or a
    sbc hl, de
    jp p, +
    or a
    sbc hl, bc
    or a
    sbc hl, de
    ld ($C0A6), hl
    dec de
    ld hl, ($C0A2)
    dec hl
    ld ($C0A2), hl
    jp ++

+:  add hl, bc
    add hl, de
    ld ($C0A6), hl
    inc bc
    ld hl, ($C0A8)
    ld a, h
    add a, l
    ld ($C0A9), a
    jp nc, +
    ld hl, ($C0A4)
    inc hl
    ld ($C0A4), hl
++: ld a, b
    or c
    jp nz, _LABEL_2353_
    ld hl, $C0A9
    inc (hl)
    jp _LABEL_23A5_

+:  ld a, b
    or c
    jp nz, _LABEL_2356_
    ld hl, $C0A9
    inc (hl)
    jp _LABEL_23A8_

_LABEL_23A5_:
    call _LABEL_259B_
_LABEL_23A8_:
    ld hl, ($C0A6)
    inc hl
    add hl, bc
    or a
    adc hl, de
    jp m, +
    or a
    sbc hl, bc
    add hl, de
    ld ($C0A6), hl
    inc de
    ld hl, ($C0A2)
    inc hl
    ld ($C0A2), hl
    jp ++

+:  or a
    sbc hl, de
    add hl, bc
    ld ($C0A6), hl
    inc bc
    ld hl, ($C0A8)
    ld a, h
    add a, l
    ld ($C0A9), a
    jp nc, +
    ld hl, ($C0A4)
    inc hl
    ld ($C0A4), hl
++: ld a, d
    or e
    jp nz, _LABEL_23A5_
    jp _LABEL_23EF_

+:  ld a, d
    or e
    jp nz, _LABEL_23A8_
    jp _LABEL_23F2_

_LABEL_23EF_:
    call _LABEL_259B_
_LABEL_23F2_:
    ld hl, ($C0A6)
    inc hl
    add hl, de
    or a
    sbc hl, bc
    jp p, +
    add hl, bc
    add hl, de
    ld ($C0A6), hl
    inc de
    ld hl, ($C0A2)
    inc hl
    ld ($C0A2), hl
    jp ++

+:  or a
    sbc hl, de
    or a
    sbc hl, bc
    ld ($C0A6), hl
    dec bc
    ld hl, ($C0A8)
    ld a, h
    sub l
    ld ($C0A9), a
    jp nc, +
    ld hl, ($C0A4)
    dec hl
    ld ($C0A4), hl
++: ld a, b
    or c
    jp nz, _LABEL_23EF_
    ret

+:  ld a, b
    or c
    jp nz, _LABEL_23F2_
    ret

_LABEL_2435_:
    push bc
      push de
        ld l, b
        ld de, ($C0A8)
        call Multiply_l_e_hl
        ld a, l
        ld b, h
        add a, $80
        jp nc, +
        inc b
+:    pop de
      ld ($C0A9), a
      ld l, e
      ld h, $00
      ld ($C0A4), hl
      ld l, d
      ld e, b
      ld d, h
      add hl, de
      ld ($C0A2), hl
    pop de
    ld e, d
    ld d, $00
    ld bc, $0000
    ld hl, $0000
    ld ($C0A6), hl
--: call _LABEL_259B_
-:  ld hl, ($C0A6)
    inc hl
    or a
    sbc hl, de
    or a
    sbc hl, bc
    jp m, +
    add hl, bc
    or a
    sbc hl, de
    ld ($C0A6), hl
    dec de
    ld hl, ($C0A8)
    ld a, h
    sub l
    ld ($C0A9), a
    jp nc, +++
    ld hl, ($C0A2)
    dec hl
    ld ($C0A2), hl
    jp ++

+:  add hl, de
    or a
    sbc hl, bc
    ld ($C0A6), hl
    dec bc
    ld hl, ($C0A4)
    dec hl
    ld ($C0A4), hl
++: ld a, d
    or e
    jp nz, --
    ld hl, $C0A9
    dec (hl)
    jp _LABEL_24B9_

+++:ld a, d
    or e
    jp nz, -
    ld hl, $C0A9
    dec (hl)
    jp _LABEL_24BC_

_LABEL_24B9_:
    call _LABEL_259B_
_LABEL_24BC_:
    ld hl, ($C0A6)
    inc hl
    add hl, bc
    or a
    sbc hl, de
    jp p, +
    or a
    sbc hl, bc
    or a
    sbc hl, de
    ld ($C0A6), hl
    dec de
    ld hl, ($C0A8)
    ld a, h
    sub l
    ld ($C0A9), a
    jp nc, +++
    ld hl, ($C0A2)
    dec hl
    ld ($C0A2), hl
    jp ++

+:  add hl, bc
    add hl, de
    ld ($C0A6), hl
    inc bc
    ld hl, ($C0A4)
    inc hl
    ld ($C0A4), hl
++: ld a, b
    or c
    jp nz, _LABEL_24B9_
    jp _LABEL_2503_

+++:ld a, b
    or c
    jp nz, _LABEL_24BC_
    jp _LABEL_2506_

_LABEL_2503_:
    call _LABEL_259B_
_LABEL_2506_:
    ld hl, ($C0A6)
    inc hl
    add hl, bc
    or a
    adc hl, de
    jp m, +
    or a
    sbc hl, bc
    add hl, de
    ld ($C0A6), hl
    inc de
    ld hl, ($C0A8)
    ld a, h
    add a, l
    ld ($C0A9), a
    jp nc, +++
    ld hl, ($C0A2)
    inc hl
    ld ($C0A2), hl
    jp ++

+:  or a
    sbc hl, de
    add hl, bc
    ld ($C0A6), hl
    inc bc
    ld hl, ($C0A4)
    inc hl
    ld ($C0A4), hl
++: ld a, d
    or e
    jp nz, _LABEL_2503_
    ld hl, $C0A9
    inc (hl)
    jp _LABEL_2555_

+++:ld a, d
    or e
    jp nz, _LABEL_2506_
    ld hl, $C0A9
    inc (hl)
    jp _LABEL_2558_

_LABEL_2555_:
    call _LABEL_259B_
_LABEL_2558_:
    ld hl, ($C0A6)
    inc hl
    add hl, de
    or a
    sbc hl, bc
    jp p, +
    add hl, bc
    add hl, de
    ld ($C0A6), hl
    inc de
    ld hl, ($C0A8)
    ld a, h
    sub l
    ld ($C0A9), a
    jp nc, +++
    ld hl, ($C0A2)
    inc hl
    ld ($C0A2), hl
    jp ++

+:  or a
    sbc hl, de
    or a
    sbc hl, bc
    ld ($C0A6), hl
    dec bc
    ld hl, ($C0A4)
    dec hl
    ld ($C0A4), hl
++: ld a, b
    or c
    jp nz, _LABEL_2555_
    ret

+++:ld a, b
    or c
    jp nz, _LABEL_2558_
    ret

_LABEL_259B_:
    push bc
    push de
    push hl
      ld hl, ($C0A2)
      ld a, h
      or a
      jp z, ++
      xor a
      bit 7, h
      ld h, a
      jp nz, +
      cpl
+:    ld l, a
++:   ld de, ($C0A4)
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
      or a
      jp nz, +
      ex af, af'
      ld a, l
      ld l, e
      ld h, a
      call _LABEL_1D50_
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
      ld d, l
      ld hl, ($C0AA)
      call _LABEL_1CA1_
    pop hl
    pop de
    pop bc
    ret

; Data from 25DD to 25F1 (21 bytes)
.db $3E $10 $21 $00 $00 $29 $CB $11 $CB $10 $30 $04 $19 $30 $01 $03
.db $3D $C2 $E2 $25 $C9
; TODO unused code here

_LABEL_25F2_:
    ld hl, $0100
    ld b, $08
    xor a
-:  add a, a
    inc a
    add hl, hl
    sbc hl, de
    jp nc, +
    dec a
    add hl, de
+:  djnz -
    ret

; 9th entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode8_PaintFunction:
    ld a, (RAM_Beep)
    or a
    ret nz
    ld a, ($C089)
    or a
    ret z
    ld de, ($C091)
    ld a, d
    sub $28
    ld d, a
    ld a, e
    sub $40
    cp $90
    jp nc, +
    ld h, a
    ld a, d
    cp $B0
    jp nc, +
    ld l, a
    ex de, hl
    di
    call _LABEL_2792_
    ei
    ld a, ($C0B2)
    ld b, a
    ld a, ($C06C)
    cp b
    jp z, +
    xor a
    ld ($C06B), a
    di
    call _LABEL_2646_
    ei
+:  xor a
    ld ($C089), a
    ret

_LABEL_2646_:
    push af
    push bc
    push de
    push hl
      ld a, d
      cp $90
      jp nc, +++
      ld a, e
      ld ($C0AB), a
      ld a, d
      ld ($C0AA), a
      call _LABEL_2752_
      or a
      jp nz, +++
      ld hl, $0000
      ld ($C0AC), hl
---:
-:    ld a, ($C0AB)
      or a
      jr z, +
      dec a
      ld e, a
      ld a, ($C0AA)
      ld d, a
      call _LABEL_2752_
      or a
      jr nz, +
      ld a, e
      ld ($C0AB), a
      jr -
+:    ld a, $01
      ld ($C0B0), a
      ld ($C0B1), a
--:
      ld a, ($C0B0)
      ld ($C0AE), a
      ld a, ($C0B1)
      ld ($C0AF), a
      ld a, ($C0AA)
      cp $8F
      ld a, $01
      jr z, +
      ld a, ($C0AB)
      ld e, a
      ld a, ($C0AA)
      inc a
      ld d, a
      call _LABEL_2752_
+:    ld ($C0B1), a
      ld a, ($C0AA)
      cp $00
      ld a, $01
      jr z, +
      ld a, ($C0AB)
      ld e, a
      ld a, ($C0AA)
      dec a
      ld d, a
      call _LABEL_2752_
+:    ld ($C0B0), a
      ld a, ($C0AE)
      ld b, a
      ld a, ($C0B0)
      xor $01
      and b
      jr z, +
      ld hl, ($C0AC)
      inc hl
      ld ($C0AC), hl
      ld a, ($C0AB)
      ld l, a
      ld a, ($C0AA)
      dec a
      ld h, a
      push hl
+:      ld a, ($C0AF)
        ld b, a
        ld a, ($C0B1)
        xor $01
        and b
        jr z, +
        ld hl, ($C0AC)
        inc hl
        ld ($C0AC), hl
        ld a, ($C0AB)
        ld l, a
        ld a, ($C0AA)
        inc a
        ld h, a
        push hl
+:        ld a, ($C0AB)
          ld e, a
          ld a, ($C0AA)
          ld d, a
          ld a, $01
          call _LABEL_2850_
          ld a, ($C0AB)
          cp $AF
          jr z, _f
          inc a
          ld e, a
          ld a, ($C0AA)
          ld d, a
          call _LABEL_2752_
          or a
          jr nz, _f
          ld a, ($C0AB)
          inc a
          ld ($C0AB), a
          jp --

__:       ld hl, ($C0AC)
          ld a, h
          or l
          jr z, +++
        pop hl
        ld a, l
        ld ($C0AB), a
        ld a, h
        ld ($C0AA), a
        ld hl, ($C0AC)
        dec hl
        ld ($C0AC), hl
        ld a, ($C0AB)
        ld e, a
        ld a, ($C0AA)
        ld d, a
        call _LABEL_2752_
        or a
        jr nz, _b
        jp ---

.endasm ; push/pop matching
pop hl
.asm
        
+++:pop hl
    pop de
    pop bc
    pop af
    ret

_LABEL_2752_:
    push bc
    push de
    push hl
      push de
        call _LABEL_27CD_
        ex af, af'
      pop de
      ld a, e
      cpl
      and 7
      ld c, a
      ex af, af'
      inc c
      dec c
      jp z, _LABEL_2752_0
      dec c
      jp z, _LABEL_2752_1
      dec c
      jp z, _LABEL_2752_2
      dec c
      jp z, _LABEL_2752_3
      dec c
      jp z, _LABEL_2752_4
      dec c
      jp z, _LABEL_2752_5
      dec c
      jp z, _LABEL_2752_6
      jp _LABEL_2752_7

_LABEL_2752_0: rlca
_LABEL_2752_1: rlca
_LABEL_2752_2: rlca
_LABEL_2752_3: rlca
_LABEL_2752_4: rlca
_LABEL_2752_5: rlca
_LABEL_2752_6: rlca
_LABEL_2752_7: rlca
    pop hl
    pop de
    pop bc
    ld a, 0
    ret nc
    ld a, 1
    ret

_LABEL_2792_:
    push de
      call _LABEL_2812_
      ld a, l
      ld hl, RAM_TileModificationBuffer
      push hl
        and $07
        ld de, $1EDA
        LD_HL_A
        add hl, de
        ld d, (hl)
      pop hl
      ld c, $00
      ld a, (hl)
      and d
      jp z, +
      set 0, c
+:    inc hl
      ld a, (hl)
      and d
      jp z, +
      set 1, c
+:    inc hl
      ld a, (hl)
      and d
      jp z, +
      set 2, c
+:    inc hl
      ld a, (hl)
      and d
      jp z, +
      set 3, c
+:    ld a, c
      ld ($C0B2), a
    pop de
    ret

_LABEL_27CD_:
    call _LABEL_2812_
    ld a, ($C0B2)
    call _LABEL_27D8_
    cpl
    ret

_LABEL_27D8_:
    ld hl, RAM_TileModificationBuffer
    push bc
      ld b, $04
      ld a, ($C0B2)
      or a
      jp z, ++
      ld c, a
      push bc
      push hl
        xor a
-:      rrc c
        jp c, +
        or (hl)
+:      inc hl
        djnz -
      pop hl
      pop bc
      cpl
      ld d, a
      ld a, $FF
-:    rrc c
      jp nc, +
      and (hl)
+:    inc hl
      djnz -
      and d
      jp +

++:   push hl
        xor a
        or (hl)
        inc hl
        or (hl)
        inc hl
        or (hl)
        inc hl
        or (hl)
      pop hl
      cpl
+:  pop bc
    ret

_LABEL_2812_:
    ld a, d
    and $F8
    ld h, $00
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
    ex de, hl
    push hl
    push de
    push bc
      ld hl, RAM_TileModificationBuffer
      ld bc, 4
      call CopyVRAMToRAM
    pop bc
    pop de
    pop hl
    ld a, l
    and $F8
    or l
    cpl
    and $07
    ret

_LABEL_2850_:
    push de
    push hl
    push bc
      ld a, d
      ld ($C068), a
      ld a, e
      ld ($C064), a
      call _LABEL_1E57_
    pop bc
    pop hl
    pop de
    ret

; 10th entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode9_CopyFunction:
    exx
    ld a, ($C089)
    bit 3, a
    jp z, _LABEL_3932_
    ld a, (RAM_Beep)
    or a
    ret nz
    ld ix, $C15D
    ld a, ($C0C4)
    sub $17
    ld d, a
    ld ($C15E), a
    ld a, ($C0C6)
    sub $17
    sub d
    inc a
    ld ($C160), a
    ld a, ($C0C5)
    sub $28
    ld ($C15F), a
    ld e, a
    ld a, ($C0C7)
    sub $28
    sub e
    inc a
    ld ($C161), a
    ld hl, RAM_GraphicsDataBuffer
    ld ($C171), hl
    ld a, ($C0C8)
    sub $17
    ld ($C162), a
    ld a, ($C0C9)
    sub $28
    ld (RAM_TitleScreenAndEndTimeout), a
    di
    bit 0, (ix+0)
    call z, _LABEL_2BD8_
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
-:    ld a, e
      and $07
      ld c, a
      ld a, l
      and $07
      sub c
      jp nc, +
      add a, $08
+:    ld ($C166), a
      push bc
        call _LABEL_2AB1_
        call _LABEL_2ADC_
      pop bc
      inc e
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
    ld a, h
    cp $90
    jp nc, +
    djnz --
+:  ei
    ld a, ($C089)
    and $06
    ld ($C089), a
    ret

; 11th entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode10_MirrorFunction:
    exx
    xor a
    ld a, ($C089)
    rra
    jp nc, _LABEL_3ACE_
    rra
    ret nc
    rra
    jp nc, _LABEL_3932_
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
    ld a, ($C0C4)
    sub $17
    ld d, a
    ld ($C15E), a
    ld a, ($C0C6)
    sub $17
    sub d
    inc a
    ld ($C160), a
    ld a, ($C0C5)
    sub $28
    ld ($C15F), a
    ld e, a
    ld a, ($C0C7)
    sub $28
    sub e
    inc a
    ld ($C161), a
    ld hl, RAM_GraphicsDataBuffer
    ld ($C171), hl
    ld a, ($C0BA)
    or a
    jp nz, _LABEL_2A0B_
    ld hl, $29B3
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

; Data from 29B3 to 2A0A (88 bytes)
.db $F3 $DD $CB $00 $46 $CC $D8 $2B $DD $46 $03 $DD $4E $04 $DD $7E
.db $01 $E6 $07 $80 $3D $57 $DD $7E $02 $E6 $07 $5F $DD $66 $05 $DD
.db $6E $02 $C5 $D5 $E5 $7C $FE $90 $D2 $01 $2A $41 $7B $E6 $07 $4F
.db $7D $E6 $07 $91 $D2 $EC $29 $C6 $08 $32 $66 $C1 $C5 $CD $B1 $2A
.db $CD $DC $2A $C1 $1C $2C $7D $FE $B0 $D2 $01 $2A $10 $DE $E1 $D1
.db $C1 $15 $24 $10 $CD $C3 $A9 $2A

_LABEL_2A0B_:
    ld hl, $2A52
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
      
; Data from 2A52 to 2AB0 (95 bytes)
.db $F3 $DD $CB $00 $46 $CC $D8 $2B $DD $46 $03 $DD $4E $04 $DD $7E
.db $01 $E6 $07 $57 $DD $7E $02 $E6 $07 $DD $86 $04 $3D $5F $DD $66
.db $01 $DD $6E $06 $C5 $D5 $E5 $41 $7B $E6 $07 $4F $7D $E6 $07 $91
.db $D2 $87 $2A $C6 $08 $32 $66 $C1 $7D $FE $B0 $D2 $98 $2A $C5 $CD
.db $B1 $2A $CD $DC $2A $C1 $1D $2C $10 $DE $E1 $D1 $C1 $14 $24 $7C
.db $FE $90 $D2 $A9 $2A $10 $CD $FB $AF $32 $89 $C0 $C3 $13 $3B

_LABEL_2AB1_:
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

_LABEL_2ADC_:
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

_LABEL_2BD8_:
    set 0, (ix+0)
    push hl
    push de
    push bc
      ld a, ($C0C4)
      sub $17
      ld d, a
      ld a, ($C0C6)
      sub $18
      ld e, a
      sub d
      and $F8
      rrca
      rrca
      rrca
      inc a
      ld b, a
      ld a, d
      and $07
      jp z, +
      inc b
+:    ld a, ($C0C5)
      sub $28
      ld e, a
      ld a, ($C0C7)
      sub $29
      ld d, a
      sub e
      and $F8
      rrca
      rrca
      rrca
      inc a
      ld c, a
      ld a, e
      and $07
      jp z, +
      inc c
+:    ld a, (ix+1)
      and $F8
      ld d, a
      ld e, (ix+2)
      call _LABEL_2B74_
      ld hl, ($C171)
--:   VDP_ADDRESS_TO_DE
      push bc
        ld b, $00
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

; 12th entry of Jump Table from 165C (indexed by RAM_CurrentMode)
NonVBlankMode11_MagnifyFunction:
    exx
    bit 7, (hl)
    jp z, _LABEL_2D05_
    call _LABEL_3932_
    ld ix, $C15D
    ld a, ($C089)
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
    bit 2, a
    ret z
    di
    ld a, ($C08A)
    cp $03
    jp z, +
    ld a, $01
+:  ld ($C06B), a
    push hl
      ld a, $24
      add a, h
      ld h, a
      ld a, $3C
      add a, l
      ld l, a
      call _LABEL_1D50_
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
    ld ($C06B), a
    ld a, ($C06C)
    ld b, a
    push af
      ld a, ($C08A)
      cp $03
      jp nz, +
      ld b, $00
+:    ld a, b
      ld ($C06C), a
      call _LABEL_1D50_
    pop af
    ld ($C06C), a
    ei
    ret

_LABEL_2CF6_:
    di
    push hl
      call EnableOnlyThreeSprites
      call RestoreTileData
    pop hl
    ei
    ld a, $01
    ld (RAM_Beep), a
_LABEL_2D05_:
    set 7, (hl)
    ld a, $02
    ld ($C089), a
    ret

_LABEL_2D0D_: ; magnify mode?
    ex af, af'
    ld a, (RAM_Beep)
    or a
    ret nz
    ex af, af'
    or $40
    ld ($C089), a
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
    ld a, ($C0C4)
    sub $16
    ld d, a
    ld ($C15E), a
    ld a, ($C0C6)
    sub $17
    sub d
    ld ($C160), a
    ld a, ($C0C5)
    sub $27
    ld ($C15F), a
    ld e, a
    ld a, ($C0C7)
    sub $28
    sub e
    ld ($C161), a
    ld hl, $D000
    ld ($C171), hl
    bit 0, (ix+0)
    call z, _LABEL_2BD8_
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
        call _LABEL_2AB1_
        call _LABEL_2ADC_
        inc l
        call _LABEL_2DED_
        call _LABEL_2AB1_
        call _LABEL_2ADC_
        dec l
        inc h
        call _LABEL_2DED_
        call _LABEL_2AB1_
        call _LABEL_2ADC_
        inc l
        call _LABEL_2DED_
        call _LABEL_2AB1_
        call _LABEL_2ADC_
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
.db $5A $00 $00 $00 $00 $00 $00 $00 $00 $2A $FF $00 $00 $00 $00 $00
.db $00 $00 $00 $2A $FF $00 $00 $00 $00 $00 $00 $00 $00 $2A $FF $00
.db $00 $00 $00 $00 $00 $00 $00 $2A $FF $00 $00 $00 $00 $00 $00 $00
.db $00 $2A $FF $00 $00 $00 $00 $00 $00 $00 $00 $2A $FF $00 $00 $00
.db $00 $00 $00 $00 $00 $2A $FF $00 $00 $00 $00 $00 $00 $00 $00 $2A
.db $FF $2C $2C $2C $2C $2C $2C $2C $2C $2B $FF $5A $28 $28 $28 $28
.db $28 $28 $28 $28 $29 $FF $00 $00 $00 $00 $00 $00 $00 $00 $2A $FF
.db $00 $00 $00 $00 $00 $00 $00 $00 $2A $FF $00 $00 $00 $00 $00 $00
.db $00 $00 $2A $FF $00 $00 $00 $00 $00 $00 $00 $00 $2A $FF $00 $00
.db $00 $00 $00 $00 $00 $00 $2A $FF $00 $00 $00 $00 $00 $00 $00 $00
.db $2A $FF $00 $00 $00 $00 $00 $00 $00 $00 $2A $FF $00 $00 $00 $00
.db $00 $00 $00 $00 $2A $FF $5A $2E $00 $00 $00 $00 $00 $00 $00 $00
.db $FF $2E $00 $00 $00 $00 $00 $00 $00 $00 $FF $2E $00 $00 $00 $00
.db $00 $00 $00 $00 $FF $2E $00 $00 $00 $00 $00 $00 $00 $00 $FF $2E
.db $00 $00 $00 $00 $00 $00 $00 $00 $FF $2E $00 $00 $00 $00 $00 $00
.db $00 $00 $FF $2E $00 $00 $00 $00 $00 $00 $00 $00 $FF $2E $00 $00
.db $00 $00 $00 $00 $00 $00 $FF $2D $2C $2C $2C $2C $2C $2C $2C $2C
.db $FF $5A $27 $28 $28 $28 $28 $28 $28 $28 $28 $FF $2E $00 $00 $00
.db $00 $00 $00 $00 $00 $FF $2E $00 $00 $00 $00 $00 $00 $00 $00 $FF
.db $2E $00 $00 $00 $00 $00 $00 $00 $00 $FF $2E $00 $00 $00 $00 $00
.db $00 $00 $00 $FF $2E $00 $00 $00 $00 $00 $00 $00 $00 $FF $2E $00
.db $00 $00 $00 $00 $00 $00 $00 $FF $2E $00 $00 $00 $00 $00 $00 $00
.db $00 $FF $2E $00 $00 $00 $00 $00 $00 $00 $00 $FF $18 $57 $28 $67
.db $CB $38 $FE $2D $00 $00 $68 $A7 $28 $67 $4B $3B $59 $2E $00 $09
.db $18 $57 $98 $D7 $E7 $38 $B4 $2E $0D $00 $68 $A7 $98 $D7 $67 $3B
.db $0F $2F $0D $09

CallNonVBlankModeSpritesHandler:
    ld hl, RAM_ButtonsNewlyPressed ; used in functions later
    ld a, (RAM_Pen_Smoothed.y)
    ld b, a
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
    jp c, _LABEL_36A5_ ; Less than 47 = near top of screen
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
      ld hl, ModeSpritesHandlerJumpTable
      jp JumpToFunction

; 3rd entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
DoNothing:
    ret

ModeSpritesHandlerJumpTable:
; Jump Table from 2FD0 to 2FF3 (18 entries, indexed by RAM_CurrentMode)
; Called inside shadow registers
; Non-shadow regs have b = pen Y, (hl) = buttons newly pressed, a = mode
.dw Mode0_DrawingSpritesHandler
.dw Mode1_MenuSpritesHandler 
.dw DoNothing 
.dw Mode4_EraseSpritesHandler 
.dw DoNothing 
.dw Mode6_CircleSpritesHandler 
.dw Mode7_EllipseSpritesHandler 
.dw Mode8_PaintSpritesHandler
.dw Mode9_CopySpritesHandler 
.dw Mode10_MirrorSpritesHandler 
.dw Mode11_MagnifySpritesHandler 
.dw Mode12_DisplaySpritesHandler 
.dw DoNothing 
.dw DoNothing 
.dw Mode15_ColourSelectionMenuPlusSpritesHandler 
.dw Mode15_ColourSelectionMenuPlusSpritesHandler
.dw Mode15_ColourSelectionMenuPlusSpritesHandler 
.dw Mode15_ColourSelectionMenuPlusSpritesHandler

; 1st entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
Mode0_DrawingSpritesHandler:
      ; shadow regs
      call CheckMenuButton
    exx
    ; Set sprite 0 to the pen location
    ld a, (RAM_Pen_Smoothed.x)
    ld (RAM_SpriteTable1.xn + 0), a
    ld a, b
    ld (RAM_SpriteTable1.y + 0), a
    xor a ; CursorTile_Crosshair
    jp SetCursorIndex ; and ret

; 2nd entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
Mode1_MenuSpritesHandler:
    exx
    ; Set sprite 0 to x = 88...
    ld a, 88
    ld (RAM_SpriteTable1.xn), a
    ; ...y  = pen Y rounded to 8px, in the range 64..152 (menu area)
    ld a, b
    and %11111000 ; Round to 8px
    cp 64         ; If <64, set to 64
    jp nc, +
    ld a, 64
+:  cp 152
    jp c, +
    ld a, 152
+:  ld (RAM_SpriteTable1.y), a
    sub 64 ; Now it's relative to the menu area
    bit 1, (hl) ; Check for DO button
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

++: ld a, CursorTile_MenuArrowRight
    jp SetCursorIndex ; and ret

; 4th entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
Mode4_EraseSpritesHandler:
      call CheckMenuButton
    exx
    ld a, b
    and $F0
    cp $58
    jp nc, _LABEL_30B9_
    and $F0
    cp $40
    jr nc, +
    ld a, $40
+:  ld b, a
    dec a
    ld (RAM_SpriteTable1.y), a
    ld a, (RAM_Pen_Smoothed.x)
    and $F0
    cp $70
    jp nc, +
    ld a, $70
+:  cp $A0
    jp c, +
    ld a, $A0
+:  ld (RAM_SpriteTable1.xn), a
    ld c, a
    ld a, CursorTile_Square
    call SetCursorIndex
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_Beep), a
    ld a, b
    sub $40
    rrca
    rrca
    ld b, a
    ld a, c
    sub $70
    rrca
    rrca
    rrca
    rrca
    or b
    ld c, a
    ld a, (RAM_ColourSelectionStartValue)
    add a, c
    and $3F
    ld c, a
    ld a, ($C0BA)
    or a
    jp nz, _LABEL_30AF_
    ld a, ($C06C)
    LD_DE_A
    ld hl, RAM_Palette
    add hl, de
    ld (hl), c
    ld a, $01
    ld ($C054), a
    ret

_LABEL_30AF_:
    ld a, c
    ld ($C052), a
    ld a, $01
    ld ($C054), a
    ret

_LABEL_30B9_:
    cp $60
    jp nc, +
    ld a, $60
+:  cp $68
    jp c, +
    ld a, $68
+:  dec a
    ld (RAM_SpriteTable1.y), a
    ex af, af'
    ld a, $58
    ld (RAM_SpriteTable1.xn), a
    ld a, CursorTile_MenuArrowRight
    call SetCursorIndex
    bit 1, (hl)
    ret z
    ex af, af'
    ld b, $04
    cp $60
    jp c, +
    ld b, $FC
+:  ld a, (RAM_ColourSelectionStartValue)
    add a, b
    and $3F
    ld (RAM_ColourSelectionStartValue), a
    ld a, $01
    ld ($C054), a
    ld a, $01
    ld (RAM_Beep), a
    ret

; 6th entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
Mode6_CircleSpritesHandler:
      call CheckMenuButton
    exx
    ld a, ($C089)
    bit 1, a
    ret nz
    ld d, a
    ld a, b
    ld (RAM_SpriteTable1.y), a
    ld a, (RAM_Pen_Smoothed.x)
    ld (RAM_SpriteTable1.xn), a
    push hl
      bit 0, d
      jp nz, +
      ld a, CursorTile_ArrowBottomRight
      call SetCursorIndex
    pop hl
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_Beep), a
    ld a, (RAM_SpriteTable1.y)
    ld ($C203), a
    ld a, (RAM_SpriteTable1.xn)
    ld ($C246), a
    ld a, $A9
    ld ($C247), a
    ld de, (RAM_Pen_Smoothed)
    ld a, $04
    add a, e
    ld e, a
    ld a, $03
    add a, d
    ld d, a
    ld ($C06E), de
    ld a, ($C089)
    set 0, a
    ld ($C089), a
    ld a, SetCursorIndex_Second | CursorTile_ArrowBottomRight
    jp SetCursorIndex ; and ret

.endasm ; Unmatched push matching
push hl
.asm
    
+:    ld a, $04
      call SetCursorIndex
    pop hl
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_Beep), a
    ld hl, ($C06E)
    ld a, ($C246)
    ld b, a
    ld a, (RAM_SpriteTable1.xn)
    sub b
    sub $07
    ld b, a
    add a, h
    ld ($C071), a
    ld a, ($C203)
    ld d, a
    ld a, (RAM_SpriteTable1.y)
    sub $07
    sub d
    ld d, a
    push af
      add a, l
      ld ($C070), a
    pop af
    ld a, d
    jp nc, +
    neg
+:  ld ($C072), a
    ld hl, ($C06E)
    ld de, ($C070)
    ld a, l
    cp e
    jp c, +
    ld b, l
    ld l, e
    ld e, b
+:  ld a, h
    cp d
    jp c, +
    ld b, h
    ld h, d
    ld d, b
+:  ld ($C06E), hl
    ld ($C070), de
    ld a, ($C089)
    set 1, a
    ld ($C089), a
    ret

; 8th entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
Mode8_PaintSpritesHandler:
      ld a, $01
      and a
      ex af, af'
      jp +

; 7th entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
Mode7_EllipseSpritesHandler:
      xor a
      ex af, af'
+:    call CheckMenuButton
      ld a, CursorTile_X
      call SetCursorIndex
      ld hl, $C089
      bit 0, (hl)
      jp z, _LABEL_3206_
      bit 1, (hl)
      ret nz
    exx
    ld a, ($C203)
    ld c, a
    ex af, af'
    jp nz, +
    ld b, c
+:  ex af, af'
    ld a, b
    ld (RAM_SpriteTable1.y), a
    ld a, (RAM_Pen_Smoothed.x)
    ld (RAM_SpriteTable1.xn), a
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_Beep), a
    ld a, ($C246)
    ld b, a
    ld a, (RAM_SpriteTable1.xn)
    sub b
    jp nc, +
    neg
+:  ld ($C0AC), a
    ld hl, $0100
    ld ($C0A8), hl
    ex af, af'
    call nz, _LABEL_323B_
    exx
    ld (hl), $83
    ret

_LABEL_3206_:
    exx
    ld a, b
    ld (RAM_SpriteTable1.y), a
    ld a, (RAM_Pen_Smoothed.x)
    ld (RAM_SpriteTable1.xn), a
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_Beep), a
    ld a, (RAM_SpriteTable1.y)
    ld ($C203), a
    ld a, (RAM_SpriteTable1.xn)
    ld ($C246), a
    ld a, $A9
    ld ($C247), a
    ld de, (RAM_Pen_Smoothed)
    ld ($C0AA), de
    ld a, SetCursorIndex_Second | CursorTile_X
    call SetCursorIndex
    exx
    set 0, (hl)
    ret

_LABEL_323B_:
    ld a, ($C246)
    ld b, a
    ld a, (RAM_SpriteTable1.xn)
    sub b
    jr nc, +
    cpl
+:  ld b, a
    ld a, ($C203)
    ld c, a
    ld a, (RAM_SpriteTable1.y)
    sub c
    jr nc, +
    cpl
+:  ld h, a
    cp b
    jr nc, +
    ld a, b
+:  ld ($C0AC), a
    ld e, b
    ld l, $00
    call DivMod_hl_e_hl_a
    ld ($C0A8), hl
    ret

; 9th entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
Mode9_CopySpritesHandler:
      call CheckMenuButton
      ld hl, $C089
      ld a, (hl)
      or a
      ret nz
    exx
      ld a, b
      ld (RAM_SpriteTable1.y), a
      ld a, (RAM_Pen_Smoothed.x)
      ld (RAM_SpriteTable1.xn), a
      ld a, CursorTile_ArrowTopLeft
      call SetCursorIndex
      bit 1, (hl)
      ret z
      ld a, $01
      ld (RAM_Beep), a
      ld a, (RAM_Pen_Smoothed.x)
      ld h, a
      ld a, (RAM_Pen_Smoothed.y)
      ld l, a
      ld ($C091), hl
    exx
    ld (hl), $FF
    ret

; 10th entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
Mode10_MirrorSpritesHandler:
      call CheckMenuButton
    exx
    ld a, ($C089)
    bit 3, a
    ret nz
    bit 2, a
    jp nz, _LABEL_3385_
    bit 1, a
    jp nz, _LABEL_3311_
    ld c, $10
    ld a, b
    cp c
    jp nc, +
    ld a, c
+:  ld c, $98
    cp c
    jp c, +
    ld a, c
+:  ld (RAM_SpriteTable1.y), a
    ld a, (RAM_Pen_Smoothed.x)
    ld c, $21
    cp c
    jp nc, +
    ld a, c
+:  ld c, $C9
    cp c
    jp c, +
    ld a, c
+:  ld (RAM_SpriteTable1.xn), a
    ld a, CursorTile_ArrowBottomRight
    call SetCursorIndex
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_Beep), a
    ld a, ($C089)
    set 1, a
    ld ($C089), a
    ld a, (RAM_SpriteTable1.y)
    ld ($C203), a
    add a, $07
    ld ($C0C4), a
    add a, $08
    ld (RAM_SpriteTable1.y), a
    ld a, (RAM_SpriteTable1.xn)
    ld ($C246), a
    add a, $07
    ld ($C0C5), a
    add a, $08
    ld (RAM_SpriteTable1.xn), a
    ld a, $A9
    ld ($C247), a
    xor a
    ld ($C15D), a
    ld a, SetCursorIndex_Second | CursorTile_ArrowBottomRight
    jp SetCursorIndex ; and ret

_LABEL_3311_:
    ld a, ($C0C4)
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
    jp nc, _LABEL_332A_
    ld b, c
_LABEL_332A_:
    ld a, b
    ld c, $A6
    cp c
    jp c, +
    ld a, c
+:  ld (RAM_SpriteTable1.y), a
    ld ($C0C6), a
    ld a, (RAM_Pen_Smoothed.x)
    ld b, a
    ld a, ($C0C5)
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
+:  ld (RAM_SpriteTable1.xn), a
    ld ($C0C7), a
    ld a, CursorTile_ArrowTopLeft
    call SetCursorIndex
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_Beep), a
    ld a, ($C089)
    set 2, a
    ld ($C089), a
    ld a, (RAM_SpriteTable1.y)
    ld ($C0C6), a
    ld a, (RAM_SpriteTable1.xn)
    ld ($C0C7), a
    ret

_LABEL_3385_:
    ld c, $10
    ld a, b
    cp c
    jp nc, +
    ld a, c
+:  ld c, $97
    cp c
    jp c, +
    ld a, c
+:  ld (RAM_SpriteTable1.y), a
    ld a, (RAM_Pen_Smoothed.x)
    ld c, $21
    cp c
    jp nc, +
    ld a, c
+:  ld c, $C9
    cp c
    jp c, +
    ld a, c
+:  ld (RAM_SpriteTable1.xn), a
    ld a, CursorTile_ArrowBottomRight
    call SetCursorIndex
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_Beep), a
    ld a, (RAM_SpriteTable1.y)
    add a, $07
    ld ($C0C8), a
    ld a, (RAM_SpriteTable1.xn)
    add a, $07
    ld ($C0C9), a
    ld a, ($C089)
    set 3, a
    ld ($C089), a
    ret

; 11th entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
Mode11_MagnifySpritesHandler:
      call CheckMenuButton
    exx
    ld a, ($C089)
    ld d, a
    rrca
    jp nc, _LABEL_3527_
    ld iy, $C0BA
    rrca
    jp nc, _LABEL_3469_
    rrca
    ret c
    ld a, ($C0C4)
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
    ld ($C0C6), a
    ld a, (RAM_Pen_Smoothed.x)
    ld b, a
    ld a, ($C0C5)
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
    ld ($C0C7), a
    ld a, CursorTile_ArrowTopLeft
    call SetCursorIndex
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_Beep), a
    ld a, ($C089)
    set 2, a
    ld ($C089), a
    ld a, (RAM_SpriteTable1.y)
    ld ($C0C6), a
    ld a, (RAM_SpriteTable1.xn)
    ld ($C0C7), a
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
    ld a, CursorTile_ArrowBottomRight
    call SetCursorIndex
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_Beep), a
    ld a, ($C089)
    set 1, a
    ld ($C089), a
    ld a, (RAM_SpriteTable1.y)
    ld ($C203), a
    add a, $07
    ld ($C0C4), a
    add a, $08
    ld (RAM_SpriteTable1.y), a
    ld a, (RAM_SpriteTable1.xn)
    ld ($C246), a
    add a, $07
    ld ($C0C5), a
    add a, $08
    ld (RAM_SpriteTable1.xn), a
    ld a, $A9
    ld ($C247), a
    xor a
    ld ($C15D), a
    ld a, SetCursorIndex_Second | CursorTile_ArrowBottomRight
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
      ld a, ($C0BA)
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
    ld a, $01
    ld (RAM_Beep), a
    ld a, (RAM_SpriteTable1.y)
    add a, (hl)
    ld ($C16F), a
    inc hl
    ld a, (RAM_SpriteTable1.xn)
    add a, (hl)
    ld ($C170), a
    ld a, ($C089)
    or $01
    ld ($C089), a
    ret

; Data from 357F to 358C (14 bytes)
Data_357F: ; $357F
.db $10 $40 $2B $CC $06 $07 $04 $1B $9C $20 $70 $07 $03 $08

; 12th entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
Mode12_DisplaySpritesHandler:
      call CheckMenuButton
    exx
    ld a, ($C089)
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
    ld ($C0C4), a
    add a, $21
    ld ($C0C6), a
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
    ld ($C0C5), a
    add a, $21
    ld ($C0C7), a
    ld a, CursorTile_ArrowBottomRight
    call SetCursorIndex
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_Beep), a
    ld a, ($C089)
    or $04
    ld ($C089), a
    ld a, (RAM_SpriteTable1.y)
    ld ($C203), a
    add a, $08
    ld (RAM_SpriteTable1.y), a
    ld a, (RAM_SpriteTable1.xn)
    ld ($C246), a
    add a, $08
    ld (RAM_SpriteTable1.xn), a
    ld a, $A9
    ld ($C247), a
    xor a
    ld ($C15D), a
    ld a, SetCursorIndex_Second | CursorTile_ArrowBottomRight
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
    ld b, $00
    ld hl, $365E
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
    ld a, CursorTile_ZoomedPixel
    jp SetCursorIndex ; and ret

+:  ld a, ($C089)
    or $80
    ld ($C089), a
    ret

; Data from 365E to 3665 (8 bytes)
.db $00 $00 $50 $00 $00 $70 $50 $70

; 15th entry of Jump Table from 2FD0 (indexed by RAM_CurrentMode)
Mode15_ColourSelectionMenuPlusSpritesHandler:
      call CheckMenuButton
    exx
    ld a, $58
    ld (RAM_SpriteTable1.xn), a
    ld a, b
    and $F8
    cp $40
    jp nc, +
    ld a, $40
+:  cp $48
    jp c, +
    ld a, $48
+:  ld (RAM_SpriteTable1.y), a
    ld b, a
    ld a, CursorTile_MenuArrowRight
    call SetCursorIndex
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_Beep), a
    ld a, b
    sub $40
    jp z, +
    ld a, $01
+:  ld ($C0BA), a
    ld a, (RAM_CurrentMode)
    set 6, a
    ld (RAM_CurrentMode), a
    ret

_LABEL_36A5_:
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
    ld ($C08A), a
    ld a, $01
    ld (RAM_Beep), a
    jp ++

+++:ld a, $01
    ld (RAM_Beep), a
    ld a, ($C062)
    xor $01
    ld ($C062), a
    jp ++

+:  bit 2, (hl)
    jp z, ++
    ld ($C06C), a
    ld a, b
    ld ($C242), a
    ld a, $01
    ld (RAM_Beep), a
    ld a, ($C08A)
    cp $03
    jp nz, ++
    xor a
    ld ($C08A), a
++: ld a, $A8
    ld ($C241), a
    ld a, CursorTile_PaletteSelect
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
    COLOUR 0,0,0 ; Black
    COLOUR 3,0,0 ; Red
    COLOUR 0,3,0 ; Green
    COLOUR 3,3,0 ; Yellow
    COLOUR 0,0,3 ; Blue
    COLOUR 3,3,3 ; White
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
    ld a, ($C08A) ; Desired new pen mode
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

; 1st entry of Jump Table from 388C (indexed by $C08A)
DrawThinPenOn:
    set 0, (ix+PenMode.IsSet)
    LD_DE_TILE $19d
    LD_HL_PEN_TILE_GRAPHICS PenTile_Thin_On
    jp FillTiles2bpp

; 2nd entry of Jump Table from 388C (indexed by $C08A)
DrawMediumPenOn:
    set 0, (ix+PenMode.IsSet)
    LD_DE_TILE $19e
    LD_HL_PEN_TILE_GRAPHICS PenTile_Medium_On
    jp FillTiles2bpp

; 3rd entry of Jump Table from 388C (indexed by $C08A)
DrawThickPenOn:
    set 0, (ix+PenMode.IsSet)
    LD_DE_TILE $19f
    LD_HL_PEN_TILE_GRAPHICS PenTile_Thick_On
    jp FillTiles2bpp

; 4th entry of Jump Table from 388C (indexed by $C08A)
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

_LABEL_3932_:
    ld a, ($C089)
    and $07
    ret z
    cp $07
    jp nc, _LABEL_39C0_
    ld a, ($C0C4)
    ld b, a
    ld a, ($C0C6)
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
    ld a, ($C0C5)
    ld b, a
    ld a, ($C0C7)
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
    ld a, ($C0C5)
    ld c, a
    ld a, ($C0C2)
    ld b, a
    ld a, ($C0C4)
    call _LABEL_3A83_
    exx
      ld a, ($C0C4)
      ld b, a
      ld a, ($C0BE)
      add a, b
    exx
    call _LABEL_3A83_
    ld a, ($C0C4)
    ld c, a
    ld a, ($C0BF)
    ld b, a
    ld a, ($C0C5)
    call _LABEL_3A35_
    exx
      ld a, ($C0C5)
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
-:  ld a, (hl)
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

; Data from 39F8 to 3A23 (44 bytes)
.db $C5 $78 $3D $4F $06 $00 $21 $CA $C0 $09 $EB $C5 $E1 $29 $01 $FA
.db $C0 $09 $23 $EB $C1 $7E $DD $77 $00 $DD $23 $2B $1A $FD $77 $01
.db $1B $1A $FD $77 $00 $FD $23 $FD $23 $1B $10 $E9

+:  ld a, c
    or a
    jp z, +
-:  ld (ix+0), $E0
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

_LABEL_3ACE_:
    ld b, $0C
    ld a, ($C0BA)
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
        ld h,0
        ld l,a ; Convert to address of letter in font
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

