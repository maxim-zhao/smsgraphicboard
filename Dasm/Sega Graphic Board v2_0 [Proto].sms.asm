.MEMORYMAP
SLOTSIZE $4000
SLOT 0 $0000
SLOT 1 $4000
SLOT 2 $8000
DEFAULTSLOT 2
.ENDME
.ROMBANKMAP
BANKSTOTAL 2
BANKSIZE $4000
BANKS 2
.ENDRO

.define BypassDetection

.emptyfill $ff

.define Port_IOPortControl $3f
.define Port_PSG           $7f
.define Port_VDPData       $be
.define Port_VDPAddress    $bf
.define Port_VDPStatus     $bf
.define Port_IOPort1       $dc
.define Port_IOPort2       $dd

; Macros for Port_IOPortControl
; These can be ORed together, best to specify four each time
; It's presumably irrelevant what the high nibble is when the bit is set to IN...
; Bits:
; D7 : Port 2 TH pin output level (1=high, 0=low)
; D6 : Port 2 TR pin output level (1=high, 0=low)
; D5 : Port 1 TH pin output level (1=high, 0=low)
; D4 : Port 1 TR pin output level (1=high, 0=low)
; D3 : Port 2 TH pin direction (1=input, 0=output)
; D2 : Port 2 TR pin direction (1=input, 0=output)
; D1 : Port 1 TH pin direction (1=input, 0=output)
; D0 : Port 1 TR pin direction (1=input, 0=output)
.define IO_TR1_OUT_1 %00010000
.define IO_TR1_OUT_0 %00000000
.define IO_TR1_IN    %00010001
.define IO_TH1_OUT_1 %00100000
.define IO_TH1_OUT_0 %00000000
.define IO_TH1_IN    %00100010
.define IO_TR2_OUT_1 %01000000
.define IO_TR2_OUT_0 %00000000
.define IO_TR2_IN    %01000100
.define IO_TH2_OUT_1 %10000000
.define IO_TH2_OUT_0 %00000000
.define IO_TH2_IN    %10001000

; RAM
.define RAM_ResetButton1 $C000 ; 1b Currently pressed value
.define RAM_ResetButton2 $C001 ; 1b Positive edge signal
.define RAM_VDPReg1Value $C003 ; 1b
.define RAM_VRAMFillHighByte $C004 ; 1b
;---
.define RAM_VBlankFunctionControl $C007 ; 1b - bit 1 set means read the graphics board in the VBlank
.define RAM_SpriteTable2DirtyFlag $C008 ; 1b - non-zero if sprite table should be copied to VRAM in VBlank
.define RAM_PSGIsActive  $C009 ;  1b ???
;---
.define RAM_ButtonsPressed $C02C ; 1b: buttons pressed last time we looked
.define RAM_ButtonsNewlyPressed $C02D ; 1b: buttons pressed last time we looked which were'nt pressed in the previous frame
.define RAM_PenY_Smoothed $C02E ; 1b: average of itself and the last raw value
.define RAM_PenX_Smoothed $C02F ; 1b: average of itself and the last raw value
.define RAM_PenX $C030 ; 1b
;---
.define RAM_PenY $C033 ; 1b
.define RAM_Pressure $C034 ; 1b - never used
.define RAM_NonVBlankDynamicFunction $C03C ; 2b
;---
.define RAM_Palette      $C042 ; 17b
;---
.define RAM_SplashScreenTimeout $C163 ; 2b
;---
.define RAM_SpriteTable1 $C200 ; 192b - write here
.define RAM_SpriteTable1_Y RAM_SpriteTable1
.define RAM_SpriteTable1_XN RAM_SpriteTable1+64
.define RAM_SpriteTable2 $C2C0 ; 192b - copy here for staging to VRAM?
.define RAM_SpriteTable2_Y RAM_SpriteTable2
.define RAM_SpriteTable2_XN RAM_SpriteTable2+64

.BANK 0 SLOT 0
.ORG $0000
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
    ld e, a
    ld d, $00
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
.db $06 $A0 $FF $FF $FF $FF $FF $00 $00 $00 $00

ScreenOff:
    ld a, (RAM_VDPReg1Value)
    and %10111111 ; unset bit 6 = screen off
    jp +

ScreenOn:
    ld a, (RAM_VDPReg1Value)
    or %01000000 ; set bit 6 = screen on
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
    ld (hl), $00
    ldir

Start_AfterRAMClear:
    ld sp, $DFFE
    call SilencePSG
    call DelayLoop1
    call InitialiseVDPRegisters
    call FillNameTableWithTile9

    ld a, IO_TR1_IN | IO_TH1_IN | IO_TR2_IN | IO_TH2_IN; $FF ; all inputs
    out (Port_IOPortControl), a

    ei
    ld a, $01
    call SetVBlankFunctionAndWait
    
    di
    call TitleScreen
    
    ; Zero tiles
    ld de, $4000
    ld bc, $3800
    ld h, $00
    call FillVRAMWithH

    ; Set up screen

    ld hl, ControlTiles ; $4552 ; data: tiles for palette, UI controls
    ld de, $71A0 ; Tile $18d
    call DecompressGraphics

    ld de, $7660 ; Tile $1b3 
    ld b, $0D ; 26 tiles?
    ld hl, $41B2 ; 2bpp tile data - all colour 0
    call Write2bppToVRAM

    ld de, $7800 ; Tilemap
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
    ld de, $C000 ; palette
    ld bc, 32
    call RawDataToVRAM
    ; Blank ???
    ld hl, $C15D
    ld de, $C15E
    ld bc, 8
    ld (hl), $00
    ldir
    ; Paging unnecessary, we are 32KB
    ld a, $02
    ld ($FFFF), a
    ; Write a bunch of zeroes in there..?
    ld hl, $8000
    ld de, $8001
    ld bc, $3FFF
    ld (hl), $00
    ldir
    ; Then another 16KB
    ld a, $03
    ld ($FFFF), a
    ld hl, $8000
    ld de, $8001
    ld bc, $3FFF
    ld (hl), $00
    ldir
    ; Store a pointer?
    ld hl, $8000
    ld ($C183), hl
    ld ($C187), hl
    ld a, $01
    ld ($C182), a
    call InitialiseCursorSprites
    call ScreenOn
    ld hl, $4858
    ld ($C03E), hl
    ld a, $01
    ld ($C00B), a
    ; Main loop
-:  ei
    ld a, $03
    call SetVBlankFunctionAndWait
    call _LABEL_2F92_
    call _LABEL_164F_
    call SpriteTable1to2
    jp -

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
    
    ld de, $4000 ; start of VRAM
    ld h, $00
    ld bc, $4000 ; all of VRAM
    call FillVRAMWithH
    jp DisableSprites_RAMAndVRAM

FillNameTableWithTile9:
    ld de, $7800 ; Name table address
    ld bc, $0380 ; Entry count
    ld hl, $0009 ; Date to write
    ; fall through
    
FillVRAMWithHL:
    rst $08 ; VDPAddressToDE
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
    ld hl, RAM_SpriteTable1_Y
    ld de, RAM_SpriteTable1_Y+1
    ld bc, 64-1
    ld (hl), 224
    ldir
    ld hl, RAM_SpriteTable2_Y
    ld de, RAM_SpriteTable2_Y+1
    ld bc, 64-1
    ld (hl), 224
    ldir
DisableSprites_VRAM:
    ld de, $7F00 ; Sprite table Y addresses
    ld h, 224
    ld bc, 64
    ; fall through

FillVRAMWithH:
    ; Write h to VRAM address de, bc times
    rst $08 ; VDPAddressToDE
-:  ld a, h
    out (Port_VDPData), a
    dec bc
    ld a, b
    or c
    jp nz, -
    ret

RawDataToVRAM:
    ; write bc bytes from hl to VRAM address de
    rst $08 ; VDPAddressToDE
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
    rst $08 ; VDPAddressToDE
    ld c, Port_VDPData
-:  outi
    push af
      ld a, (RAM_VRAMFillHighByte)
      out (c), a
    pop af
    jp nz, -
    ret

; Data from 1D3 to 1F3 (33 bytes)
.db $32 $05 $C0 $CF $7E $D9 $0E $BE $06 $04 $67 $3A $05 $C0 $1F $54
.db $38 $02 $16 $00 $ED $51 $10 $F6 $D9 $23 $0B $78 $B1 $C2 $D7 $01
.db $C9

Write2bppToVRAM:
    ; write data from hl to VRAM address de, 2 bytes then 2 zeroes, b*16 times
    rst $08 ; VDPAddressToDE
Write2bppToVRAMCurrentAddress:
    ; write data from hl to VDP, 2 bytes then 2 zeroes, b*16 times
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
    rst $08 ; VDPAddressToDE
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
      rst $08 ; VDPAddressToDE
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
      rst $08 ; VDPAddressToDE
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

_LABEL_263_:
    ; read b*c (?) bytes from VRAM address de to hl
    rst $08 ; VDPAddressToDE
    push af ; delay
    pop af
--: push bc
      ld b, c
      ld c, Port_VDPData
-:    ini ; read a byte to hl
      push af ; delay
      pop af
      jr nz, -
    pop bc
    ld a, b
    or a
    ret z
    djnz --
    ret

_LABEL_277_:
--: push bc
      ld b, c
      push de
-:      rst $08 ; VDPAddressToDE
        ex (sp), hl ; delay
        ex (sp), hl
        in a, (Port_VDPData)
        ex af, af' ; save value
        ex (sp), hl ; delay
        ex (sp), hl
        ld a, e
        out (Port_VDPAddress), a
        ld a, d
        or $40
        out (Port_VDPAddress), a
        ex af, af' ; restore value
        and $E1
        or h
        push af
        pop af
        out (Port_VDPData), a
        inc de
        inc de
        djnz -
      pop de
    push hl
      ld hl, $0040
      add hl, de
      ex de, hl
    pop hl
    pop bc
    djnz --
    ret

; Data from 2A2 to 2D4 (51 bytes)
.db $C5 $E5 $69 $26 $00 $29 $29 $29 $44 $4D $E1 $CD $BB $02 $E5 $21
.db $C0 $02 $19 $EB $E1 $C1 $10 $E8 $C9 $CF $E5 $C5 $06 $04 $CB $0C
.db $7D $DA $C7 $02 $AF $D3 $BE $00 $10 $F4 $C1 $E1 $0B $78 $B1 $C2
.db $BC $02 $C9

DecompressGraphics:
    ld b, $04 ; bitplane count
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
-:  rst $08 ; VDPAddressToDE
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

_LABEL_304_:
    ld a, (RAM_SpriteTable2DirtyFlag)
    or a
    ret z
    xor a
    ld (RAM_SpriteTable2DirtyFlag), a
    ld a, ($C006)
    ld de, $7F00 ; Sprite table: Y
    rst $08 ; VDPAddressToDE
    ld hl, RAM_SpriteTable2_Y
    ld c, Port_VDPData
    call Outi64
    ld hl, RAM_SpriteTable2_XN
    ld de, $7F80 ; Sprite table: XN
    rst $08 ; VDPAddressToDE
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
    ld a, (RAM_NonVBlankDynamicFunction)
    and $3F
    cp $0C ; ???
    ret z
    ld a, ($C006)
    rrca ; Check low bit
    jp c, +
    ; Even: straight copy
    ld hl, RAM_SpriteTable1_Y
    ld de, RAM_SpriteTable2_Y
    ld bc, 64
    ldir
    ld hl, RAM_SpriteTable1_XN
    ld de, RAM_SpriteTable2_XN
    ld bc, 64*2
    ldir
    ld a, 1
    ld (RAM_SpriteTable2DirtyFlag), a
    ret

+:  ; Odd: reverse order
    ld hl, RAM_SpriteTable1_Y + 63
    ld de, RAM_SpriteTable2_Y
    ld b, 64
-:  ld a, (hl)
    ld (de), a
    dec hl
    inc de
    djnz -
    ld hl, RAM_SpriteTable1_XN + 63 * 2
    ld de, RAM_SpriteTable2_XN
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
    ld hl, $0573 ; tilemap data: MENU | DO | PEN bar
    ld de, $7D48 ; 4, 21
    ld bc, $0330 ; 24x3 tiles
    call WriteAreaToTilemap
    ld hl, TopBarPaletteTiles
    ld de, $784A ; 5, 1
    ld bc, $002C ; count
    call RawDataToVRAM
    ld hl, TopBarStatusTiles ; data: top bar status
    ld de, $78AC ; 22, 2
    ld bc, $000A ; count
    jp RawDataToVRAM ; and ret

SetDrawingAreaTilemap:
    ld hl, $0000 ; Tilemap data to write
    ld bc, $1216 ; 12 rows, 16 columns
    ld de, $78CA ; tile $1c6
--: rst $08 ; VDPAddressToDE
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

_LABEL_4CD_:
    ld a, (RAM_PSGIsActive)
    or a
    ret z
    inc a
    ld (RAM_PSGIsActive), a
    cp $05
    jp z, SilencePSG
    ld a, $8F
    out (Port_PSG), a
    ld a, $03
    out (Port_PSG), a
    ld a, $90
    out (Port_PSG), a
    ret

SilencePSG:
    ld a, $9F
    out (Port_PSG), a
    ld a, $BF
    out (Port_PSG), a
    ld a, $DF
    out (Port_PSG), a
    ld a, $FF
    out (Port_PSG), a
    cpl
    ld (RAM_PSGIsActive), a
    ret

; Data from 4FD to 602 (262 bytes)
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
.db $30 $00 $3F $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00

.org $051d
DrawingPalette:
.db $3F $00 $01 $02 $03 $04 $08 $0C $10 $20 $30 $38 $07 $0F $1F $2F
.db $00 $00 $3F $03 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $03

.org $053d
TopBarPaletteTiles:
.dw $018d, $018e, $018f, $0190, $0191, $0192, $0193, $0194, $0195, $0196, $0197, $0198, $0199, $019a, $019b, $019c
.dw $098D, $099D, $099E, $099F, $09A0, $09A1

TopBarStatusTiles:
.dw $09A4, $09A4, $09A4, $09A4, $09A4, $09A2, $0DA4, $0DA4, $0DA4, $0DA4

.dw $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4
.dw $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4, $0DA4
.dw $0DA4, $0DA4, $0BA2, $0BA3, $09AA, $09AB, $09AC, $09AD
.dw $09AE, $09AF, $09B0, $09B1, $09B2, $09B3, $09B4, $09B5
.dw $09B6, $09B7, $09B8, $09B9, $09BA, $09BB, $09BC, $09BD
.dw $09BE, $09BF, $09A3, $0DA2, $09A4, $09A4, $09A4, $09A4
.dw $09A4, $09A4, $09A4, $09A4, $09A4, $09A4, $09A4, $09A4
.dw $09A4, $09A4, $09A4, $09A4, $09A4, $09A4, $09A4, $09A4
.dw $09A4, $09A4, $0FA2

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
        jp nz, _LABEL_6CD_
        rrca
        jp nc, +
        call _LABEL_304_
        call _LABEL_376B_
        call _LABEL_371E_
        ld a, (RAM_NonVBlankDynamicFunction)
        cp $83
        jp nz, +
        ld a, ($C054)
        or a
        jp z, +
        xor a
        ld ($C054), a

        ld hl, RAM_Palette
        ld de, $C000 ; tile palette index 0
        rst $08 ; VDPAddressToDE
        ld b, $11 ; 17 palette entries
-:      ld a, (hl)
        inc hl
        push af
        pop af
        out (Port_VDPData), a
        djnz -

        ld de, $C014 ; sprite palette index 4
        rst $08 ; VDPAddressToDE
        ld a, ($C053) ; value to write
        ld b, 8       ; 8 palette entries
-:      out (Port_VDPData), a
        inc a ; ???
        push af
        pop af
        djnz -

        call _LABEL_1F0F_

+:      ld hl, $C006
        inc (hl)
        call _LABEL_3B2E_
        ld a, (RAM_VBlankFunctionControl)
        bit 1, a
        jp z, +
        push af
            call ReadGraphicsBoard
        pop af
+:      bit 0, a
        jp z, +
        call _LABEL_37EE_
        call _LABEL_386B_
+:      call _LABEL_4CD_
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
    ld (hl), $00
    ldir
    ; Then go to the usual startup code
    jp Start_AfterRAMClear

_LABEL_6CD_:
    ld a, (RAM_VBlankFunctionControl)
    or a
    jp z, VBlank_CheckResetAndExit
    push af
        call _LABEL_B0A_
    pop af
    bit 1, a
    push af
        call nz, ReadGraphicsBoard
    pop af
    bit 2, a
    call nz, _LABEL_9BC_
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

ReadGraphicsBoard:
    ; Main graphics board read

    in a, (Port_IOPort1)
    bit 4, a    ; Check for data
    jp nz, NoBoardData ; not pressed -> skip
    
    ; no delay: 10us from in to out

    ; ============================================================= Read 1 (TH = 1)

    ld a, IO_TR1_OUT_0 | IO_TH1_OUT_1 | IO_TR2_OUT_0 | IO_TH2_OUT_0 ; $20 = all output, all zero except P1 TH = 1
    out (Port_IOPortControl), a

    ld b,16
-:  djnz - ; delay: 238 cycles = 66us from out to in

    ; Read the buttons and calculate what's new since last time
    ld a, (RAM_ButtonsPressed)
    ld b, a
    in a, (Port_IOPort1) 
    cpl
    ld c, a
    ld (RAM_ButtonsPressed), a
    xor b
    and c
    ld (RAM_ButtonsNewlyPressed), a

    ld b, $5C
-:  djnz - ; delay: 1266 cyles = 354us from in to out
    nop
    nop

    ; ============================================================= Read 2 (TH = 0, 1)
    
    ld a, IO_TR1_OUT_0 | IO_TH1_OUT_0 | IO_TR2_OUT_0 | IO_TH2_OUT_0 ; all output, all zero
    out (Port_IOPortControl), a

    ld b, $28
-:  djnz - ; delay: 533 cycles = 149us from out to in

    ; read low 4 bits
    in a, (Port_IOPort1)
    and $0F
    rlca
    rlca
    rlca
    rlca
    ld d, a
    
    ; no delay: 45 cycles = 13us from in to out

    ld a, IO_TR1_OUT_0 | IO_TH1_OUT_1 | IO_TR2_OUT_0 | IO_TH2_OUT_0
    out (Port_IOPortControl), a

    ld b, $28
-:  djnz - ; delay: 533 cycles = 149us from out to in

    in a, (Port_IOPort1)
    and $0F
    or d
    cp $FD
    jp c, PressureTooLow
    ld (RAM_Pressure), a

    ld b, $0B
-:  djnz - ; delay: 204 cycles = 57 cycles from in to out

    ; ============================================================= Read 3 (TH = 0, 1)

    ld a, IO_TR1_OUT_0 | IO_TH1_OUT_0 | IO_TR2_OUT_0 | IO_TH2_OUT_0
    out (Port_IOPortControl), a

    ld b, $28
-:  djnz - ; delay: 533 cycles = 149us from out to in

    in a, (Port_IOPort1)
    and $0F
    rlca
    rlca
    rlca
    rlca
    ld h, a
    
    ; no delay: 45 cycles = 13us from in to out
    
    ld a, IO_TR1_OUT_0 | IO_TH1_OUT_1 | IO_TR2_OUT_0 | IO_TH2_OUT_0
    out (Port_IOPortControl), a

    ld b, $28
-:  djnz - ; delay: 533 cycles = 149us from out to in

    in a, (Port_IOPort1)
    and $0F
    or h
    ld (RAM_PenX), a
    nop

    ld b, $0E
-:  djnz - ; delay: 230 cycles = 64us from in to out

    ; ============================================================= Read 4 (TH = 0, 1)

    ld a, IO_TR1_OUT_0 | IO_TH1_OUT_0 | IO_TR2_OUT_0 | IO_TH2_OUT_0
    out (Port_IOPortControl), a

    ld b, $28
-:  djnz - ; delay: 533 cycles = 149us from out to in

    in a, (Port_IOPort1)
    and $0F
    rlca
    rlca
    rlca
    rlca
    ld d, a
    
    ; no delay: 45 cycles = 13us from in to out
     
    ld a, IO_TR1_OUT_0 | IO_TH1_OUT_1 | IO_TR2_OUT_0 | IO_TH2_OUT_0
    out (Port_IOPortControl), a

    ld b, $28
-:  djnz - ; delay: 533 cycles = 149us from out to in

    in a, (Port_IOPort1)
    and $0F
    or d
    ld (RAM_PenY), a

    ; ============================================================= Done: set TR, TH

    ; no delay: 42 cycles = 12us from in to out
     
    ld a, IO_TR1_OUT_1 | IO_TH1_OUT_1 | IO_TR2_OUT_0 | IO_TH2_OUT_0 ; $30
    out (Port_IOPortControl), a

    ld a, (RAM_PenX)
    ld c, a
    ld a, (RAM_PenX_Smoothed)
    or a
    jp z, +

    ; RAM_PenX_Smoothed = (RAM_PenX_Smoothed + RAM_PenX) / 2
    ld b, $00
    ld h, $00
    ld l, a
    add hl, bc ; add together
    ld e, $02
    call DivMod168 ; very slow way to do this
    ld (RAM_PenX_Smoothed), a
    jp ++

+:  ld a, c
    ld (RAM_PenX_Smoothed), a

++: ld a, (RAM_PenY)
    ld c, a
    ld a, (RAM_PenY_Smoothed)
    or a
    jp z, +

    ; RAM_PenY_Smoothed = (RAM_PenY_Smoothed + RAM_PenY) / 2
    ld b, $00
    ld h, $00
    ld l, a
    add hl, bc
    ld e, 2
    call DivMod168 ; very slow way to do this
    ld (RAM_PenY_Smoothed), a
    ret

+:  ld a, c
    ld (RAM_PenY_Smoothed), a
    ret

PressureTooLow:
    ld a, IO_TR1_OUT_1 | IO_TH1_OUT_1 | IO_TR2_OUT_0 | IO_TH2_OUT_0 ; $30
    out (Port_IOPortControl), a
    ret

    ; Board doesn't want to give us data
    ; Note that this is rather duplicated from above, it looks like it 
    ; could be optimised a bit
NoBoardData:
    ; no delay: 36 cycles = 10us from in to out

    ; Set TH
    ld a, IO_TR1_OUT_0 | IO_TH1_OUT_1 | IO_TR2_OUT_0 | IO_TH2_OUT_0
    out (Port_IOPortControl), a

    ld b, $10
-:  djnz - ; delay: 238 cycles = 66 cycles from out to in

    ; Read the buttons and calculate what's new since last time
    ld a, (RAM_ButtonsPressed)
    ld b, a
    in a, (Port_IOPort1)
    cpl
    ld c, a
    ld (RAM_ButtonsPressed), a
    xor b
    and c
    ld (RAM_ButtonsNewlyPressed), a

    ; no delay: 60 cycles = 17us from in to out

    ld a, IO_TR1_OUT_1 | IO_TH1_OUT_1 | IO_TR2_OUT_0 | IO_TH2_OUT_0
    out (Port_IOPortControl), a
    ret

_LABEL_806_:
    ld b, $11
    xor a
    jp _LABEL_815_

-:  adc a, a
    jr c, +
    cp e
    jr c, ++
+:  sub e
    or a
++: ccf
_LABEL_815_:
    adc hl, hl
    djnz -
    ret

DivMod168:
    ; hl = 16-bit number
    ; e = 8-bit number
    ; returns 
    ; a = hl / e
    ; e = hl % e
    ld a, e
    or a
    ret z ; 
    ld b, $08
    xor a
-:  adc hl, hl
    ld a, h
    jp c, +
    cp e
    jp c, ++
+:  sub e
    ld h, a
    xor a
++: ccf
    djnz -
    rl l
    ld a, l
    ld e, a
    ret

; Data from 835 to 846 (18 bytes)
.db $2E $00 $67 $48 $06 $00 $3E $08 $29 $D2 $42 $08 $09 $3D $C2 $3D
.db $08 $C9

_LABEL_847_:
    ld h, l
    ld b, $08
    ld d, $00
    ld l, d
-:  add hl, hl
    jr nc, +
    add hl, de
+:  djnz -
    ret

_LABEL_854_:
    ld hl, $0000
    or a
    ret z
    ld b, $08
-:
    add hl, hl
    adc a, a
    jr nc, +
    add hl, de
    adc a, $00
+:  djnz -
    ret

TitleScreen: ; $865
    ; blank RAM for ???
    ld hl, $C15D
    ld de, $C15E
    ld bc, 8
    ld (hl), $00
    ldir

    ; Initialise timeout counter
    ld hl, $0200 ; 8533ms
    ld (RAM_SplashScreenTimeout), hl

    ld hl, FontTiles ; $3C02 ; compressed tile data: font
    ld de, $4000 ; tile 0
    call DecompressGraphics

    ld h, $00
    ld de, $6000 ; tile 256
    ld bc, $1800 ; 192 tiles (up to name table)
    call FillVRAMWithH

    ld hl, Tiles_SegaLogo ; $14CA ; compressed tile date: Sega logo
    ld de, $7200 ; tile 400
    call DecompressGraphics
    
    ld hl, Palette_TitleScreen
    ld de, $C000 ; Tile palette
    ld bc, $0007
    call RawDataToVRAM
    
    ld hl, Palette_Logo ; $0C2A
    ld de, $C010 ; Sprite palette
    ld bc, $0008
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
    
TitleScreen_PostAnimationLoop:
    ld hl, $14A2 ; data: Sega logo tilemap data
    ld de, $78D6 ; 11, 3
    ld bc, $040A ; 10x4
    ld a, $01
    ld (RAM_VRAMFillHighByte), a
    call WriteAreaToTilemap_1byte
    
    ld hl, Text_CopyrightSega1987 ; $0A08 ; data: (c) Sega 1987
    ld de, $7D94 ; 10, 22
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
    ld ($C010), hl
    ld de, $7C0C ; 6, 16
    ld ($C012), de
    ld bc, $0C14 ; area?
    ld ($C014), bc
    ld a, $84
    call SetVBlankFunctionAndWait

    ; decrement timeout counter
    ld hl, (RAM_SplashScreenTimeout)
    dec hl
    ld (RAM_SplashScreenTimeout), hl
    ld a, l
    or h
    jp z, TitleScreenTimedOut
    jp CheckForGraphicsBoard

GraphicsBoardDetected:
    ld hl, Text_PushButton ; $09FC ; Data: "PUSH  BUTTON"
    ld ($C010), hl
    ld de, $7C14 ; 10, 16
    ld ($C012), de
    ld bc, $200C ; area?
    ld ($C014), bc
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
    ld hl, (RAM_SplashScreenTimeout)
    dec hl
    ld (RAM_SplashScreenTimeout), hl
    ld a, l
    or h
    jp z, TitleScreenTimedOut
    
    ; check button presses from last read
    ld a, (RAM_ButtonsNewlyPressed)
    and $07
    jp z, - ; loop until pressed
    
    ld a, $01
    ld (RAM_PSGIsActive), a
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
    ld de, $6020 ; Tile $101
    ld b, 135 ; $87 ; 135 tiles
    call Write2bppToVRAMSlowly
    ld de, $7A00 ; 0, 8
    ld hl, Tilemap_Logo ; $0B8A
    ld bc, $0520 ; 5 rows, 32 columns
    ld a, $09
    ld (RAM_VRAMFillHighByte), a
    call WriteAreaToTilemap_1byte
    jp TitleScreen_PostAnimationLoop

_LABEL_9BC_:
    ld hl, $C00E
    dec (hl)
    ret p
    ld a, ($C015)
    ld (hl), a
    inc hl
    ld a, (hl)
    xor $01
    ld (hl), a
    jp z, +
    ld de, ($C012)
    ld a, ($C014)
    ld b, a
    ld hl, ($C010)
    xor a
    jp RawDataToVRAM_Interleaved1

+:  ; Blank the text area
    ld de, $7C0C ; tilemap 6, 16 
    ld bc, 20 ; 20 tiles
    ld hl, 9
    jp FillVRAMWithHL ; and ret

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

.org $0a14
Palette_TitleScreen:
.db $10 $00 $3F $00 $30 $00 $3F

TitleScreenAnimate_Bit0Zero:
    set 7, (ix+1) ; set high bit of $c15e
    inc (ix+2) ; increment $c15f
    dec (ix+3) ; decrement $c160
    ret p      ; if it's reached -1:
    set 0, (ix+1) ; set low bit of $c15e
    ret

_LABEL_A2B_:
    ld a, (ix+2)
    push hl
        ld c, a
        ld a, $1F
        sub c
        add a, a
        ld l, a
        ld h, $00
        add hl, de
        ex de, hl
    pop hl
    ld b, (ix+2)
    inc b
    jp RawDataToVRAM_Interleaved2

_LABEL_A41_:
    ld c, (ix+2)
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
    and $F8
    ; multiply by 108
    ld l, a     ; x1
    ld h, $00
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
    and $07
    add a, a ; x2
    add a, a ; x4
    ld e, a
    ld d, $00
    add hl, de ; x8
    ld de, $6020 ; magic? Tile 257
    add hl, de
    push hl
        ld a, c ; high 5 bits of c
        and $F8
        ; Multiply by 54
        ld l, a
        ld h, $00
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
        ld e, a
        ld d, $00
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
    ld l, a
    ld h, $00
    add hl, hl
    add hl, hl
    push hl
      add hl, hl
      push hl
        add hl, hl
        add hl, hl
        push hl
          add hl, hl
        pop de
        add hl, de
      pop de
      add hl, de
    pop de
    add hl, de
    ld a, b
    and $07
    add a, a
    add a, a
    ld e, a
    ld d, $00
    add hl, de
    ld de, $6020
    add hl, de
    ld de, $0020
    ld b, $1B
    ld c, $BE
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

_LABEL_B0A_:
    ld a, ($C15E) ; check for high bit = new write
    bit 7, a
    ret z
    and %01111111 ; clear high bit
    ld ($C15E), a
    bit 0, a
    jp z, +
    bit 1, a
    jp z, _LABEL_B52_
    ret

+:  ld a, $09
    ld (RAM_VRAMFillHighByte), a
    ld hl, Tilemap_Logo + 32 * 0 ; $0B8A
    ld de, $7A00        ; 0, 8
    call _LABEL_A2B_
    ld hl, Tilemap_Logo + 32 * 1 ; $0BAA
    ld de, $7A40        ; 0, 9
    call _LABEL_A41_
    ld hl, Tilemap_Logo + 32 * 2 ; $0BCA
    ld de, $7A80        ; 0, 10
    call _LABEL_A2B_
    ld hl, Tilemap_Logo + 32 * 3 ; $0BEA
    ld de, $7AC0        ; 0, 11
    call _LABEL_A41_
    ld hl, Tilemap_Logo + 32 * 4 ; $0C0A
    ld de, $7B00        ; 0, 12
    jp _LABEL_A2B_

_LABEL_B52_:
    ld c, $23
    ld b, (ix+4)
_LABEL_B57_:
    push bc
        call UpdateSplashScreenAnimationTilesLine
    pop bc
    dec c
    dec b
    jp p, _LABEL_B57_
    ld a, (ix+4)
    cp $11
    jp nc, _LABEL_B89_
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
_LABEL_B89_:
    ret

;.orga $b8a
Tilemap_Logo:
.incbin "Logo tilemap.bin"
;.orga $c2a
Palette_Logo:
.db $10 $3f $15 $2a $00 $00 $00 $00
;.orga $c32
Tiles_Logo:
.incbin "Logo tiles.2bpp"
;.orga $14a2
Tilemap_SegaLogo:
.db $90 $91 $92 $93 $94 $95 $96 $97 $98 $99 
.db $9a $9b $9c $9d $9e $9f $a0 $a1 $a2 $a3 
.db $a4 $a5 $a6 $a7 $a8 $a9 $aa $ab $ac $ad 
.db $ae $af $b0 $ae $b1 $b2 $ae $ae $ae $b3
;.orga $14ca
Tiles_SegaLogo:
.incbin "Sega logo.pscompr"

_LABEL_164F_:
    ld hl, RAM_NonVBlankDynamicFunction
    ld a, (hl)
    and $3F
    exx
    ld hl, $165C
    jp JumpToFunction

; Jump Table from 165C to 167F (18 entries, indexed by RAM_NonVBlankDynamicFunction)
.dw _LABEL_1C4A_ _LABEL_1680_ _LABEL_16C0_ _LABEL_1EE2_ _LABEL_171E_ _LABEL_1F66_ _LABEL_21A2_ _LABEL_21A2_
.dw _LABEL_2605_ _LABEL_2862_ _LABEL_290D_ _LABEL_2C5A_ _LABEL_1740_ _LABEL_18C8_ _LABEL_1790_ _LABEL_17DE_
.dw _LABEL_182C_ _LABEL_187A_

; 2nd entry of Jump Table from 165C (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_1680_:
    exx
    bit 7, (hl)
    ret nz
    set 7, (hl)
    inc hl
    ld (hl), $00
    di
    xor a
    ld ($C089), a
    inc a
    ld ($C00A), a
    call _LABEL_3B13_
    call SetDrawingAreaTilemap
    ld bc, $0E0C
    ld de, $1A10
    ld hl, $0405
    call _LABEL_1902_
    ld hl, (RAM_PenY_Smoothed)
    ld ($C08D), hl
    ld hl, ($C031)
    ld ($C08F), hl
    ld hl, ($C03E)
    ld (RAM_PenY_Smoothed), hl
    ld ($C031), hl
    ld a, $01
    ld (RAM_PSGIsActive), a
    ei
    ret

; 3rd entry of Jump Table from 165C (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_16C0_:
    di
    call SetDrawingAreaTilemap
    ld hl, (RAM_PenY_Smoothed)
    ld a, $48
    cp l
    jp c, _LABEL_16D0_
    ld hl, $4858
_LABEL_16D0_:
    ld ($C03E), hl
    call _LABEL_198B_
    ld hl, ($C08D)
    ld (RAM_PenY_Smoothed), hl
    ld hl, ($C08F)
    ld ($C031), hl
    exx
    inc hl
    ld a, (hl)
    ld (hl), $00
    cp $03
    jp nz, _LABEL_16F2_
    ld (hl), a
    ld a, $0F
    jp _LABEL_1715_

_LABEL_16F2_:
    cp $04
    jp nz, _LABEL_16FD_
    ld (hl), a
    ld a, $11
    jp _LABEL_1715_

_LABEL_16FD_:
    cp $05
    jp c, _LABEL_170D_
    cp $08
    jp nc, _LABEL_170D_
    ld (hl), a
    ld a, $0E
    jp _LABEL_1715_

_LABEL_170D_:
    cp $0A
    jp nz, _LABEL_1715_
    ld (hl), a
    ld a, $10
_LABEL_1715_:
    dec hl
    ld (hl), a
    ld a, $01
    ld (RAM_PSGIsActive), a
    ei
    ret

; 5th entry of Jump Table from 165C (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_171E_:
    ld a, ($C0BA)
    or a
    jp z, +

    ld a, (RAM_PSGIsActive)
    or a
    ret nz

    di
    ld de, $4000 ; Tiles
    ld h, $00
    ld bc, 396 * 32 ; $3180 ; 396 tiles
    call FillVRAMWithH
    ei
    ; fall through
+:  exx
    ld (hl), $00
    ld a, $01
    ld ($C00A), a
    ret

; 13th entry of Jump Table from 165C (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_1740_:
    ld a, (RAM_PSGIsActive)
    or a
    ret nz
    exx
    bit 7, (hl)
    jp z, _LABEL_1768_
    ld a, (RAM_ButtonsNewlyPressed)
    bit 0, a
    ret z
    di
    call DrawUIControls
    ld hl, ($C08D)
    ld (RAM_PenY_Smoothed), hl
    ld hl, ($C08F)
    ld ($C031), hl
    ld a, $01
    ld (RAM_NonVBlankDynamicFunction), a
    ei
    ret

_LABEL_1768_:
    set 7, (hl)
    ld hl, (RAM_PenY_Smoothed)
    ld ($C08D), hl
    ld hl, ($C031)
    ld ($C08F), hl
    di
    call ScreenOff
    call DisableSprites_VRAM
    ld de, $7800
    ld hl, $8D09
    ld bc, $0380
    call FillVRAMWithHL
    call SetDrawingAreaTilemap
    ei
    jp ScreenOn

; 15th entry of Jump Table from 165C (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_1790_:
    ld a, (RAM_PSGIsActive)
    or a
    ret nz
    exx
    bit 7, (hl)
    jp z, _LABEL_17B8_
    bit 6, (hl)
    ret z
    di
    exx
    call _LABEL_198B_
    ld hl, ($C08D)
    ld (RAM_PenY_Smoothed), hl
    ld hl, ($C08F)
    ld ($C031), hl
    exx
    inc hl
    ld a, (hl)
    ld (hl), $00
    dec hl
    ld (hl), a
    ei
    ret

_LABEL_17B8_:
    set 7, (hl)
    di
    ld bc, $040A
    ld de, $1AC7
    ld hl, $0405
    call _LABEL_1902_
    ld hl, (RAM_PenY_Smoothed)
    ld ($C08D), hl
    ld hl, ($C031)
    ld ($C08F), hl
    ld hl, $4858
    ld (RAM_PenY_Smoothed), hl
    ld ($C031), hl
    ei
    ret

; 16th entry of Jump Table from 165C (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_17DE_:
    ld a, (RAM_PSGIsActive)
    or a
    ret nz
    exx
    bit 7, (hl)
    jp z, _LABEL_1806_
    bit 6, (hl)
    ret z
    di
    exx
    call _LABEL_198B_
    ld hl, ($C08D)
    ld (RAM_PenY_Smoothed), hl
    ld hl, ($C08F)
    ld ($C031), hl
    exx
    inc hl
    ld a, (hl)
    ld (hl), $00
    dec hl
    ld (hl), a
    ei
    ret

_LABEL_1806_:
    set 7, (hl)
    di
    ld bc, $0410
    ld de, $1AF4
    ld hl, $0405
    call _LABEL_1902_
    ld hl, (RAM_PenY_Smoothed)
    ld ($C08D), hl
    ld hl, ($C031)
    ld ($C08F), hl
    ld hl, $4858
    ld (RAM_PenY_Smoothed), hl
    ld ($C031), hl
    ei
    ret

; 17th entry of Jump Table from 165C (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_182C_:
    ld a, (RAM_PSGIsActive)
    or a
    ret nz
    exx
    bit 7, (hl)
    jp z, _LABEL_1854_
    bit 6, (hl)
    ret z
    di
    exx
    call _LABEL_198B_
    ld hl, ($C08D)
    ld (RAM_PenY_Smoothed), hl
    ld hl, ($C08F)
    ld ($C031), hl
    exx
    inc hl
    ld a, (hl)
    ld (hl), $00
    dec hl
    ld (hl), a
    ei
    ret

_LABEL_1854_:
    set 7, (hl)
    di
    ld bc, $040E
    ld de, $1B39
    ld hl, $0405
    call _LABEL_1902_
    ld hl, (RAM_PenY_Smoothed)
    ld ($C08D), hl
    ld hl, ($C031)
    ld ($C08F), hl
    ld hl, $4858
    ld (RAM_PenY_Smoothed), hl
    ld ($C031), hl
    ei
    ret

; 18th entry of Jump Table from 165C (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_187A_:
    ld a, (RAM_PSGIsActive)
    or a
    ret nz
    exx
    bit 7, (hl)
    jp z, _LABEL_18A2_
    bit 6, (hl)
    ret z
    di
    exx
    call _LABEL_198B_
    ld hl, ($C08D)
    ld (RAM_PenY_Smoothed), hl
    ld hl, ($C08F)
    ld ($C031), hl
    exx
    inc hl
    ld a, (hl)
    ld (hl), $00
    dec hl
    ld (hl), a
    ei
    ret

_LABEL_18A2_:
    set 7, (hl)
    di
    ld bc, $040D
    ld de, $1C11
    ld hl, $0405
    call _LABEL_1902_
    ld hl, (RAM_PenY_Smoothed)
    ld ($C08D), hl
    ld hl, ($C031)
    ld ($C08F), hl
    ld hl, $4858
    ld (RAM_PenY_Smoothed), hl
    ld ($C031), hl
    ei
    ret

; 14th entry of Jump Table from 165C (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_18C8_:
    exx
    bit 7, (hl)
    jp z, _LABEL_18D9_
    ld hl, RAM_SplashScreenTimeout
    dec (hl)
    ret p
    call ScreenOff
    jp FullReset

_LABEL_18D9_:
    set 7, (hl)
    ld a, $80
    ld (RAM_SplashScreenTimeout), a
    ld a, $E0
    ld (RAM_SpriteTable1_Y), a
    ret

_LABEL_18E6_:
    ld a, (RAM_ButtonsNewlyPressed)
    bit 0, a
    ret z
    ld a, (RAM_PSGIsActive)
    or a
    ret nz
    ld a, (RAM_NonVBlankDynamicFunction)
    and $3F
    cp $01
    ret z
    cp $02
    ret z
    ld a, $01
    ld (RAM_NonVBlankDynamicFunction), a
    ret

_LABEL_1902_:
    ld a, ($C082)
    or a
    call nz, _LABEL_1981_
    ld a, $80
    ld ($C082), a
    ld ($C01A), bc
    push de
      push hl
        push hl
          ld a, h
          add a, $03
          ld l, a
          ld h, $00
          add hl, hl
          add hl, hl
          add hl, hl
          add hl, hl
          add hl, hl
          add hl, hl
          ex de, hl
        pop hl
        ld a, l
        add a, $05
        add a, a
        ld h, $00
        ld l, a
        inc l
        add hl, de
        ld de, $3800
        add hl, de
        ld ($C018), hl
        ex de, hl
        ld h, $08
        call _LABEL_277_
      pop hl
      push hl
        ld a, h
        ld de, $02C0
        call _LABEL_854_
        ex de, hl
      pop hl
      ld h, $00
      add hl, hl
      add hl, hl
      add hl, hl
      add hl, hl
      add hl, hl
      add hl, de
      ex de, hl
      set 6, d
    pop hl
    ld ($C016), de
    call _LABEL_19D3_
    ld b, (hl)
    inc hl
    rst $08 ; VDPAddressToDE
-:  ld a, (hl)
    cp $FF
    inc hl
    jp z, _LABEL_1976_
    exx
    ld de, $41B2
    ld l, a
    ld h, $00
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, de
    ld b, $01
    call Write2bppToVRAMCurrentAddress
    exx
_LABEL_1973_:
    djnz -
    ret

_LABEL_1976_:
    push hl
      ld hl, $02C0
      add hl, de
      ex de, hl
    pop hl
    rst $08 ; VDPAddressToDE
    jp _LABEL_1973_

_LABEL_1981_:
    push bc
    push de
    push hl
      call _LABEL_198B_
    pop hl
    pop de
    pop bc
    ret

_LABEL_198B_:
    xor a
    ld ($C082), a
    ld de, ($C018)
    ld bc, ($C01A)
    ld h, $00
    call _LABEL_277_
    push bc
    push de
    push hl
      ld de, ($C016)
      ld bc, ($C01A)
      ld l, c
      ld h, $00
      add hl, hl
      add hl, hl
      add hl, hl
      add hl, hl
      add hl, hl
      ld ($C0BB), hl
      ld hl, $C400
-:    push bc
        ld bc, ($C0BB)
        rst $08 ; VDPAddressToDE
_LABEL_19BB_:
        ld a, (hl)
        out (Port_VDPData), a
        inc hl
        dec bc
        ld a, b
        or c
        jp nz, _LABEL_19BB_
        push hl
          ld hl, $02C0
          add hl, de
          ex de, hl
        pop hl
      pop bc
      djnz -
    pop hl
    pop de
    pop bc
    ret

_LABEL_19D3_:
    push bc
    push de
    push hl
      ld de, ($C016)
      res 6, d
      ld bc, ($C01A)
      ld l, c
      ld h, $00
      add hl, hl
      add hl, hl
      add hl, hl
      add hl, hl
      add hl, hl
      ld ($C0BB), hl
      ld hl, $C400
-:    push bc
        ld bc, ($C0BB)
        rst $08 ; VDPAddressToDE
        push af
        pop af
_LABEL_19F6_:
        push af
        pop af
        in a, (Port_VDPData)
        ld (hl), a
        inc hl
        dec bc
        ld a, b
        or c
        jp nz, _LABEL_19F6_
        push hl
          ld hl, $02C0
          add hl, de
          ex de, hl
        pop hl
      pop bc
      djnz -
    pop hl
    pop de
    pop bc
    ret

; Data from 1A10 to 1C49 (570 bytes)
.db $B6 $1F $20 $20 $00 $0D $05 $0E $15 $00 $20 $20 $21 $FF $26 $00
.db $00 $05 $18 $09 $14 $00 $00 $00 $00 $22 $FF $26 $00 $00 $03 $0F
.db $0C $0F $12 $00 $00 $00 $22 $FF $26 $00 $00 $05 $12 $01 $13 $05
.db $00 $00 $00 $22 $FF $26 $00 $00 $13 $11 $15 $01 $12 $05 $00 $00
.db $22 $FF $26 $00 $00 $03 $09 $12 $03 $0C $05 $00 $00 $22 $FF $26
.db $00 $00 $05 $0C $0C $09 $10 $13 $05 $00 $22 $FF $26 $00 $00 $10
.db $01 $09 $0E $14 $00 $00 $00 $22 $FF $26 $00 $00 $03 $0F $10 $19
.db $00 $00 $00 $00 $22 $FF $26 $00 $00 $0D $09 $12 $12 $0F $12 $00
.db $00 $22 $FF $26 $00 $00 $0D $01 $07 $0E $09 $06 $19 $00 $22 $FF
.db $26 $00 $00 $04 $09 $13 $10 $0C $01 $19 $00 $22 $FF $26 $00 $00
.db $05 $0E $04 $00 $00 $00 $00 $00 $22 $FF $25 $24 $24 $24 $24 $24
.db $24 $24 $24 $24 $24 $23 $FF $2C $1F $20 $00 $0D $0F $04 $05 $00
.db $20 $21 $FF $26 $00 $00 $0C $09 $0E $05 $00 $00 $22 $FF $26 $00
.db $00 $10 $01 $09 $0E $14 $00 $22 $FF $25 $24 $24 $24 $24 $24 $24
.db $24 $24 $23 $FF $44 $1F $20 $00 $03 $0F $0C $0F $12 $00 $0D $05
.db $0E $15 $00 $20 $21 $FF $26 $00 $00 $03 $0F $0C $0F $12 $00 $13
.db $05 $14 $00 $00 $00 $22 $FF $26 $00 $00 $02 $01 $03 $0B $00 $03
.db $0F $0C $0F $12 $00 $00 $22 $FF $25 $24 $24 $24 $24 $24 $24 $24
.db $24 $24 $24 $24 $24 $24 $24 $23 $FF $3C $1F $20 $00 $0D $0F $04
.db $05 $00 $13 $05 $14 $00 $20 $21 $FF $26 $00 $00 $16 $1E $12 $05
.db $16 $05 $12 $13 $05 $00 $22 $FF $26 $00 $00 $08 $1E $12 $05 $16
.db $05 $12 $13 $05 $00 $22 $FF $25 $24 $24 $24 $24 $24 $24 $24 $24
.db $24 $24 $24 $24 $23 $FF $70 $1F $20 $20 $00 $03 $0F $0C $0F $12
.db $00 $20 $20 $21 $FF $26 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
.db $00 $22 $FF $26 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $22
.db $FF $26 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $22 $FF $26
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $22 $FF $26 $00 $00
.db $10 $01 $07 $05 $00 $15 $10 $00 $00 $22 $FF $26 $00 $00 $10 $01
.db $07 $05 $00 $04 $0F $17 $0E $22 $FF $25 $24 $24 $24 $24 $24 $24
.db $24 $24 $24 $24 $24 $23 $FF $91 $09 $8E $09 $92 $09 $8E $09 $93
.db $09 $8E $09 $94 $09 $8E $09 $8E $09 $8E $09 $8E $09 $8E $09 $8E
.db $09 $8E $09 $95 $09 $8E $09 $96 $09 $8E $09 $97 $09 $8E $09 $98
.db $09 $38 $1F $20 $00 $05 $12 $01 $13 $05 $00 $1D $00 $20 $21 $FF
.db $26 $00 $00 $0E $0F $00 $00 $00 $00 $00 $00 $00 $22 $FF $26 $00
.db $00 $19 $05 $13 $00 $00 $00 $00 $00 $00 $22 $FF $25 $24 $24 $24
.db $24 $24 $24 $24 $24 $24 $24 $24 $23 $FF

; 1st entry of Jump Table from 165C (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_1C4A_:
    exx
    ld a, (RAM_ButtonsNewlyPressed)
    and $03
    ret nz
    di
    ld a, ($C062)
    or a
    jp nz, _LABEL_1C7F_
    ld a, (RAM_ButtonsPressed)
    ld ($C06D), a
    bit 2, a
    ld a, ($C08A)
    ld ($C06B), a
    ld hl, ($C031)
    ld de, (RAM_PenY_Smoothed)
    call nz, _LABEL_1CA1_
    ld a, (RAM_PenX_Smoothed)
    ld ($C032), a
    ld a, (RAM_PenY_Smoothed)
    ld ($C031), a
    ei
    ret

_LABEL_1C7F_:
    di
    ld a, (RAM_ButtonsPressed)
    ld ($C06D), a
    ld hl, (RAM_PenY_Smoothed)
    exx
    ld a, ($C08A)
    ld ($C06B), a
    call _LABEL_1D40_
    ld a, (RAM_PenX_Smoothed)
    ld ($C032), a
    ld a, (RAM_PenY_Smoothed)
    ld ($C031), a
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
    jp c, _LABEL_1CD9_
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

_LABEL_1CD9_:
    ld h, e
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
    jp c, _LABEL_1D17_
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

_LABEL_1D17_:
    ld h, e
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
    exx
    bit 0, c
    jp nz, +
    inc h
    jp ++
+:  dec h
++: exx
    ret

; Data from 1D3F to 1D3F (1 bytes)
.db $C9

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
    jp z, _LABEL_1D85_
    cp $01
    jp z, _LABEL_1D94_
    cp $02
    jp z, _LABEL_1DD0_
    cp $03
    jp z, _LABEL_1D94_
    ret

_LABEL_1D85_:
    ld a, ($C063)
    ld ($C064), a
    ld a, ($C067)
    ld ($C068), a
    jp _LABEL_1E57_

_LABEL_1D94_:
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
    ld a, ($C067)
    ld ($C068), a
    call _LABEL_1E57_
    ld a, ($C065)
    ld ($C064), a
    ld a, ($C069)
    ld ($C068), a
    jp _LABEL_1E57_

_LABEL_1DD0_:
    ld a, ($C066)
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
    jp _LABEL_1E57_

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
      pop de
      add hl, de
    pop de
    add hl, de
    ex de, hl
    ld a, c
    and $F8
    ld l, a
    ld h, $00
    add hl, hl
    add hl, hl
    add hl, de
    ld a, b
    and $07
    add a, a
    add a, a
    ld e, a
    ld d, $00
    add hl, de
    ex de, hl
    push bc
      ld hl, $C073
      ld bc, $0004
      call _LABEL_263_
    pop bc
    ld a, c
    and $07
    push de
      ld hl, $1EDA
      ld e, a
      ld d, $00
      add hl, de
      ld a, (hl)
      ld hl, $C073
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
    or $40
    ld d, a
    ld hl, $C073
    ld bc, $0004
    jp RawDataToVRAM

; Data from 1EDA to 1EE1 (8 bytes)
.db $80 $40 $20 $10 $08 $04 $02 $01

; 4th entry of Jump Table from 165C (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_1EE2_:
    exx
    bit 7, (hl)
    jp z, + ; could ret nz
    ret
+:  set 7, (hl)
    di
    ld de, $1B76
    ld bc, $080D
    ld hl, $0405
    call _LABEL_1902_
    ld hl, $1BE7
    ld de, $7A1C
    ld bc, $030E
    call WriteAreaToTilemap
    xor a
    ld ($C053), a
    ld a, $01
    ld ($C054), a
    ei
    ret

_LABEL_1F0F_:
    ld a, ($C053)
    and $3C
    rrca
    rrca
    ld de, $4E80
    call +
    ld de, $5400
    inc a
    and $0F
+:  push af
      ld hl, $1F46
      add a, a
      ld c, a
      ld b, $00
      add hl, bc
      rst $08 ; VDPAddressToDE
      ld a, (hl)
      call +
      inc hl
      ld a, (hl)
      call +
    pop af
    ret

+:  push hl
      ld c, a
      ld b, $00
      ld hl, $44A2
      add hl, bc
      ld b, $01
      call Write2bppToVRAMCurrentAddress
    pop hl
    ret

; Data from 1F46 to 1F65 (32 bytes)
.db $00 $20 $00 $30 $00 $40 $00 $50 $00 $60 $00 $70 $00 $80 $00 $90
.db $00 $A0 $20 $10 $20 $20 $20 $30 $20 $40 $20 $50 $20 $60 $20 $70

; 6th entry of Jump Table from 165C (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_1F66_:
    ld a, ($C089)
    cp $03
    ret nz
    ld a, (RAM_PSGIsActive)
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
+:  ld a, b
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
+:  ld b, a
    ld a, (iy+15)
    sub (iy+13)
    cp $08
    jp c, _LABEL_2085_
    ld a, $24
    cp (iy+13)
    jp c, +
    cp (iy+15)
    jp nc, _LABEL_2079_
+:  ld a, $D4
    cp (iy+13)
    jp nc, +
    cp (iy+15)
    jp c, _LABEL_2079_
+:  ld a, (iy+13)
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
+:  ld a, (iy+12)
    sub $3C
    jp nc, +
    xor a
+:  ld (iy+12), a
    ld c, a
    ld a, (iy+14)
    sub $3C
    sub c
    ld b, a
-:  push bc
      ld a, (iy+12)
      and $F8
      ld l, a
      ld h, $00
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
        ld l, a
        ld h, $00
        add hl, hl
        add hl, hl
      pop de
      add hl, de
      ld a, (iy+12)
      and $07
      add a, a
      add a, a
      ld e, a
      ld d, $00
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
-:
    push bc
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
    jp z, _LABEL_20CD_
    push de
      ld hl, $219A
      ld e, a
      ld d, $00
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
_LABEL_20CD_:
    ld a, (iy+15)
    and $F8
    sub c
    jp c, _LABEL_20EA_
    and $F8
    rrca
    rrca
    rrca
    or a
    jp z, _LABEL_20EA_
    ld b, a
-:  call _LABEL_20FE_
    ld hl, $0020
    add hl, de
    ex de, hl
    djnz -
_LABEL_20EA_:
    ld a, (iy+15)
    and $07
    ret z
    push de
    ld hl, $219A
    ld e, a
    ld d, $00
    add hl, de
    pop de
    ld a, (hl)
    cpl
    jp _LABEL_213E_

_LABEL_20FE_:
    ld a, e
    out (Port_VDPAddress), a
    ld a, d
    or $40
    out (Port_VDPAddress), a
    ld hl, $00FF
    ld c, $BE
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
    ld hl, $C073
    push de
      push af
        ld bc, $0004
        call _LABEL_263_
      pop af
      cpl
      ld b, a
      ld hl, $C073
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
    or $40
    out (Port_VDPAddress), a
    ld a, b
    cpl
    ld hl, $C073
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
.db $FF $7F $3F $1F $0F $07 $03 $01

; 7th entry of Jump Table from 165C (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_21A2_:
    ld a, (RAM_PSGIsActive)
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
      ld d, $00
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
    jp z, _LABEL_2203_
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

_LABEL_2203_:
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
_LABEL_2219_:
    call _LABEL_259B_
    ld hl, ($C0A6)
    inc hl
    or a
    sbc hl, de
    or a
    sbc hl, bc
    jp m, _LABEL_223B_
    add hl, bc
    or a
    sbc hl, de
    ld ($C0A6), hl
    dec de
    ld hl, ($C0A2)
    dec hl
    ld ($C0A2), hl
    jp _LABEL_224A_

_LABEL_223B_:
    add hl, de
    or a
    sbc hl, bc
    ld ($C0A6), hl
    dec bc
    ld hl, ($C0A4)
    dec hl
    ld ($C0A4), hl
_LABEL_224A_:
    ld a, d
    or e
    jp nz, _LABEL_2219_
_LABEL_224F_:
    call _LABEL_259B_
    ld hl, ($C0A6)
    inc hl
    add hl, bc
    or a
    sbc hl, de
    jp p, _LABEL_2271_
    or a
    sbc hl, bc
    or a
    sbc hl, de
    ld ($C0A6), hl
    dec de
    ld hl, ($C0A2)
    dec hl
    ld ($C0A2), hl
    jp _LABEL_227E_

_LABEL_2271_:
    add hl, bc
    add hl, de
    ld ($C0A6), hl
    inc bc
    ld hl, ($C0A4)
    inc hl
    ld ($C0A4), hl
_LABEL_227E_:
    ld a, b
    or c
    jp nz, _LABEL_224F_
_LABEL_2283_:
    call _LABEL_259B_
    ld hl, ($C0A6)
    inc hl
    add hl, bc
    or a
    adc hl, de
    jp m, _LABEL_22A3_
    or a
    sbc hl, bc
    add hl, de
    ld ($C0A6), hl
    inc de
    ld hl, ($C0A2)
    inc hl
    ld ($C0A2), hl
    jp _LABEL_22B2_

_LABEL_22A3_:
    or a
    sbc hl, de
    add hl, bc
    ld ($C0A6), hl
    inc bc
    ld hl, ($C0A4)
    inc hl
    ld ($C0A4), hl
_LABEL_22B2_:
    ld a, d
    or e
    jp nz, _LABEL_2283_
_LABEL_22B7_:
    call _LABEL_259B_
    ld hl, ($C0A6)
    inc hl
    add hl, de
    or a
    sbc hl, bc
    jp p, _LABEL_22D5_
    add hl, bc
    add hl, de
    ld ($C0A6), hl
    inc de
    ld hl, ($C0A2)
    inc hl
    ld ($C0A2), hl
    jp _LABEL_22E6_

_LABEL_22D5_:
    or a
    sbc hl, de
    or a
    sbc hl, bc
    ld ($C0A6), hl
    dec bc
    ld hl, ($C0A4)
    dec hl
    ld ($C0A4), hl
_LABEL_22E6_:
    ld a, b
    or c
    jp nz, _LABEL_22B7_
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
_LABEL_2307_:
    call _LABEL_259B_
_LABEL_230A_:
    ld hl, ($C0A6)
    inc hl
    or a
    sbc hl, de
    or a
    sbc hl, bc
    jp m, _LABEL_2329_
    add hl, bc
    or a
    sbc hl, de
    ld ($C0A6), hl
    dec de
    ld hl, ($C0A2)
    dec hl
    ld ($C0A2), hl
    jp _LABEL_2343_

_LABEL_2329_:
    add hl, de
    or a
    sbc hl, bc
    ld ($C0A6), hl
    dec bc
    ld hl, ($C0A8)
    ld a, h
    sub l
    ld ($C0A9), a
    jp nc, _LABEL_234B_
    ld hl, ($C0A4)
    dec hl
    ld ($C0A4), hl
_LABEL_2343_:
    ld a, d
    or e
    jp nz, _LABEL_2307_
    jp _LABEL_2353_

_LABEL_234B_:
    ld a, d
    or e
    jp nz, _LABEL_230A_
    jp _LABEL_2356_

_LABEL_2353_:
    call _LABEL_259B_
_LABEL_2356_:
    ld hl, ($C0A6)
    inc hl
    add hl, bc
    or a
    sbc hl, de
    jp p, _LABEL_2375_
    or a
    sbc hl, bc
    or a
    sbc hl, de
    ld ($C0A6), hl
    dec de
    ld hl, ($C0A2)
    dec hl
    ld ($C0A2), hl
    jp _LABEL_238D_

_LABEL_2375_:
    add hl, bc
    add hl, de
    ld ($C0A6), hl
    inc bc
    ld hl, ($C0A8)
    ld a, h
    add a, l
    ld ($C0A9), a
    jp nc, _LABEL_2399_
    ld hl, ($C0A4)
    inc hl
    ld ($C0A4), hl
_LABEL_238D_:
    ld a, b
    or c
    jp nz, _LABEL_2353_
    ld hl, $C0A9
    inc (hl)
    jp _LABEL_23A5_

_LABEL_2399_:
    ld a, b
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
    jp m, _LABEL_23C5_
    or a
    sbc hl, bc
    add hl, de
    ld ($C0A6), hl
    inc de
    ld hl, ($C0A2)
    inc hl
    ld ($C0A2), hl
    jp _LABEL_23DF_

_LABEL_23C5_:
    or a
    sbc hl, de
    add hl, bc
    ld ($C0A6), hl
    inc bc
    ld hl, ($C0A8)
    ld a, h
    add a, l
    ld ($C0A9), a
    jp nc, _LABEL_23E7_
    ld hl, ($C0A4)
    inc hl
    ld ($C0A4), hl
_LABEL_23DF_:
    ld a, d
    or e
    jp nz, _LABEL_23A5_
    jp _LABEL_23EF_

_LABEL_23E7_:
    ld a, d
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
    jp p, _LABEL_240D_
    add hl, bc
    add hl, de
    ld ($C0A6), hl
    inc de
    ld hl, ($C0A2)
    inc hl
    ld ($C0A2), hl
    jp _LABEL_2429_

_LABEL_240D_:
    or a
    sbc hl, de
    or a
    sbc hl, bc
    ld ($C0A6), hl
    dec bc
    ld hl, ($C0A8)
    ld a, h
    sub l
    ld ($C0A9), a
    jp nc, _LABEL_242F_
    ld hl, ($C0A4)
    dec hl
    ld ($C0A4), hl
_LABEL_2429_:
    ld a, b
    or c
    jp nz, _LABEL_23EF_
    ret

_LABEL_242F_:
    ld a, b
    or c
    jp nz, _LABEL_23F2_
    ret

_LABEL_2435_:
    push bc
    push de
    ld l, b
    ld de, ($C0A8)
    call _LABEL_847_
    ld a, l
    ld b, h
    add a, $80
    jp nc, _LABEL_2447_
    inc b
_LABEL_2447_:
    pop de
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
_LABEL_2465_:
    call _LABEL_259B_
_LABEL_2468_:
    ld hl, ($C0A6)
    inc hl
    or a
    sbc hl, de
    or a
    sbc hl, bc
    jp m, _LABEL_2492_
    add hl, bc
    or a
    sbc hl, de
    ld ($C0A6), hl
    dec de
    ld hl, ($C0A8)
    ld a, h
    sub l
    ld ($C0A9), a
    jp nc, _LABEL_24AD_
    ld hl, ($C0A2)
    dec hl
    ld ($C0A2), hl
    jp _LABEL_24A1_

_LABEL_2492_:
    add hl, de
    or a
    sbc hl, bc
    ld ($C0A6), hl
    dec bc
    ld hl, ($C0A4)
    dec hl
    ld ($C0A4), hl
_LABEL_24A1_:
    ld a, d
    or e
    jp nz, _LABEL_2465_
    ld hl, $C0A9
    dec (hl)
    jp _LABEL_24B9_

_LABEL_24AD_:
    ld a, d
    or e
    jp nz, _LABEL_2468_
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
    jp p, _LABEL_24E6_
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
    jp nc, _LABEL_24FB_
    ld hl, ($C0A2)
    dec hl
    ld ($C0A2), hl
    jp _LABEL_24F3_

_LABEL_24E6_:
    add hl, bc
    add hl, de
    ld ($C0A6), hl
    inc bc
    ld hl, ($C0A4)
    inc hl
    ld ($C0A4), hl
_LABEL_24F3_:
    ld a, b
    or c
    jp nz, _LABEL_24B9_
    jp _LABEL_2503_

_LABEL_24FB_:
    ld a, b
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
    jp m, _LABEL_252E_
    or a
    sbc hl, bc
    add hl, de
    ld ($C0A6), hl
    inc de
    ld hl, ($C0A8)
    ld a, h
    add a, l
    ld ($C0A9), a
    jp nc, _LABEL_2549_
    ld hl, ($C0A2)
    inc hl
    ld ($C0A2), hl
    jp _LABEL_253D_

_LABEL_252E_:
    or a
    sbc hl, de
    add hl, bc
    ld ($C0A6), hl
    inc bc
    ld hl, ($C0A4)
    inc hl
    ld ($C0A4), hl
_LABEL_253D_:
    ld a, d
    or e
    jp nz, _LABEL_2503_
    ld hl, $C0A9
    inc (hl)
    jp _LABEL_2555_

_LABEL_2549_:
    ld a, d
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
    jp p, _LABEL_257E_
    add hl, bc
    add hl, de
    ld ($C0A6), hl
    inc de
    ld hl, ($C0A8)
    ld a, h
    sub l
    ld ($C0A9), a
    jp nc, _LABEL_2595_
    ld hl, ($C0A2)
    inc hl
    ld ($C0A2), hl
    jp _LABEL_258F_

_LABEL_257E_:
    or a
    sbc hl, de
    or a
    sbc hl, bc
    ld ($C0A6), hl
    dec bc
    ld hl, ($C0A4)
    dec hl
    ld ($C0A4), hl
_LABEL_258F_:
    ld a, b
    or c
    jp nz, _LABEL_2555_
    ret

_LABEL_2595_:
    ld a, b
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
    jp z, _LABEL_25AF_
    xor a
    bit 7, h
    ld h, a
    jp nz, _LABEL_25AE_
    cpl
_LABEL_25AE_:
    ld l, a
_LABEL_25AF_:
    ld de, ($C0A4)
    ld a, d
    or a
    jp z, _LABEL_25C1_
    xor a
    bit 7, d
    ld d, a
    jp nz, _LABEL_25C0_
    cpl
_LABEL_25C0_:
    ld e, a
_LABEL_25C1_:
    ex af, af'
    or a
    jp nz, _LABEL_25D1_
    ex af, af'
    ld a, l
    ld l, e
    ld h, a
    call _LABEL_1D50_
    pop hl
    pop de
    pop bc
    ret

_LABEL_25D1_:
    ex af, af'
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

; 9th entry of Jump Table from 165C (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_2605_:
    ld a, (RAM_PSGIsActive)
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
      jp nc, _LABEL_274D_
      ld a, e
      ld ($C0AB), a
      ld a, d
      ld ($C0AA), a
      call _LABEL_2752_
      or a
      jp nz, _LABEL_274D_
      ld hl, $0000
      ld ($C0AC), hl
_LABEL_2665_:
      ld a, ($C0AB)
      or a
      jr z, _LABEL_267D_
      dec a
      ld e, a
      ld a, ($C0AA)
      ld d, a
      call _LABEL_2752_
      or a
      jr nz, _LABEL_267D_
      ld a, e
      ld ($C0AB), a
      jr _LABEL_2665_

_LABEL_267D_:
      ld a, $01
      ld ($C0B0), a
      ld ($C0B1), a
_LABEL_2685_:
      ld a, ($C0B0)
      ld ($C0AE), a
      ld a, ($C0B1)
      ld ($C0AF), a
      ld a, ($C0AA)
      cp $8F
      ld a, $01
      jr z, _LABEL_26A6_
      ld a, ($C0AB)
      ld e, a
      ld a, ($C0AA)
      inc a
      ld d, a
      call _LABEL_2752_
_LABEL_26A6_:
      ld ($C0B1), a
      ld a, ($C0AA)
      cp $00
      ld a, $01
      jr z, _LABEL_26BE_
      ld a, ($C0AB)
      ld e, a
      ld a, ($C0AA)
      dec a
      ld d, a
      call _LABEL_2752_
_LABEL_26BE_:
      ld ($C0B0), a
      ld a, ($C0AE)
      ld b, a
      ld a, ($C0B0)
      xor $01
      and b
      jr z, _LABEL_26DE_
      ld hl, ($C0AC)
      inc hl
      ld ($C0AC), hl
      ld a, ($C0AB)
      ld l, a
      ld a, ($C0AA)
      dec a
      ld h, a
      push hl
_LABEL_26DE_:
        ld a, ($C0AF)
        ld b, a
        ld a, ($C0B1)
        xor $01
        and b
        jr z, _LABEL_26FB_
        ld hl, ($C0AC)
        inc hl
        ld ($C0AC), hl
        ld a, ($C0AB)
        ld l, a
        ld a, ($C0AA)
        inc a
        ld h, a
        push hl
_LABEL_26FB_:
        ld a, ($C0AB)
        ld e, a
        ld a, ($C0AA)
        ld d, a
        ld a, $01
        call _LABEL_2850_
        ld a, ($C0AB)
        cp $AF
        jr z, _LABEL_2725_
        inc a
        ld e, a
        ld a, ($C0AA)
        ld d, a
        call _LABEL_2752_
        or a
        jr nz, _LABEL_2725_
        ld a, ($C0AB)
        inc a
        ld ($C0AB), a
        jp _LABEL_2685_

_LABEL_2725_:
        ld hl, ($C0AC)
        ld a, h
        or l
        jr z, _LABEL_274D_
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
      jr nz, _LABEL_2725_
      jp _LABEL_2665_

_LABEL_274D_:
      pop hl
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
    and $07
    ld c, a
    ex af, af'
    inc c
    dec c
    jp z, _LABEL_2781_
    dec c
    jp z, _LABEL_2782_
    dec c
    jp z, _LABEL_2783_
    dec c
    jp z, _LABEL_2784_
    dec c
    jp z, _LABEL_2785_
    dec c
    jp z, _LABEL_2786_
    dec c
    jp z, _LABEL_2787_
    jp _LABEL_2788_

_LABEL_2781_:
    rlca
_LABEL_2782_:
    rlca
_LABEL_2783_:
    rlca
_LABEL_2784_:
    rlca
_LABEL_2785_:
    rlca
_LABEL_2786_:
    rlca
_LABEL_2787_:
    rlca
_LABEL_2788_:
    rlca
    pop hl
    pop de
    pop bc
    ld a, $00
    ret nc
    ld a, $01
    ret

_LABEL_2792_:
    push de
    call _LABEL_2812_
    ld a, l
    ld hl, $C073
    push hl
    and $07
    ld de, $1EDA
    ld l, a
    ld h, $00
    add hl, de
    ld d, (hl)
    pop hl
    ld c, $00
    ld a, (hl)
    and d
    jp z, _LABEL_27AF_
    set 0, c
_LABEL_27AF_:
    inc hl
    ld a, (hl)
    and d
    jp z, _LABEL_27B7_
    set 1, c
_LABEL_27B7_:
    inc hl
    ld a, (hl)
    and d
    jp z, _LABEL_27BF_
    set 2, c
_LABEL_27BF_:
    inc hl
    ld a, (hl)
    and d
    jp z, _LABEL_27C7_
    set 3, c
_LABEL_27C7_:
    ld a, c
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
    ld hl, $C073
    push bc
    ld b, $04
    ld a, ($C0B2)
    or a
    jp z, _LABEL_2805_
    ld c, a
    push bc
    push hl
    xor a
_LABEL_27E9_:
    rrc c
    jp c, _LABEL_27EF_
    or (hl)
_LABEL_27EF_:
    inc hl
    djnz _LABEL_27E9_
    pop hl
    pop bc
    cpl
    ld d, a
    ld a, $FF
_LABEL_27F8_:
    rrc c
    jp nc, _LABEL_27FE_
    and (hl)
_LABEL_27FE_:
    inc hl
    djnz _LABEL_27F8_
    and d
    jp _LABEL_2810_

_LABEL_2805_:
    push hl
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
_LABEL_2810_:
    pop bc
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
    ld l, a
    ld h, $00
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
    ld hl, $C073
    ld bc, $0004
    call _LABEL_263_
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

; 10th entry of Jump Table from 165C (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_2862_:
    exx
    ld a, ($C089)
    bit 3, a
    jp z, _LABEL_3932_
    ld a, (RAM_PSGIsActive)
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
    ld hl, $C400
    ld ($C171), hl
    ld a, ($C0C8)
    sub $17
    ld ($C162), a
    ld a, ($C0C9)
    sub $28
    ld (RAM_SplashScreenTimeout), a
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
_LABEL_28D0_:
    push bc
    push de
    push hl
    ld b, c
_LABEL_28D4_:
    ld a, e
    and $07
    ld c, a
    ld a, l
    and $07
    sub c
    jp nc, _LABEL_28E1_
    add a, $08
_LABEL_28E1_:
    ld ($C166), a
    push bc
    call _LABEL_2AB1_
    call _LABEL_2ADC_
    pop bc
    inc e
    inc l
    ld a, l
    cp $B0
    jp nc, _LABEL_28F6_
    djnz _LABEL_28D4_
_LABEL_28F6_:
    pop hl
    pop de
    pop bc
    inc d
    inc h
    ld a, h
    cp $90
    jp nc, _LABEL_2903_
    djnz _LABEL_28D0_
_LABEL_2903_:
    ei
    ld a, ($C089)
    and $06
    ld ($C089), a
    ret

; 11th entry of Jump Table from 165C (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_290D_:
    exx
    xor a
    ld a, ($C089)
    rra
    jp nc, _LABEL_3ACE_
    rra
    ret nc
    rra
    jp nc, _LABEL_3932_
    ld a, (RAM_PSGIsActive)
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
    ld hl, $C400
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
    jp nc, _LABEL_2A40_
    add a, (ix+4)
    cp (ix+19)
    jp nc, _LABEL_2A33_
    ld a, ($C15F)
    add a, (ix+4)
    sub (ix+19)
    neg
    add a, (ix+19)
    ld (RAM_SplashScreenTimeout), a
    ret

_LABEL_2A33_:
    ld a, ($C170)
    ld (RAM_SplashScreenTimeout), a
    sub (ix+2)
    ld ($C161), a
    ret

_LABEL_2A40_:
    ld a, ($C15F)
    add a, (ix+4)
    sub (ix+19)
    ld b, a
    ld a, ($C170)
    sub b
    ld (RAM_SplashScreenTimeout), a
    ret

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
    ld hl, $2BD0
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
    rst $08 ; VDPAddressToDE
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
    ld hl, $2BD0
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
    jp z, _LABEL_2B48_
    ld b, a
_LABEL_2B39_:
    push hl
    rrc (hl)
    inc hl
    rrc (hl)
    inc hl
    rrc (hl)
    inc hl
    rrc (hl)
    pop hl
    djnz _LABEL_2B39_
_LABEL_2B48_:
    pop bc
    push hl
    pop iy
    pop hl
    ld a, e
    out (Port_VDPAddress), a
    ld a, d
    or $40
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
    ld l, a
    ld h, $00
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
    ld l, a
    ld h, $00
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
    ld l, a
    ld h, $00
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
    ld l, a
    ld h, $00
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
.db $80 $40 $20 $10 $08 $04 $02 $01

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
    jp z, _LABEL_2BFA_
    inc b
_LABEL_2BFA_:
    ld a, ($C0C5)
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
    jp z, _LABEL_2C15_
    inc c
_LABEL_2C15_:
    ld a, (ix+1)
    and $F8
    ld d, a
    ld e, (ix+2)
    call _LABEL_2B74_
    ld hl, ($C171)
_LABEL_2C24_:
    rst $08 ; VDPAddressToDE
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
_LABEL_2C3D_:
    in a, (Port_VDPData)
    ld (hl), a
    inc hl
    push af
    pop af
    dec bc
    ld a, b
    or c
    jp nz, _LABEL_2C3D_
    ld hl, $02C0
    add hl, de
    ex de, hl
    pop hl
    ld bc, $01A0
    add hl, bc
    pop bc
    djnz _LABEL_2C24_
    pop bc
    pop de
    pop hl
    ret

; 12th entry of Jump Table from 165C (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_2C5A_:
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
    ld a, (RAM_SpriteTable1_Y)
    add a, $04
    cp (iy+1)
    ret c
    cp (iy+2)
    ret nc
    sub $18
    ld l, a
    ld a, (RAM_SpriteTable1_XN)
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
    jp z, _LABEL_2CAC_
    ld a, $01
_LABEL_2CAC_:
    ld ($C06B), a
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
    jp nz, _LABEL_2CE9_
    ld b, $00
_LABEL_2CE9_:
    ld a, b
    ld ($C06C), a
    call _LABEL_1D50_
    pop af
    ld ($C06C), a
    ei
    ret

_LABEL_2CF6_:
    di
    push hl
    call _LABEL_3B13_
    call _LABEL_198B_
    pop hl
    ei
    ld a, $01
    ld (RAM_PSGIsActive), a
_LABEL_2D05_:
    set 7, (hl)
    ld a, $02
    ld ($C089), a
    ret

_LABEL_2D0D_:
    ex af, af'
    ld a, (RAM_PSGIsActive)
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
    ld e, (hl)
    inc hl
    ld d, (hl)
    inc hl
    ld a, (hl)
    inc hl
    ld h, (hl)
    ld l, a
    ld bc, $0909
    call _LABEL_1902_
    pop de
    ld h, $00
    ld bc, $0808
    call _LABEL_277_
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
_LABEL_2DA2_:
    push bc
    push de
    push hl
    ld b, c
_LABEL_2DA6_:
    push bc
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
    jp nc, _LABEL_2DDD_
    djnz _LABEL_2DA6_
_LABEL_2DDD_:
    pop hl
    pop de
    pop bc
    inc d
    inc h
    inc h
    ld a, h
    cp $90
    jp nc, _LABEL_2DEB_
    djnz _LABEL_2DA2_
_LABEL_2DEB_:
    ei
    ret

_LABEL_2DED_:
    ld a, e
    and $07
    ld c, a
    ld a, l
    and $07
    sub c
    jp nc, _LABEL_2DFA_
    add a, $08
_LABEL_2DFA_:
    ld ($C166), a
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

_LABEL_2F92_:
    ld hl, RAM_ButtonsNewlyPressed
    ld a, (RAM_PenY_Smoothed)
    ld b, a
    ld a, (RAM_NonVBlankDynamicFunction)
    and $3F
    cp $0C
    ret z
    cp $09
    jp c, _LABEL_2FB0_
    cp $0B
    jp c, _LABEL_2FB6_
    cp $0D
    jp z, _LABEL_2FB6_
_LABEL_2FB0_:
    ld a, b
    cp $2F
    jp c, _LABEL_36A5_
_LABEL_2FB6_:
    ld a, b
    sub $28
    jp nc, _LABEL_2FBD_
    xor a
_LABEL_2FBD_:
    ld b, a
    ld a, $A8
    ld ($C241), a
    ld a, (RAM_NonVBlankDynamicFunction)
    and $3F
    exx
    ld hl, $2FD0
    jp JumpToFunction

; 3rd entry of Jump Table from 2FD0 (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_2FCF_:
    ret

; Jump Table from 2FD0 to 2FF3 (18 entries, indexed by RAM_NonVBlankDynamicFunction)
.dw _LABEL_2FF4_ _LABEL_3006_ _LABEL_2FCF_ _LABEL_3044_ _LABEL_2FCF_ _LABEL_30F7_ _LABEL_31B6_ _LABEL_31AF_
.dw _LABEL_3264_ _LABEL_3294_ _LABEL_33D1_ _LABEL_358D_ _LABEL_2FCF_ _LABEL_2FCF_ _LABEL_3666_ _LABEL_3666_
.dw _LABEL_3666_ _LABEL_3666_

; 1st entry of Jump Table from 2FD0 (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_2FF4_:
    call _LABEL_18E6_
    exx
    ld a, (RAM_PenX_Smoothed)
    ld (RAM_SpriteTable1_XN), a
    ld a, b
    ld (RAM_SpriteTable1_Y), a
    xor a
    jp _LABEL_37C7_

; 2nd entry of Jump Table from 2FD0 (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_3006_:
    exx
    ld a, $58
    ld (RAM_SpriteTable1_XN), a
    ld a, b
    and $F8
    cp $40
    jp nc, _LABEL_3016_
    ld a, $40
_LABEL_3016_:
    cp $98
    jp c, _LABEL_301D_
    ld a, $98
_LABEL_301D_:
    ld (RAM_SpriteTable1_Y), a
    sub $40
    bit 1, (hl)
    jp z, _LABEL_303F_
    rrca
    rrca
    rrca
    add a, $02
    cp $02
    ld b, a
    jp z, _LABEL_3037_
    ld ($C03D), a
    ld a, $02
_LABEL_3037_:
    ld (RAM_NonVBlankDynamicFunction), a
    ld a, b
    dec a
    ld ($C00A), a
_LABEL_303F_:
    ld a, $03
    jp _LABEL_37C7_

; 4th entry of Jump Table from 2FD0 (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_3044_:
    call _LABEL_18E6_
    exx
    ld a, b
    and $F0
    cp $58
    jp nc, _LABEL_30B9_
    and $F0
    cp $40
    jr nc, _LABEL_3058_
    ld a, $40
_LABEL_3058_:
    ld b, a
    dec a
    ld (RAM_SpriteTable1_Y), a
    ld a, (RAM_PenX_Smoothed)
    and $F0
    cp $70
    jp nc, _LABEL_3069_
    ld a, $70
_LABEL_3069_:
    cp $A0
    jp c, _LABEL_3070_
    ld a, $A0
_LABEL_3070_:
    ld (RAM_SpriteTable1_XN), a
    ld c, a
    ld a, $02
    call _LABEL_37C7_
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_PSGIsActive), a
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
    ld a, ($C053)
    add a, c
    and $3F
    ld c, a
    ld a, ($C0BA)
    or a
    jp nz, _LABEL_30AF_
    ld a, ($C06C)
    ld e, a
    ld d, $00
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
    jp nc, _LABEL_30C0_
    ld a, $60
_LABEL_30C0_:
    cp $68
    jp c, _LABEL_30C7_
    ld a, $68
_LABEL_30C7_:
    dec a
    ld (RAM_SpriteTable1_Y), a
    ex af, af'
    ld a, $58
    ld (RAM_SpriteTable1_XN), a
    ld a, $03
    call _LABEL_37C7_
    bit 1, (hl)
    ret z
    ex af, af'
    ld b, $04
    cp $60
    jp c, _LABEL_30E3_
    ld b, $FC
_LABEL_30E3_:
    ld a, ($C053)
    add a, b
    and $3F
    ld ($C053), a
    ld a, $01
    ld ($C054), a
    ld a, $01
    ld (RAM_PSGIsActive), a
    ret

; 6th entry of Jump Table from 2FD0 (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_30F7_:
    call _LABEL_18E6_
    exx
    ld a, ($C089)
    bit 1, a
    ret nz
    ld d, a
    ld a, b
    ld (RAM_SpriteTable1_Y), a
    ld a, (RAM_PenX_Smoothed)
    ld (RAM_SpriteTable1_XN), a
    push hl
    bit 0, d
    jp nz, _LABEL_314E_
    ld a, $05
    call _LABEL_37C7_
    pop hl
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_PSGIsActive), a
    ld a, (RAM_SpriteTable1_Y)
    ld ($C203), a
    ld a, (RAM_SpriteTable1_XN)
    ld ($C246), a
    ld a, $A9
    ld ($C247), a
    ld de, (RAM_PenY_Smoothed)
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
    ld a, $25
    jp _LABEL_37C7_

_LABEL_314E_:
    ld a, $04
    call _LABEL_37C7_
    pop hl
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_PSGIsActive), a
    ld hl, ($C06E)
    ld a, ($C246)
    ld b, a
    ld a, (RAM_SpriteTable1_XN)
    sub b
    sub $07
    ld b, a
    add a, h
    ld ($C071), a
    ld a, ($C203)
    ld d, a
    ld a, (RAM_SpriteTable1_Y)
    sub $07
    sub d
    ld d, a
    push af
    add a, l
    ld ($C070), a
    pop af
    ld a, d
    jp nc, _LABEL_3185_
    neg
_LABEL_3185_:
    ld ($C072), a
    ld hl, ($C06E)
    ld de, ($C070)
    ld a, l
    cp e
    jp c, _LABEL_3197_
    ld b, l
    ld l, e
    ld e, b
_LABEL_3197_:
    ld a, h
    cp d
    jp c, _LABEL_319F_
    ld b, h
    ld h, d
    ld d, b
_LABEL_319F_:
    ld ($C06E), hl
    ld ($C070), de
    ld a, ($C089)
    set 1, a
    ld ($C089), a
    ret

; 8th entry of Jump Table from 2FD0 (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_31AF_:
    ld a, $01
    and a
    ex af, af'
    jp _LABEL_31B8_

; 7th entry of Jump Table from 2FD0 (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_31B6_:
    xor a
    ex af, af'
_LABEL_31B8_:
    call _LABEL_18E6_
    ld a, $09
    call _LABEL_37C7_
    ld hl, $C089
    bit 0, (hl)
    jp z, _LABEL_3206_
    bit 1, (hl)
    ret nz
    exx
    ld a, ($C203)
    ld c, a
    ex af, af'
    jp nz, _LABEL_31D5_
    ld b, c
_LABEL_31D5_:
    ex af, af'
    ld a, b
    ld (RAM_SpriteTable1_Y), a
    ld a, (RAM_PenX_Smoothed)
    ld (RAM_SpriteTable1_XN), a
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_PSGIsActive), a
    ld a, ($C246)
    ld b, a
    ld a, (RAM_SpriteTable1_XN)
    sub b
    jp nc, _LABEL_31F5_
    neg
_LABEL_31F5_:
    ld ($C0AC), a
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
    ld (RAM_SpriteTable1_Y), a
    ld a, (RAM_PenX_Smoothed)
    ld (RAM_SpriteTable1_XN), a
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_PSGIsActive), a
    ld a, (RAM_SpriteTable1_Y)
    ld ($C203), a
    ld a, (RAM_SpriteTable1_XN)
    ld ($C246), a
    ld a, $A9
    ld ($C247), a
    ld de, (RAM_PenY_Smoothed)
    ld ($C0AA), de
    ld a, $29
    call _LABEL_37C7_
    exx
    set 0, (hl)
    ret

_LABEL_323B_:
    ld a, ($C246)
    ld b, a
    ld a, (RAM_SpriteTable1_XN)
    sub b
    jr nc, _LABEL_3246_
    cpl
_LABEL_3246_:
    ld b, a
    ld a, ($C203)
    ld c, a
    ld a, (RAM_SpriteTable1_Y)
    sub c
    jr nc, _LABEL_3252_
    cpl
_LABEL_3252_:
    ld h, a
    cp b
    jr nc, _LABEL_3257_
    ld a, b
_LABEL_3257_:
    ld ($C0AC), a
    ld e, b
    ld l, $00
    call _LABEL_806_
    ld ($C0A8), hl
    ret

; 9th entry of Jump Table from 2FD0 (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_3264_:
    call _LABEL_18E6_
    ld hl, $C089
    ld a, (hl)
    or a
    ret nz
    exx
    ld a, b
    ld (RAM_SpriteTable1_Y), a
    ld a, (RAM_PenX_Smoothed)
    ld (RAM_SpriteTable1_XN), a
    ld a, $04
    call _LABEL_37C7_
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_PSGIsActive), a
    ld a, (RAM_PenX_Smoothed)
    ld h, a
    ld a, (RAM_PenY_Smoothed)
    ld l, a
    ld ($C091), hl
    exx
    ld (hl), $FF
    ret

; 10th entry of Jump Table from 2FD0 (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_3294_:
    call _LABEL_18E6_
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
    jp nc, _LABEL_32B0_
    ld a, c
_LABEL_32B0_:
    ld c, $98
    cp c
    jp c, _LABEL_32B7_
    ld a, c
_LABEL_32B7_:
    ld (RAM_SpriteTable1_Y), a
    ld a, (RAM_PenX_Smoothed)
    ld c, $21
    cp c
    jp nc, _LABEL_32C4_
    ld a, c
_LABEL_32C4_:
    ld c, $C9
    cp c
    jp c, _LABEL_32CB_
    ld a, c
_LABEL_32CB_:
    ld (RAM_SpriteTable1_XN), a
    ld a, $05
    call _LABEL_37C7_
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_PSGIsActive), a
    ld a, ($C089)
    set 1, a
    ld ($C089), a
    ld a, (RAM_SpriteTable1_Y)
    ld ($C203), a
    add a, $07
    ld ($C0C4), a
    add a, $08
    ld (RAM_SpriteTable1_Y), a
    ld a, (RAM_SpriteTable1_XN)
    ld ($C246), a
    add a, $07
    ld ($C0C5), a
    add a, $08
    ld (RAM_SpriteTable1_XN), a
    ld a, $A9
    ld ($C247), a
    xor a
    ld ($C15D), a
    ld a, $25
    jp _LABEL_37C7_

_LABEL_3311_:
    ld a, ($C0C4)
    add a, $07
    ld c, a
    cp b
    jp c, _LABEL_331C_
    ld b, c
_LABEL_331C_:
    ld a, c
    add a, $58
    jp nc, _LABEL_3324_
    ld a, $FF
_LABEL_3324_:
    ld c, a
    cp b
    jp nc, _LABEL_332A_
    ld b, c
_LABEL_332A_:
    ld a, b
    ld c, $A6
    cp c
    jp c, _LABEL_3332_
    ld a, c
_LABEL_3332_:
    ld (RAM_SpriteTable1_Y), a
    ld ($C0C6), a
    ld a, (RAM_PenX_Smoothed)
    ld b, a
    ld a, ($C0C5)
    add a, $07
    ld c, a
    cp b
    jp c, _LABEL_3347_
    ld b, c
_LABEL_3347_:
    ld a, c
    add a, $58
    jp nc, _LABEL_334F_
    ld a, $FF
_LABEL_334F_:
    ld c, a
    cp b
    jp nc, _LABEL_3355_
    ld b, c
_LABEL_3355_:
    ld a, b
    ld c, $D7
    cp c
    jp c, _LABEL_335D_
    ld a, c
_LABEL_335D_:
    ld (RAM_SpriteTable1_XN), a
    ld ($C0C7), a
    ld a, $04
    call _LABEL_37C7_
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_PSGIsActive), a
    ld a, ($C089)
    set 2, a
    ld ($C089), a
    ld a, (RAM_SpriteTable1_Y)
    ld ($C0C6), a
    ld a, (RAM_SpriteTable1_XN)
    ld ($C0C7), a
    ret

_LABEL_3385_:
    ld c, $10
    ld a, b
    cp c
    jp nc, _LABEL_338D_
    ld a, c
_LABEL_338D_:
    ld c, $97
    cp c
    jp c, _LABEL_3394_
    ld a, c
_LABEL_3394_:
    ld (RAM_SpriteTable1_Y), a
    ld a, (RAM_PenX_Smoothed)
    ld c, $21
    cp c
    jp nc, _LABEL_33A1_
    ld a, c
_LABEL_33A1_:
    ld c, $C9
    cp c
    jp c, _LABEL_33A8_
    ld a, c
_LABEL_33A8_:
    ld (RAM_SpriteTable1_XN), a
    ld a, $05
    call _LABEL_37C7_
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_PSGIsActive), a
    ld a, (RAM_SpriteTable1_Y)
    add a, $07
    ld ($C0C8), a
    ld a, (RAM_SpriteTable1_XN)
    add a, $07
    ld ($C0C9), a
    ld a, ($C089)
    set 3, a
    ld ($C089), a
    ret

; 11th entry of Jump Table from 2FD0 (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_33D1_:
    call _LABEL_18E6_
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
    jp c, _LABEL_33F2_
    ld b, c
_LABEL_33F2_:
    ld a, c
    add a, $58
    jp nc, _LABEL_33FA_
    ld a, $FF
_LABEL_33FA_:
    ld c, a
    cp b
    jp nc, _LABEL_3400_
    ld b, c
_LABEL_3400_:
    ld a, b
    ld c, $A6
    cp c
    jp c, _LABEL_3408_
    ld a, c
_LABEL_3408_:
    bit 0, (iy+0)
    call nz, _LABEL_34E0_
    ld (RAM_SpriteTable1_Y), a
    ld ($C0C6), a
    ld a, (RAM_PenX_Smoothed)
    ld b, a
    ld a, ($C0C5)
    add a, $07
    ld c, a
    cp b
    jp c, _LABEL_3424_
    ld b, c
_LABEL_3424_:
    ld a, c
    add a, $58
    jp nc, _LABEL_342C_
    ld a, $FF
_LABEL_342C_:
    ld c, a
    cp b
    jp nc, _LABEL_3432_
    ld b, c
_LABEL_3432_:
    ld a, b
    ld c, $D7
    cp c
    jp c, _LABEL_343A_
    ld a, c
_LABEL_343A_:
    bit 0, (iy+0)
    call z, _LABEL_3501_
    ld (RAM_SpriteTable1_XN), a
    ld ($C0C7), a
    ld a, $04
    call _LABEL_37C7_
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_PSGIsActive), a
    ld a, ($C089)
    set 2, a
    ld ($C089), a
    ld a, (RAM_SpriteTable1_Y)
    ld ($C0C6), a
    ld a, (RAM_SpriteTable1_XN)
    ld ($C0C7), a
    ret

_LABEL_3469_:
    ld c, $10
    ld a, b
    cp c
    jp nc, _LABEL_3471_
    ld a, c
_LABEL_3471_:
    ld c, $98
    cp c
    jp c, _LABEL_3478_
    ld a, c
_LABEL_3478_:
    bit 0, (iy+0)
    call nz, _LABEL_34ED_
    ld (RAM_SpriteTable1_Y), a
    ld a, (RAM_PenX_Smoothed)
    ld c, $21
    cp c
    jp nc, _LABEL_348C_
    ld a, c
_LABEL_348C_:
    ld c, $C9
    cp c
    jp c, _LABEL_3493_
    ld a, c
_LABEL_3493_:
    bit 0, (iy+0)
    call z, _LABEL_3513_
    ld (RAM_SpriteTable1_XN), a
    ld a, $05
    call _LABEL_37C7_
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_PSGIsActive), a
    ld a, ($C089)
    set 1, a
    ld ($C089), a
    ld a, (RAM_SpriteTable1_Y)
    ld ($C203), a
    add a, $07
    ld ($C0C4), a
    add a, $08
    ld (RAM_SpriteTable1_Y), a
    ld a, (RAM_SpriteTable1_XN)
    ld ($C246), a
    add a, $07
    ld ($C0C5), a
    add a, $08
    ld (RAM_SpriteTable1_XN), a
    ld a, $A9
    ld ($C247), a
    xor a
    ld ($C15D), a
    ld a, $25
    jp _LABEL_37C7_

_LABEL_34E0_:
    ld c, a
    ld a, ($C16F)
    add a, $60
    cp c
    jp nc, _LABEL_34EB_
    ld c, a
_LABEL_34EB_:
    ld a, c
    ret

_LABEL_34ED_:
    ld c, a
    ld a, ($C16F)
    sub $07
    cp c
    jp c, _LABEL_34F8_
    ld c, a
_LABEL_34F8_:
    add a, $58
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
    jp nz, _LABEL_3535_
    ld hl, $357F
_LABEL_3535_:
    ld a, b
    cp (hl)
    jp nc, _LABEL_353B_
    ld a, (hl)
_LABEL_353B_:
    inc hl
    cp (hl)
    jp c, _LABEL_3541_
    ld a, (hl)
_LABEL_3541_:
    ld (RAM_SpriteTable1_Y), a
    inc hl
    ld a, (RAM_PenX_Smoothed)
    cp (hl)
    jp nc, _LABEL_354D_
    ld a, (hl)
_LABEL_354D_:
    inc hl
    cp (hl)
    jp c, _LABEL_3553_
    ld a, (hl)
_LABEL_3553_:
    ld (RAM_SpriteTable1_XN), a
    inc hl
    ld a, (hl)
    inc hl
    call _LABEL_37C7_
    ex de, hl
    pop hl
    bit 1, (hl)
    ret z
    ex de, hl
    ld a, $01
    ld (RAM_PSGIsActive), a
    ld a, (RAM_SpriteTable1_Y)
    add a, (hl)
    ld ($C16F), a
    inc hl
    ld a, (RAM_SpriteTable1_XN)
    add a, (hl)
    ld ($C170), a
    ld a, ($C089)
    or $01
    ld ($C089), a
    ret

; Data from 357F to 358C (14 bytes)
.db $10 $40 $2B $CC $06 $07 $04 $1B $9C $20 $70 $07 $03 $08

; 12th entry of Jump Table from 2FD0 (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_358D_:
    call _LABEL_18E6_
    exx
    ld a, ($C089)
    bit 2, a
    jp nz, _LABEL_363C_
    ld c, $0F
    ld a, b
    cp c
    jp nc, _LABEL_35A1_
    ld a, c
_LABEL_35A1_:
    ld c, $7F
    cp c
    jp c, _LABEL_35A8_
    ld a, c
_LABEL_35A8_:
    ld (RAM_SpriteTable1_Y), a
    add a, $07
    ld ($C0C4), a
    add a, $21
    ld ($C0C6), a
    ld a, (RAM_PenX_Smoothed)
    ld c, $20
    cp c
    jp nc, _LABEL_35BF_
    ld a, c
_LABEL_35BF_:
    ld c, $B0
    cp c
    jp c, _LABEL_35C6_
    ld a, c
_LABEL_35C6_:
    ld (RAM_SpriteTable1_XN), a
    add a, $07
    ld ($C0C5), a
    add a, $21
    ld ($C0C7), a
    ld a, $05
    call _LABEL_37C7_
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_PSGIsActive), a
    ld a, ($C089)
    or $04
    ld ($C089), a
    ld a, (RAM_SpriteTable1_Y)
    ld ($C203), a
    add a, $08
    ld (RAM_SpriteTable1_Y), a
    ld a, (RAM_SpriteTable1_XN)
    ld ($C246), a
    add a, $08
    ld (RAM_SpriteTable1_XN), a
    ld a, $A9
    ld ($C247), a
    xor a
    ld ($C15D), a
    ld a, $25
    call _LABEL_37C7_
    ld c, $00
    ld a, (RAM_SpriteTable1_Y)
    sub $17
    cp $40
    jp nc, _LABEL_361A_
    set 0, c
_LABEL_361A_:
    ld a, (RAM_SpriteTable1_XN)
    sub $20
    cp $50
    jp nc, _LABEL_3626_
    set 1, c
_LABEL_3626_:
    ld a, c
    ld ($C172), a
    rlc c
    ld b, $00
    ld hl, $365E
    add hl, bc
    ld a, (hl)
    ld ($C162), a
    inc hl
    ld a, (hl)
    ld (RAM_SplashScreenTimeout), a
    ret

_LABEL_363C_:
    bit 1, (hl)
    jp nz, _LABEL_3655_
    ld a, (RAM_PenX_Smoothed)
    and $FE
    dec a
    ld (RAM_SpriteTable1_XN), a
    ld a, b
    and $FE
    ld (RAM_SpriteTable1_Y), a
    ld a, $08
    jp _LABEL_37C7_

_LABEL_3655_:
    ld a, ($C089)
    or $80
    ld ($C089), a
    ret

; Data from 365E to 3665 (8 bytes)
.db $00 $00 $50 $00 $00 $70 $50 $70

; 15th entry of Jump Table from 2FD0 (indexed by RAM_NonVBlankDynamicFunction)
_LABEL_3666_:
    call _LABEL_18E6_
    exx
    ld a, $58
    ld (RAM_SpriteTable1_XN), a
    ld a, b
    and $F8
    cp $40
    jp nc, _LABEL_3679_
    ld a, $40
_LABEL_3679_:
    cp $48
    jp c, _LABEL_3680_
    ld a, $48
_LABEL_3680_:
    ld (RAM_SpriteTable1_Y), a
    ld b, a
    ld a, $03
    call _LABEL_37C7_
    bit 1, (hl)
    ret z
    ld a, $01
    ld (RAM_PSGIsActive), a
    ld a, b
    sub $40
    jp z, _LABEL_3699_
    ld a, $01
_LABEL_3699_:
    ld ($C0BA), a
    ld a, (RAM_NonVBlankDynamicFunction)
    set 6, a
    ld (RAM_NonVBlankDynamicFunction), a
    ret

_LABEL_36A5_:
    ld a, $0F
    ld (RAM_SpriteTable1_Y), a
    ld a, (RAM_PenX_Smoothed)
    and $F8
    ld b, $28
    cp b
    jp c, _LABEL_36BC_
    ld b, $D0
    cp b
    jp nc, _LABEL_36BC_
    ld b, a
_LABEL_36BC_:
    ld a, b
    ld (RAM_SpriteTable1_XN), a
    sub $28
    rrca
    rrca
    rrca
    cp $10
    jp c, _LABEL_36F4_
    sub $11
    jp m, _LABEL_3711_
    bit 2, (hl)
    jp z, _LABEL_3711_
    cp $04
    jp z, _LABEL_36E4_
    ld ($C08A), a
    ld a, $01
    ld (RAM_PSGIsActive), a
    jp _LABEL_3711_

_LABEL_36E4_:
    ld a, $01
    ld (RAM_PSGIsActive), a
    ld a, ($C062)
    xor $01
    ld ($C062), a
    jp _LABEL_3711_

_LABEL_36F4_:
    bit 2, (hl)
    jp z, _LABEL_3711_
    ld ($C06C), a
    ld a, b
    ld ($C242), a
    ld a, $01
    ld (RAM_PSGIsActive), a
    ld a, ($C08A)
    cp $03
    jp nz, _LABEL_3711_
    xor a
    ld ($C08A), a
_LABEL_3711_:
    ld a, $A8
    ld ($C241), a
    ld a, $01
    call _LABEL_37C7_
    jp _LABEL_18E6_

_LABEL_371E_:
    ld hl, $C086
    dec (hl)
    ret p
    ld (hl), $04
    inc hl
    ld a, (hl)
    inc a
    cp $06
    jp c, _LABEL_372E_
    xor a
_LABEL_372E_:
    ld (hl), a
    ld hl, $3742
    ld d, $00
    ld e, a
    add hl, de
    ld de, $C01F
    rst $08 ; VDPAddressToDE
    ld b, $00
    djnz -3
    ld a, (hl)
    out (Port_VDPData), a
    ret

; Data from 3742 to 3747 (6 bytes)
.db $00 $03 $0C $0F $30 $3F

InitialiseCursorSprites:
    ld hl, InitialiseCursorSprites_Y
    ld de, RAM_SpriteTable1_Y
    ld bc, 4
    ldir
    ld hl, InitialiseCursorSprites_XN
    ld de, RAM_SpriteTable1_XN
    ld bc, 8
    ldir
    ret

; Data from 375F to 376A (12 bytes)
InitialiseCursorSprites_Y:
.db $40 $00 $E0 $E0 
InitialiseCursorSprites_XN:
.db $40 $A8 $28 $A7 $00 $A7 $00 $A9

_LABEL_376B_:
    ld hl, $C083
    bit 7, (hl)
    ret z
    res 7, (hl)
    set 6, (hl)
    ld de, $7500
    bit 5, (hl)
    jp z, _LABEL_3780_
    ld de, $7520
_LABEL_3780_:
    ld hl, ($C084)
    rst $08 ; VDPAddressToDE
    ld c, $BE
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    outi
    ret

_LABEL_37C7_:
    push bc
    push hl
    ld b, a
    ld hl, $C083
    ld a, (hl)
    and $3F
    cp b
    jp z, _LABEL_37EB_
    ld a, b
    ld (hl), a
    res 5, a
    push hl
    ld l, a
    ld h, $00
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, hl
    ld de, $3EB2
    add hl, de
    ld ($C084), hl
    pop hl
    set 7, (hl)
_LABEL_37EB_:
    pop hl
    pop bc
    ret

_LABEL_37EE_:
    ld hl, $C088
    ld a, (RAM_ButtonsPressed)
    ld c, a
    ld b, $03
    ld de, $7540
    bit 0, c
    jp nz, _LABEL_383C_
    bit 0, (hl)
    jp z, _LABEL_380E_
    res 0, (hl)
    push hl
      ld hl, $4092
      call Write2bppToVRAMSlowly
    pop hl
_LABEL_380E_:
    ld b, $03
    ld de, $75A0
    bit 1, c
    jp nz, _LABEL_384E_
    bit 1, (hl)
    jp z, _LABEL_3827_
    res 1, (hl)
    push hl
      ld hl, $40F2
      call Write2bppToVRAMSlowly
    pop hl
_LABEL_3827_:
    ld b, $03
    ld de, $7600
    bit 2, c
    jp nz, _LABEL_3860_
    bit 2, (hl)
    ret z
    res 2, (hl)
    ld hl, $4152
    jp Write2bppToVRAMSlowly

_LABEL_383C_:
    bit 0, (hl)
    jp nz, _LABEL_380E_
    set 0, (hl)
    push hl
      ld hl, $40C2
      call Write2bppToVRAMSlowly
    pop hl
    jp _LABEL_380E_

_LABEL_384E_:
    bit 1, (hl)
    jp nz, _LABEL_3827_
    set 1, (hl)
    push hl
      ld hl, $4122
      call Write2bppToVRAMSlowly
    pop hl
    jp _LABEL_3827_

_LABEL_3860_:
    bit 2, (hl)
    ret nz
    set 2, (hl)
    ld hl, $4182
    jp Write2bppToVRAMSlowly

_LABEL_386B_:
    ld ix, $C00B
    ld a, ($C08A)
    cp (ix+1)
    jp z, _LABEL_390F_
    bit 0, (ix+0)
    push af
    call nz, _LABEL_38C8_
    pop af
    ld (ix+1), a
    ld b, $01
    ld hl, $388C
    jp JumpToFunction

; Jump Table from 388C to 3893 (4 entries, indexed by $C08A)
.dw _LABEL_3894_ _LABEL_38A1_ _LABEL_38AE_ _LABEL_38BB_

; 1st entry of Jump Table from 388C (indexed by $C08A)
_LABEL_3894_:
    set 0, (ix+0)
    ld de, $73A0
    ld hl, $4002
    jp Write2bppToVRAM

; 2nd entry of Jump Table from 388C (indexed by $C08A)
_LABEL_38A1_:
    set 0, (ix+0)
    ld de, $73C0
    ld hl, $4022
    jp Write2bppToVRAM

; 3rd entry of Jump Table from 388C (indexed by $C08A)
_LABEL_38AE_:
    set 0, (ix+0)
    ld de, $73E0
    ld hl, $4042
    jp Write2bppToVRAM

; 4th entry of Jump Table from 388C (indexed by $C08A)
_LABEL_38BB_:
    set 0, (ix+0)
    ld de, $7400
    ld hl, $4062
    jp Write2bppToVRAM

_LABEL_38C8_:
    ld b, $01
    ld a, (ix+1)
    ld hl, $38D3
    jp JumpToFunction

; Jump Table from 38D3 to 38DA (4 entries, indexed by $C00C)
.dw _LABEL_38DB_ _LABEL_38E8_ _LABEL_38F5_ _LABEL_3902_

; 1st entry of Jump Table from 38D3 (indexed by $C00C)
_LABEL_38DB_:
    res 0, (ix+0)
    ld de, $73A0
    ld hl, $3FF2
    jp Write2bppToVRAM

; 2nd entry of Jump Table from 38D3 (indexed by $C00C)
_LABEL_38E8_:
    res 0, (ix+0)
    ld de, $73C0
    ld hl, $4012
    jp Write2bppToVRAM

; 3rd entry of Jump Table from 38D3 (indexed by $C00C)
_LABEL_38F5_:
    res 0, (ix+0)
    ld de, $73E0
    ld hl, $4032
    jp Write2bppToVRAM

; 4th entry of Jump Table from 38D3 (indexed by $C00C)
_LABEL_3902_:
    res 0, (ix+0)
    ld de, $7400
    ld hl, $4052
    jp Write2bppToVRAM

_LABEL_390F_:
    ld a, ($C062)
    cp (ix+2)
    ret z
    ld (ix+2), a
    ld b, $01
    bit 0, a
    jp z, _LABEL_3929_
    ld de, $7420
    ld hl, $4082
    jp Write2bppToVRAM

_LABEL_3929_:
    ld de, $7420
    ld hl, $4072
    jp Write2bppToVRAM

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
    jp c, _LABEL_3957_
    ld a, $0C
_LABEL_3957_:
    ld ($C0BF), a
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
    jp c, _LABEL_397A_
    ld a, $0C
_LABEL_397A_:
    ld ($C0C2), a
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
_LABEL_39DE_:
    ld a, (hl)
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
    djnz _LABEL_39DE_
    jp _LABEL_3A24_

; Data from 39F8 to 3A23 (44 bytes)
.db $C5 $78 $3D $4F $06 $00 $21 $CA $C0 $09 $EB $C5 $E1 $29 $01 $FA
.db $C0 $09 $23 $EB $C1 $7E $DD $77 $00 $DD $23 $2B $1A $FD $77 $01
.db $1B $1A $FD $77 $00 $FD $23 $FD $23 $1B $10 $E9

_LABEL_3A24_:
    ld a, c
    or a
    jp z, _LABEL_3A33_
_LABEL_3A29_:
    ld (ix+0), $E0
    inc ix
    dec a
    jp nz, _LABEL_3A29_
_LABEL_3A33_:
    ei
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
    djnz _LABEL_3A5A_
    ret

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
    djnz _LABEL_3AA5_
    ret

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
    jp nc, _LABEL_3AF5_
    ld a, (RAM_SpriteTable1_Y)
    add a, $07
    ld ($C16F), a
    ld c, a
    ld a, (RAM_SpriteTable1_XN)
    add a, $04
    ld ($C170), a
    ld ix, $C248
    ld de, $C204
    ld hl, $3B21
    jp _LABEL_3A5A_

_LABEL_3AF5_:
    ld a, (RAM_SpriteTable1_XN)
    add a, $08
    ld ($C170), a
    ld c, a
    ld a, (RAM_SpriteTable1_Y)
    add a, $03
    ld ($C16F), a
    ld ix, $C248
    ld de, $C204
    ld hl, $3B21
    jp _LABEL_3AA5_

_LABEL_3B13_:
    ld hl, $C203
    ld de, $C204
    ld bc, $003C
    ld (hl), $E0
    ldir
    ret

; Data from 3B21 to 3B2D (13 bytes)
.db $00 $08 $10 $18 $20 $28 $30 $38 $40 $48 $50 $58 $60

_LABEL_3B2E_:
    ld a, ($C00A)
    or a
    ret z
    dec a
    ld l, a
    xor a
    ld h, a
    ld ($C00A), a
    push hl
    add hl, hl
    add hl, hl
    push hl
    add hl, hl
    pop bc
    add hl, bc
    pop bc
    add hl, bc
    ld bc, $3B66 ; table?
    add hl, bc
    ld de, $7660 ; tile $1B3
    rst $08 ; VDPAddressToDE
    ld b, $0D
_LABEL_3B4D_:
    push bc
      ld a, (hl)
      push hl
        ld h, $00
        ld l, a
        add hl, hl
        add hl, hl
        add hl, hl
        add hl, hl
        ld bc, $41B2
        add hl, bc
        ld b, $01
        call Write2bppToVRAMCurrentAddress
      pop hl
      inc hl
    pop bc
    djnz _LABEL_3B4D_
    ret
    
.org $3b66
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $03 $0f $0c $0f $12 $00 $0d $0f $04 $05 $00 $00 $00 $05 $12 $01 $13 $05 $00 $0d $0f $04 $05 $00 $00 $00 $13 $11 $15 $01 $12 $05 $00 $0d $0f $04 $05 $00 $00 $03 $09 $12 $03 $0c $05 $00 $0d $0f $04 $05 $00 $00 $05 $0c $0c $09 $10 $13 $05 $00 $0d $0f $04 $05 $00 $10 $01 $09 $0e $14 $00 $0d $0f $04 $05 $00 $00 $00 $03 $0f $10 $19 $00 $0d $0f $04 $05 $00 $00 $00 $00 $0d $09 $12 $12 $0f $12 $00 $0d $0f $04 $05 $00 $00 $0d $01 $07 $0e $09 $06 $19 $00 $0d $0f $04 $05 $00 $04 $09 $13 $10 $0c $01 $19 $00 $0d $0f $04 $05 $00 $00 $00 $14 $08 $05 $00 $05 $0e $04 $00 $00 $00

.org $3c02
FontTiles:
.incbin "Font tiles.pscompr"

.org $3eb2
.incbin "Sega Graphic Board v2.0 [Proto]_3eb2.inc"

; blank to end of slot (somewhat unnecessarily)

.BANK 1 slot 1
.ORGA $4000

; Data from 4000 to 4551 (1362 bytes)
.incbin "Sega Graphic Board v2.0 [Proto]_4000.inc"

ControlTiles:
.incbin "Control tiles.pscompr"

; .smstag ; Doesn't entirely match
.orga $7ff0
.db "TMR SEGA", "WK", 
.dw $2688 ; checksum
.dw $4009 ; product code - not valid?
.db $02 ; version 2
.db $4c ; 32KB, SMS export
