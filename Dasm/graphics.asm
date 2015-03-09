.section "Graphics helpers" force
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
      ld hl, DRAWING_AREA_WIDTH_TILES * SizeOfTile
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
    ld a, 1
    ld (RAM_SpriteTable2DirtyFlag), a
    ret
.ends

