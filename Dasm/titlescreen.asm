.define LogoHeightPixels 35

.enum 0 ; Flags by bit number
  Flag_BlindsDone db ; 0 when drawing blinds effect, 1 when done
  Flag_RollDone   db ; 0 when drawing roll effect, 1 when done
  Flag_2 db
  Flag_3 db
  Flag_4 db
  Flag_5 db
  Flag_6 db
  Flag_EffectRunning  db ; 0 when effect code is running, 1 otherwise
.ende

.section "Title screen main loop" force
TitleScreen: ; $865
    ; blank RAM for title screen animation
    ld hl, RAM_TitleScreen
    ld de, RAM_TitleScreen+1
    ld bc, 8
    ld (hl), 0
    ldir

    ; Initialise timeout counter
    ld hl, 8.54*60 ; $0200 ; 8533ms
    ld (RAM_TitleScreen.Timeout), hl

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
+:  ld (RAM_TitleScreen.VBlankControl), a ; gets either $8a or $88 accordingly

    ; Draw into tilemap for splash screen animation
    ld b, 0   ; Draw into line 0
    ld c, LogoHeightPixels ; Draw all lines
-:  push bc
      call UpdateSplashScreenAnimationTilesLine
    pop bc
    inc b
    dec c
    jp p, -

    ld a, 31
    ld (RAM_TitleScreen.BlindsCounter), a

    call ScreenOn

    ei
-:  ld a, (RAM_TitleScreen.VBlankControl)
    call SetVBlankFunctionAndWait

    ld a, (RAM_ButtonsNewlyPressed)
    and %0000111 ; any button
    jp nz, TitleScreenButtonPressed

    ; No button pressed
    ; Update title screen state
    ld hl, - ; push loop address
    push hl
      ld ix, RAM_TitleScreen
      bit Flag_BlindsDone, (ix+TitleScreen.Flags)
      jp z, TitleScreenAnimate_Blinds ; Blinds slide animation
      bit Flag_RollDone, (ix+TitleScreen.Flags)
      jp z, TitleScreenAnimate_Roll ; Pixel slide/flip animation
    inc sp ; Discard loop address - could have popped it...
    inc sp
    di

; Matching push above, ignore
.endasm
pop hl
.asm

TitleScreen_PostAnimationLoop:
    ld hl, Tilemap_SegaLogo
    LD_DE_TILEMAP 11, 3
    LD_BC_AREA 10, 4
    ld a, $01
    ld (RAM_VRAMFillHighByte), a
    call WriteAreaToTilemap_1byte

    ld hl, Text_CopyrightSega1987
    LD_DE_TILEMAP 10, 22
    ld b, 12 ; length
    xor a
    call RawDataToVRAM_Interleaved1
    call ScreenOn
    ei

_CheckForGraphicsBoard:
    in a, (Port_IOPort1)
    and %11101111 ; $EF ; Ignore TL
    cp  %11100000 ; $E0 ; Expect UDLR lines low, rest high
.ifdef BypassDetection
    jp GraphicsBoardDetected
.else
    jp z, GraphicsBoardDetected
.endif

    ld hl, Text_NotGraphicBoard
    ld (RAM_TitleScreenTextPointer), hl
    LD_DE_TILEMAP 6, 16
    ld (RAM_TitleScreenTextLocation), de
    ld bc, 20 | (12<<8)
    ld (RAM_TitleScreenTextLength), bc ; also sets RAM_TitleScreenTextFlashSpeed
    ld a, 1<<VBlankFunctionControl_TitleScreen + 1<<VBlankFunctionControl_TitleScreen_UpdateText
    call SetVBlankFunctionAndWait

    ; decrement timeout counter
    ld hl, (RAM_TitleScreen.Timeout)
    dec hl
    ld (RAM_TitleScreen.Timeout), hl
    ld a, l
    or h
    jp z, TitleScreenTimedOut
    jp _CheckForGraphicsBoard

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
    jp z, _CheckForGraphicsBoard
.else
    jp nz, _CheckForGraphicsBoard
.endif

    ; decrement title screen counter again
    ld hl, (RAM_TitleScreen.Timeout)
    dec hl
    ld (RAM_TitleScreen.Timeout), hl
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
    LD_BC_AREA 32, 5 ; 5 rows, 32 columns
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
 COLOUR 0,0,1 ; dark blue
 COLOUR 0,0,0 ; Black
 COLOUR 3,3,3 ; White
 COLOUR 0,0,0 ; Black
 COLOUR 0,0,3 ; Bright blue
 COLOUR 0,0,0 ; Black
 COLOUR 3,3,3 ; White
.ends

.section "Title screen animation" force
TitleScreenAnimate_Blinds:
    set Flag_EffectRunning, (ix+TitleScreen.Flags) ; set high flag
    inc (ix+TitleScreen.BlindsOffset) ; increment blinds offset
    dec (ix+TitleScreen.BlindsCounter) ; decrement BlindsCounter
    ret p      ; if it's reached -1:
    set Flag_BlindsDone, (ix+TitleScreen.Flags) ; set flag for phase 2
    ret

UpdateTilemap_RightToLeftRow:
    ; Offset by 
    ld a, (ix+TitleScreen.BlindsOffset)
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
    ld b, (ix+TitleScreen.BlindsOffset)
    inc b
    jp RawDataToVRAM_Interleaved2

UpdateTilemap_LeftToRightRow:
    ; get animation control value
    ld c, (ix+TitleScreen.BlindsOffset)
    ; hl += 31 - n
    ld a, 31
    sub c
    ld c, a
    ld b, 0
    add hl, bc
    ld b, (ix+TitleScreen.BlindsOffset)
    inc b
    jp RawDataToVRAM_Interleaved2

TitleScreenAnimate_Roll:
    set Flag_EffectRunning, (ix+TitleScreen.Flags) ; Set high bit of flags
    inc (ix+TitleScreen.RollPixelCount)    ; Increment RollPixelCount
    ld a, (ix+TitleScreen.RollPixelCount)  ; check if it's reached 36
    cp LogoHeightPixels + 1
    ret nz
    set Flag_RollDone, (ix+TitleScreen.Flags) ; Set flag 1 when it does to go to phase 3
    ret

UpdateSplashScreenAnimationTilesLine:
    ; parameters = b, c
    ; b = pixel row to draw into
    ; c = logo row to draw there

    ; get b
    ld a, b
    ; get high 5 bits
    and %11111000
    ; multiply by 108 = width in tiles * 4
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

    ld b, 27 ; counter: number of tiles per row
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

UpdateSplashScreenAnimationTilesBlankLine:
    ; Draws a blank line (palette entry 0) into the tiles at row b
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
      xor a ; We write zeroes...
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
    ld a, (RAM_TitleScreen.Flags) ; check for high bit = new write
    bit Flag_EffectRunning, a
    ret z
    and (1<<Flag_EffectRunning) ~ $ff ; clear running flag
    ld (RAM_TitleScreen.Flags), a
    bit Flag_BlindsDone, a
    jp z, TitleScreenVBlank_Blinds
    bit Flag_RollDone, a
    jp z, TitleScreenVBlank_Roll
    ret

TitleScreenVBlank_Blinds:
    ; Update the five rows of tilemap
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

TitleScreenVBlank_Roll:
    ; Draw the "down" part of the animation (from the top)
    ld c, LogoHeightPixels
    ld b, (ix+TitleScreen.RollPixelCount)
-:  push bc
      call UpdateSplashScreenAnimationTilesLine
    pop bc
    dec c
    dec b
    jp p, -

    ; After the 17th frame, we no longer need to draw the "up" part
    ld a, (ix+TitleScreen.RollPixelCount)
    cp LogoHeightPixels / 2
    jp nc, +

    ; Draw the "up" part of the animation (down to the bottom)
    inc a
    ld d, a
    ld a, LogoHeightPixels
    sub d
    ld b, a
    ld c, 0
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

    ld a, LogoHeightPixels
    sub (ix+TitleScreen.RollPixelCount)
    ld b, a
    call UpdateSplashScreenAnimationTilesBlankLine
+:  ret
.ends

.section "Title screen data 2" force
;.orga $b8a
Tilemap_Logo:
.incbin "Graphics/Logo tilemap.bin"
;.orga $c2a
Palette_Logo:
 COLOUR 0,0,1 ; Dark blue
 COLOUR 3,3,3 ; White
 COLOUR 1,1,1 ; Dark grey
 COLOUR 2,2,2 ; Light grey
 COLOUR 0,0,0 ; Black x4
 COLOUR 0,0,0
 COLOUR 0,0,0
 COLOUR 0,0,0
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
