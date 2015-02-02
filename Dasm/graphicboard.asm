.enum 0
GraphicBoardButtonBit_Menu db
GraphicBoardButtonBit_Do db
GraphicBoardButtonBit_Pen db
.ende

ReadGraphicBoard:
    ; Main graphic board read
    ;
    ; If TL is 1, the graphic board has no pen data and only buttons are read.
    ; Bit 3 should always be 0.
    ; Bit 2 is 1 when the Pen button is down
    ; Bit 1 is 1 when the Do button is down
    ; Bit 0 is 1 when the Menu button is down
    ; 
    ; Cycles       |36|238|60|
    ; Read TL      X
    ; TR        ------|______|---
    ; TH        -----------------
    ; Read data           X
    ;              Buttons^
    ;
    ; If it is 0, pen pressure is read.
    ; The data read is the high nibble followed by the low.
    ; If the value is too low, it stops there. (TODO: calculate the time - it is quite short.)
    ;
    ; Cycles       |10|238|1266|533|45|533|???|
    ; Read TL      X
    ; TR        ------|_______________________|---
    ; TH        ---------------|______|-----------
    ; Read data           X        X      X
    ;              Buttons^        Pressure
    ;
    ; If the pen pressure is OK, it continues on to read the X and Y positions.
    ;
    ; Cycles       |10|238|1266|533|45|533|204|533|45|533|230|533|45|533|42|
    ; Read TL      X
    ; TR        ------|____________________________________________________|---
    ; TH        ---------------|______|-------|______|-------|______|----------
    ; Read data           X        X      X       X      X       X      X
    ;              Buttons^        Pressure       ^Pen X^^       ^Pen Y^^
    ;
    ; Note that during the title screen, the board is read with TH floating and TR high.
    ; In this state, the button data is returned. Thus it's unclear why this code waits 
    ; before reading the buttons...

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

    ld b, 92
-:  djnz - ; delay: 1266 cyles = 354us from in to out
    nop
    nop

    ; ============================================================= Read 2 (TH = 0, 1)
    
    ld a, IO_TR1_OUT_0 | IO_TH1_OUT_0 | IO_TR2_OUT_0 | IO_TH2_OUT_0 ; all output, all zero
    out (Port_IOPortControl), a

    ld b, 40
-:  djnz - ; delay: 533 cycles = 149us from out to in

    ; read low 4 bits
    in a, (Port_IOPort1)
    and %00001111
    rlca
    rlca
    rlca
    rlca
    ld d, a
    
    ; no delay: 45 cycles = 13us from in to out

    ld a, IO_TR1_OUT_0 | IO_TH1_OUT_1 | IO_TR2_OUT_0 | IO_TH2_OUT_0
    out (Port_IOPortControl), a

    ld b, 40
-:  djnz - ; delay: 533 cycles = 149us from out to in

    in a, (Port_IOPort1)
    and %00001111
    or d
    cp 253
    jp c, PressureTooLow
    ld (RAM_Pressure), a

    ld b, 11
-:  djnz - ; delay: 204 cycles = 57 cycles from in to out

    ; ============================================================= Read 3 (TH = 0, 1)

    ld a, IO_TR1_OUT_0 | IO_TH1_OUT_0 | IO_TR2_OUT_0 | IO_TH2_OUT_0
    out (Port_IOPortControl), a

    ld b, 40
-:  djnz - ; delay: 533 cycles = 149us from out to in

    in a, (Port_IOPort1)
    and %00001111
    rlca
    rlca
    rlca
    rlca
    ld h, a
    
    ; no delay: 45 cycles = 13us from in to out
    
    ld a, IO_TR1_OUT_0 | IO_TH1_OUT_1 | IO_TR2_OUT_0 | IO_TH2_OUT_0
    out (Port_IOPortControl), a

    ld b, 40
-:  djnz - ; delay: 533 cycles = 149us from out to in

    in a, (Port_IOPort1)
    and %00001111
    or h
    ld (RAM_PenX), a
    nop

    ld b, 14
-:  djnz - ; delay: 230 cycles = 64us from in to out

    ; ============================================================= Read 4 (TH = 0, 1)

    ld a, IO_TR1_OUT_0 | IO_TH1_OUT_0 | IO_TR2_OUT_0 | IO_TH2_OUT_0
    out (Port_IOPortControl), a

    ld b, 40
-:  djnz - ; delay: 533 cycles = 149us from out to in

    in a, (Port_IOPort1)
    and %00001111
    rlca
    rlca
    rlca
    rlca
    ld d, a
    
    ; no delay: 45 cycles = 13us from in to out
     
    ld a, IO_TR1_OUT_0 | IO_TH1_OUT_1 | IO_TR2_OUT_0 | IO_TH2_OUT_0
    out (Port_IOPortControl), a

    ld b, 40
-:  djnz - ; delay: 533 cycles = 149us from out to in

    in a, (Port_IOPort1)
    and %00001111
    or d
    ld (RAM_PenY), a

    ; ============================================================= Done: set TR, TH

    ; no delay: 42 cycles = 12us from in to out
     
    ld a, IO_TR1_OUT_1 | IO_TH1_OUT_1 | IO_TR2_OUT_0 | IO_TH2_OUT_0 ; $30
    out (Port_IOPortControl), a

    ld a, (RAM_PenX)
    ld c, a
    ld a, (RAM_Pen_Smoothed.x)
    or a
    jp z, +

    ; RAM_Pen_Smoothed.x = (RAM_Pen_Smoothed.x + RAM_PenX) / 2
    ld b, 0
    ld h, 0
    ld l, a
    add hl, bc ; add together
    ld e, 2
    call DivMod_hl_e_a_e ; very slow way to do this
    ld (RAM_Pen_Smoothed.x), a
    jp ++

+:  ld a, c
    ld (RAM_Pen_Smoothed.x), a

++: ld a, (RAM_PenY)
    ld c, a
    ld a, (RAM_Pen_Smoothed.y)
    or a
    jp z, +

    ; RAM_Pen_Smoothed.y = (RAM_Pen_Smoothed.y + RAM_PenY) / 2
    ld b, 0
    ld h, 0
    ld l, a
    add hl, bc
    ld e, 2
    call DivMod_hl_e_a_e ; very slow way to do this
    ld (RAM_Pen_Smoothed.y), a
    ret

+:  ld a, c
    ld (RAM_Pen_Smoothed.y), a
    ret

PressureTooLow:
    ld a, IO_TR1_OUT_1 | IO_TH1_OUT_1 | IO_TR2_OUT_0 | IO_TH2_OUT_0
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

    ld b, 16
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

    ; no delay: 60 cycles = 17us from in to out

    ld a, IO_TR1_OUT_1 | IO_TH1_OUT_1 | IO_TR2_OUT_0 | IO_TH2_OUT_0
    out (Port_IOPortControl), a
    ret
