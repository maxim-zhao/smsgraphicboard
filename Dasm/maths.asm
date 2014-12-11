DivMod_hl_e_hl_a:
    ; hl = 16-bit number
    ; e = 8-bit number
    ; returns
    ; hl = hl / e
    ; a  = hl % e
    ld b, 17   ; bits
    xor a
    jp +++
-:  adc a, a   ; shift carry into a
    jr c, +    ; if nothing came out
    cp e       ; check if it's bigger than e
    jr c, ++
+:  sub e      ; if so, subtract it
    or a       ; then clear the carry flag
++: ccf
+++:adc hl, hl ; shift hl let into carry
    djnz -
    ret

DivMod_hl_e_a_e:
    ; hl = 16-bit number
    ; e = 8-bit number
    ; returns 
    ; a = hl / e
    ; e = hl % e
    ld a, e
    or a
    ret z ; avoid divide by 0
    ld b, 8 ; Bits
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

Multiply_a_b_hl:
    ; Unused function
    ; Calculates hl = a * b
    ; Trashes a,b,c
    ld l, 0 ; hl = a
    ld h, a
    ld c, b ; bc = b
    ld b, 0
    ld a, 8 ; 8 bits
-:  add hl, hl ; hl *= 2
    jp nc, +
    add hl,bc ; Where we carry, add in bc
+:  dec a
    jp nz, -
    ret
    
Multiply_l_e_hl:
    ; Calculates hl = l * e
    ; Trashes d
    ; e is unmodified
    ld h, l ; hl = l * 256
    ld b, 8 ; 8 bits
    ld d, 0 ; de = e
    ld l, d
-:  add hl, hl ; hl *= 2
    jr nc, +
    add hl, de ; Where we carry, add in de
+:  djnz -
    ret

Multiply_a_de_ahl:
    ; Calculates ahl = a * de
    ld hl, 0
    or a
    ret z   ; Return hl = 0 if a = 0
    ld b, 8 ; 8 bits
-:  add hl, hl ; hl *= 2
    adc a, a   ; a = a * 2 + carry
    jr nc, +
    add hl, de ; Where we carry, add in de
    adc a, 0   ; And carry into a
+:  djnz -
    ret
    