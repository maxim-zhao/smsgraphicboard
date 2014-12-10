DivMod16_8_16_8:
    ; hl = 16-bit number
    ; e = 8-bit number
    ; returns
    ; hl = hl / e
    ; a  = hl % e
    ld b, $11  ; 17 bits
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

DivMod16_8_8_8:
    ; hl = 16-bit number
    ; e = 8-bit number
    ; returns 
    ; a = hl / e
    ; e = hl % e
    ld a, e
    or a
    ret z ; avoid divide by 0
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
