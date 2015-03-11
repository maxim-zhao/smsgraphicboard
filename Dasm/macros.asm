.macro LD_DE_TILEMAP args x, y
  ld de, VDPAddressMask_Write | TileMapAddress | ((x + 32 * y) * 2)
.endm

.macro LD_DE_TILE args index
  ld de, VDPAddressMask_Write | (index * 32)
.endm

.macro LD_DE_PALETTE args index
  ld de, VDPAddressMask_Palette | index
.endm

.macro LD_DE_SPRITE_TABLE_Y args index
  ld de, VDPAddressMask_Write | SpriteTableAddress | index
.endm

.macro LD_DE_SPRITE_TABLE_X args index
  ld de, VDPAddressMask_Write | SpriteTableAddress | (index*2 + 128)
.endm

; For tilemap operations
.macro LD_BC_TILEMAP_AREA args columns, rows
  ld bc, (columns*2) | (rows << 8)
.endm

; For tile operations
.macro LD_BC_AREA args columns, rows
  ld bc, columns | (rows << 8)
.endm

; For tile operations and cursor movement
.macro LD_HL_LOCATION args x, y
  ld hl, x | (y << 8)
.endm

.macro VDP_ADDRESS_TO_DE
  rst $08
.endm

.macro LD_HL_A
  ld l,a
  ld h,0
.endm

.macro LD_HL_C
  ld l,c
  ld h,0
.endm

.macro LD_DE_A
  ld e,a
  ld d,0
.endm

; This is a bit flexible...
; 1 arg = HTML-style short colour code, e.g. $fff. We take the high two bits of each nibble.
; 2 args = repeating version of above
; 3 args = R, G, B in the range 0-3
; 4 args = repeating version of above
.macro COLOUR
.if NARGS == 1
.db ((\1 >> 10) & %000011) | ((\1 >> 4) & %001100) | ((\1 << 2) & %110000)
.else
.if NARGS == 2
.rept \1
 COLOUR \2 ; Recurse!
.endr
.else
.if NARGS == 3
.db (\1 & %11) | ((\2 & %11) << 2) | ((\3 & %11) << 4)
.else
.if NARGS == 4
.rept \1
 COLOUR \2 \3 \4 ; Recurse!
.endr
.else
.endif
.endif
.endif
.endif
.endm

.macro TILEMAPENTRY args tileIndex, flags
.dw tileIndex | (flags << 8)
.endm
