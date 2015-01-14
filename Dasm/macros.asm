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

.macro LD_BC_AREA args columns, rows
  ld bc, (columns*2) | (rows << 8)
.endm

.macro VDP_ADDRESS_TO_DE
  rst $08
.endm