.macro LdDETilemap args x, y
  ld de, VDPAddressMask_Write | TileMapAddress | ((x + 32 * y) * 2)
.endm

.macro LdDETile args index
  ld de, VDPAddressMask_Write | (index * 32)
.endm

.macro LdDEPalette args index
  ld de, VDPAddressMask_Palette | index
.endm

.macro ldDESpriteTableY args index
  ld de, VDPAddressMask_Write | SpriteTableAddress | index
.endm

.macro ldDESpriteTableX args index
  ld de, VDPAddressMask_Write | SpriteTableAddress | (index*2 + 128)
.endm
