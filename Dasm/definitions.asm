; Port numbers

.define Port_IOPortControl $3f
.define Port_PSG           $7f
.define Port_VDPData       $be
.define Port_VDPAddress    $bf
.define Port_VDPStatus     $bf
.define Port_IOPort1       $dc
.define Port_IOPort2       $dd

; Values for Port_IOPortControl
; These can be ORed together, best to specify four each time
; It's presumably irrelevant what the high nibble is when the bit is set to IN... but the Graphic Board software uses 1.
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



; VDP data sizes
.define SizeOfTile 32
.define SizeOfNameTableEntry 2
.define TilesPerRow 32
.define SizeOfRow TilesPerRow*SizeOfNameTableEntry
.define SizeOfVRAM 16*1024

; Tile attribute flags
.define TileAttribute_None     %00000000
.define TileAttribute_Priority %00010000
.define TileAttribute_Palette2 %00001000
.define TileAttribute_VFlip    %00000100
.define TileAttribute_HFlip    %00000010

; VRAM address masks
.define VDPAddressMask_Read     %00 << 14
.define VDPAddressMask_Write    %01 << 14
.define VDPAddressMask_Register %10 << 14
.define VDPAddressMask_Palette  %11 << 14

; VRAM addresses
.define TileMapAddress $3800
.define SpriteTableAddress $3f00

.define SpriteTableYTerminator 224


; Sound stuff
.define PSG_Latch    %10000000
.define PSG_Data     %00000000
.define PSG_Channel0 0<<5
.define PSG_Channel1 1<<5
.define PSG_Channel2 2<<5
.define PSG_Channel3 3<<5
.define PSG_Tone     %00000000
.define PSG_Volume   %00010000
