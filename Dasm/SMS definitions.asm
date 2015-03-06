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
.define IO_TR1_Out1 %00010000
.define IO_TR1_Out0 %00000000
.define IO_TR1_In   %00010001
.define IO_TH1_Out1 %00100000
.define IO_TH1_Out0 %00000000
.define IO_TH1_In   %00100010
.define IO_TR2_Out1 %01000000
.define IO_TR2_Out0 %00000000
.define IO_TR2_In   %01000100
.define IO_TH2_Out1 %10000000
.define IO_TH2_Out0 %00000000
.define IO_TH2_In   %10001000

.define IOPort2_RESET %00010000

; VDP register values
.define VDPR0B0_VideoSync_Bit         0
.define VDPR0B0_VideoSync_ON          0<<VDPR0B0_VideoSync_Bit
.define VDPR0B0_VideoSync_OFF         1<<VDPR0B0_VideoSync_Bit
.define VDPR0B1_ExtraHeightModes_Bit  1
.define VDPR0B1_ExtraHeightModes_ON   1<<VDPR0B1_ExtraHeightModes_Bit
.define VDPR0B1_ExtraHeightModes_OFF  0<<VDPR0B1_ExtraHeightModes_Bit
.define VDPR0B2_SMSMode_Bit           2
.define VDPR0B2_SMSMode_ON            1<<VDPR0B2_SMSMode_Bit
.define VDPR0B2_SMSMode_OFF           0<<VDPR0B2_SMSMode_Bit
.define VDPR0B3_SpriteShift_Bit       3
.define VDPR0B3_SpriteShift_ON        1<<VDPR0B3_SpriteShift_Bit
.define VDPR0B3_SpriteShift_OFF       0<<VDPR0B3_SpriteShift_Bit
.define VDPR0B4_LineInterrupts_Bit    4
.define VDPR0B4_LineInterrupts_ON     1<<VDPR0B4_LineInterrupts_Bit
.define VDPR0B4_LineInterrupts_OFF    0<<VDPR0B4_LineInterrupts_Bit
.define VDPR0B5_BlankLeftColumn_Bit   5
.define VDPR0B5_BlankLeftColumn_ON    1<<VDPR0B5_BlankLeftColumn_Bit
.define VDPR0B5_BlankLeftColumn_OFF   0<<VDPR0B5_BlankLeftColumn_Bit
.define VDPR0B6_FixTop2Rows_Bit       6
.define VDPR0B6_FixTop2Rows_ON        1<<VDPR0B6_FixTop2Rows_Bit
.define VDPR0B6_FixTop2Rows_OFF       0<<VDPR0B6_FixTop2Rows_Bit
.define VDPR0B7_FixRight8Columns_Bit  7
.define VDPR0B7_FixRight8Columns_ON   1<<VDPR0B7_FixRight8Columns_Bit
.define VDPR0B7_FixRight8Columns_OFF  0<<VDPR0B7_FixRight8Columns_Bit

.define VDPR1B0_ZoomedSprites_Bit     0
.define VDPR1B0_ZoomedSprites_ON      1<<VDPR1B0_ZoomedSprites_Bit
.define VDPR1B0_ZoomedSprites_OFF     0<<VDPR1B0_ZoomedSprites_Bit
.define VDPR1B1_DoubledSprites_Bit    1
.define VDPR1B1_DoubledSprites_ON     1<<VDPR1B1_DoubledSprites_Bit
.define VDPR1B1_DoubledSprites_OFF    0<<VDPR1B1_DoubledSprites_Bit
.define VDPR1B2                       0<<2 ; Always 0
.define VDPR1B3_30RowMode_Bit         3
.define VDPR1B3_30RowMode_ON          1<<VDPR1B3_30RowMode_Bit
.define VDPR1B3_30RowMode_OFF         0<<VDPR1B3_30RowMode_Bit
.define VDPR1B4_28RowMode_Bit         4
.define VDPR1B4_28RowMode_ON          1<<VDPR1B4_28RowMode_Bit
.define VDPR1B4_28RowMode_OFF         0<<VDPR1B4_28RowMode_Bit
.define VDPR1B5_VBlankInterrupts_Bit  5
.define VDPR1B5_VBlankInterrupts_ON   1<<VDPR1B5_VBlankInterrupts_Bit
.define VDPR1B5_VBlankInterrupts_OFF  0<<VDPR1B5_VBlankInterrupts_Bit
.define VDPR1B6_EnableDisplay_Bit     6
.define VDPR1B6_EnableDisplay_ON      1<<VDPR1B6_EnableDisplay_Bit
.define VDPR1B6_EnableDisplay_OFF     0<<VDPR1B6_EnableDisplay_Bit
.define VDPR1B7                       1<<7 ; Always 1

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
.define SpriteSet 1

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



; RAM
.define RAM_Start $c000
.define SizeOfRAM $2000
