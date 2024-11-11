; vim: ft=gbasm
\ifndef GB_INC
GB_INC = 1

GB_MMAP_ROM00_START    = $0000
GB_MMAP_ROM00_SIZE     = $4000
GB_MMAP_ROMXX_START    = $4000
GB_MMAP_ROMXX_SIZE     = $4000
GB_MMAP_VRAM_START     = $8000
GB_MMAP_VRAM_SIZE      = $2000
GB_MMAP_SRAM_START     = $A000
GB_MMAP_SRAM_SIZE      = $2000
GB_MMAP_WRAM0_START    = $C000
GB_MMAP_WRAM0_SIZE     = $1000
GB_MMAP_WRAMX_START    = $D000
GB_MMAP_WRAMX_SIZE     = $1000
GB_MMAP_OAM_START      = $FE00
GB_MMAP_OAM_SIZE       = $A0
GB_MMAP_WAVE_RAM_START = $FF30
GB_MMAP_WAVE_RAM_SIZE  = $10
GB_MMAP_HRAM_START     = $FF80
GB_MMAP_HRAM_SIZE      = $7F

GB_MBC5_BANK_LO = $2222

;; Joypad (R/W)
;;
;; Fields: ..ad3210
;; 0 - A/Right
;; 1 - B/Left
;; 2 - Select/Up
;; 3 - Start/Down
;; d - Read D-Pad
;; a - Read Action Buttons
;;
;; Every input/output line in the Joypad is active low.
;; For example, to read the D-Pad write %00100000 and then read the lower
;; nibble to get *inverted* the direction button states.
;;
GB_P1 = $FF00
GB_P1_BIT_READ_DIRECTION = 4
GB_P1_BIT_READ_ACTION    = 5

GB_SB = $FF01
GB_SC = $FF02

GB_DIV = $FF04
GB_TIMA = $FF05
GB_TMA = $FF06
GB_TAC = $FF07

;; Interrupt Flag (RW)
;;
;; Fields: ...jstlv
;; v - VBlank interrupt requested
;; l - LCD interrupt requested
;; t - Timer interrupt requested
;; s - Serial interrupt requested
;; j - Joypad interrupt requested
;;
GB_IF = $FF0F
GB_IF_BIT_VBLANK = 0
GB_IF_BIT_LCD    = 1
GB_IF_BIT_TIMER  = 2
GB_IF_BIT_SERIAL = 3
GB_IF_BIT_JOYPAD = 4

GB_LCDC = $FF40
GB_LCDC_BIT_BG_WIN_PRIO     = 0
GB_LCDC_BIT_OBJ_ENABLE      = 1
GB_LCDC_BIT_OBJ_SIZE        = 2
GB_LCDC_BIT_BG_TILE_MAP     = 3
GB_LCDC_BIT_TILE_DATA       = 4
GB_LCDC_BIT_WIN_ENABLE      = 5
GB_LCDC_BIT_WIN_TILE_MAP    = 6
GB_LCDC_BIT_ENABLE          = 7

GB_STAT = $FF41
GB_STAT_MASK_PPU_MODE = %11

GB_SCY = $FF42
GB_SCX = $FF43

GB_LY = $FF44
GB_LYC = $FF45

GB_DMA = $FF46

GB_WY = $FF4A
GB_WX = $FF4B

;; Speed Switch (R)
GB_KEY1 = $FF4D
GB_KEY1_BIT_ARMED = 0
GB_KEY1_BIT_SPEED = 7

GB_VBK = $FF4F

GB_BCPS = $FF68
GB_BCPS_BIT_INCREMENT = 7

GB_BCPD = $FF69

GB_OCPS = $FF6A
GB_OCPS_BIT_INCREMENT = 7

GB_OCPD = $FF6B

GB_SVBK = $FF70

;; Interrupt Enable (RW)
;;
;; Fields: ...jstlv
;; v - VBlank interrupt enabled
;; l - LCD interrupt enabled
;; t - Timer interrupt enabled
;; s - Serial interrupt enabled
;; j - Joypad interrupt enabled
;;
GB_IE = $FFFF
GB_IE_BIT_VBLANK = 0
GB_IE_BIT_LCD    = 1
GB_IE_BIT_TIMER  = 2
GB_IE_BIT_SERIAL = 3
GB_IE_BIT_JOYPAD = 4

\end ; GB_INC
