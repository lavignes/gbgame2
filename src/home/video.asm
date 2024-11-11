; vim: ft=gbasm
\include "gb.inc"

\section "OAMBUF"

;; OAM source buffer
oamBuf: \space GB_MMAP_OAM_SIZE

\section "HRAM"

;; Holds the DMA transfer routine
dmaFunction: \space 10

;; Whether vblank has occurred
vBlanked:: \space 1

\section "HOME"

VideoInit::
    ld hl, dmaFunction
    ld de, DMAFunction
    ld bc, DMAFunction.End - DMAFunction
    call MemCopy
    \loop BANK, 2
        ld a, BANK
        ldh [GB_VBK], a
        ld hl, GB_MMAP_VRAM_START
        ld bc, GB_MMAP_VRAM_SIZE
        call MemZero
    \end
    ; TODO we need a function to fade palettes, so we should reuse that
    \loop INDEX, 8
        ld a, 1
        ld c, a
        ld a, INDEX
        ld hl, PaletteAllBlack
        call VideoBGPaletteWrite
        ld a, 1
        ld c, a
        ld a, INDEX
        ld hl, PaletteAllBlack
        call VideoOBJPaletteWrite
    \end
    xor a, a
    ldh [GB_VBK], a
    ldh [GB_WY], a
    ldh [GB_WX], a
    ldh [GB_SCY], a
    ldh [GB_SCX], a
    ldh [GB_LYC], a
    ldh [GB_STAT], a
    ld a, (1 << GB_LCDC_BIT_BG_WIN_PRIO) |\
          (1 << GB_LCDC_BIT_OBJ_ENABLE) |\
          (1 << GB_LCDC_BIT_OBJ_SIZE)
    ldh [GB_LCDC], a
    ret

; NOTE only call after disabling PPU interrupts
;
; TODO should I therefore automatically disable interrupts?
VideoDisable::
    ; Already disabled?
    ld hl, GB_LCDC
    bit GB_LCDC_BIT_ENABLE, [hl]
    ret z
    call VideoWaitForVBlank
    ld hl, GB_LCDC
    res GB_LCDC_BIT_ENABLE, [hl]
    ret

VideoEnable::
    ld hl, GB_LCDC
    set GB_LCDC_BIT_ENABLE, [hl]
    ret

VideoWaitForVBlank::
    ld hl, GB_STAT
.Wait:
    ld a, [hl]
    and a, GB_STAT_MASK_PPU_MODE
    jr nz, .Wait
    ret

;; Copy `C` BG palettes from `HL` to palette index `A`
VideoBGPaletteWrite::
    sla c ; C *= 4
    sla c
    add a, a ; A *= 8
    add a, a
    add a, a
    ; Load pal index
    ld de, GB_BCPS
    set GB_BCPS_BIT_INCREMENT, a ; auto-increment
    ld [de], a
    ; DE is now GB_BCPD
    inc de
.Loop:
    ; Each iteration copies 1 16-bit color
    ldi a, [hl]
    ld [de], a
    ldi a, [hl]
    ld [de], a
    dec c
    jr nz, .Loop
    ret

;; copy `C` OBJ palettes from `HL` to palette index `A`
VideoOBJPaletteWrite::
    sla c ; C *= 4
    sla c
    add a, a ; A *= 8
    add a, a
    add a, a
    ; Load pal index
    ld de, GB_OCPS
    set GB_OCPS_BIT_INCREMENT, a ; auto-increment
    ld [de], a
    ; DE is now GB_OCPD
    inc de
    jr VideoBGPaletteWrite.Loop

DMAFunction:
    ld a, >oamBuf
    ldh [GB_DMA], a
    ld a, GB_MMAP_OAM_SIZE
.Wait:
    dec a
    jr nz, .Wait
    ret
.End:

