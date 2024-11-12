; vim: ft=gbasm
\include "gb.inc"

\section "WRAM0"

stackTop: \space 128
stackBase:

\section "HOME"

\struct TESTING
    .hello 1
\end

Hello:
    \create TESTING

FOO=Hello.hello

Start::
    di
    ld sp, stackBase
    call VideoDisable
    call DoubleSpeedMode

    ld hl, GB_MMAP_HRAM_START
    ld bc, GB_MMAP_HRAM_SIZE
    call MemZero

    ld a, 1
    ldh [romBank], a
    ld [GB_MBC5_ROMX_BANK_LO], a
    xor a, a
    ld [GB_MBC5_ROMX_BANK_HI], a

    ld hl, GB_MMAP_WRAM0_START
    ld bc, GB_MMAP_WRAM0_SIZE
.WRAM0:
    ld a, c
    or a, b
    jr z, .WRAMX
    xor a, a
    ldi [hl], a
    dec bc
    jr .WRAM0
.WRAMX:
    \loop BANK, 7
        ld a, BANK+1
        ldh [GB_SVBK], a
        ld hl, GB_MMAP_WRAMX_START
        ld bc, GB_MMAP_WRAMX_SIZE
        call MemZero
    \end

    ld a, 1
    ldh [GB_SVBK], a

    call VideoInit
    call TimerInit
    call SerialInit

    ld hl, GB_IE
    set GB_IE_BIT_VBLANK, [hl]

    ei
    call VideoEnable
.Halt:
    halt
    jr .Halt

DoubleSpeedMode:
    ; Exit if already in double speed mode
    ld hl, GB_KEY1
    bit GB_KEY1_BIT_SPEED, [hl]
    ret nz
    ; Disable all interrupt flags (stop acts really weird without these)
    xor a, a
    ldh [GB_IE], a
    ldh [GB_IF], a
    ; Arm the speed switch
    set GB_KEY1_BIT_ARMED, [hl]
    ; Oddly, we need to reset the joypad lines before we speed switch
    ld a, (1 << GB_P1_BIT_READ_ACTION) | (1 << GB_P1_BIT_READ_DIRECTION)
    ldh [GB_P1], a
    stop
    ret

