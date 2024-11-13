; vim: ft=gbasm

\section "HOME"

;; Copy `BC` bytes from `HL` to `DE`
MemCopy::
    inc b
    inc c
    jr .Decrement
.CopyByte:
    ldi a, [hl]
    ld [de], a
    inc de
.Decrement:
    dec c
    jr nz, .CopyByte
    dec b
    jr nz, .CopyByte
    ret

;; Set `BC` bytes to `$00` starting at `HL`
MemZero::
    xor a, a
    ; fallthru to MemSet

;; Set `BC` bytes to `A` starting at `HL`
MemSet::
    inc b
    inc c
    jr .Decrement
.SetByte:
    ldi [hl], a
.Decrement:
    dec c
    jr nz, .SetByte
    dec b
    jr nz, .SetByte
    ret

