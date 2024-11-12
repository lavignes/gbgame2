; vim: ft=gbasm
\include "gb.inc"

\section "HRAM"

;; Saved ROMX bank
romBank:: \space 1

\section "HOME"

;; Call function at e:hl
FarCallHL::
    ldh a, [romBank]
    ; TODO: in debug mode, panic if already in bank
.SwitchBanks:
    push af ; push return bank
    ld a, e
    ldh [romBank], a
    ld [GB_MBC5_ROMX_BANK_LO], a
    rst RstCallHL
    ; restore bank and return
    pop af
    ld [romBank], a
    ld [GB_MBC5_ROMX_BANK_LO], a
    ret

