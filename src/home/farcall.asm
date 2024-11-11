; vim: ft=gbasm
\include "gb.inc"

\section "HRAM"

;; Saved ROMX bank
romBank:: \space 1

\section "HOME"

;; Call function at e:hl
;;
;; NOTE Only use this to call functions that dont pass
;; args via registers or stack
FarCallHL::
    ldh a, [romBank]
    ; Is that bank already active?
    ; TODO should I instead just panic?
    cp a, e
    jr nz, .SwitchBanks
    jp hl
.SwitchBanks:
    push af ; push return bank
    ld a, e
    ldh [romBank], a
    ld [GB_MBC5_BANK_LO], a
    jp hl ; TODO dont we need to rst RstCall?
    ; restore bank and return
    pop af
    ld [romBank], a
    ld [GB_MBC5_BANK_LO], a
    ret

