; vim: ft=gbasm
\section "HEADER"

\macro TITLE
    \byte \1
    \if \len \1 > 11
        \fail "game title too long"
    \end
    \loop N, 11 - \len \1
        \byte $00
    \end
\end

PostBoot:
    nop
    jp Start

Header:
    ; tendy logo
    \byte $CE, $ED, $66, $66, $CC, $0D, $00, $0B
    \byte $03, $73, $00, $83, $00, $0C, $00, $0D
    \byte $00, $08, $11, $1F, $88, $89, $00, $0E
    \byte $DC, $CC, $6E, $E6, $DD, $DD, $D9, $99
    \byte $BB, $BB, $67, $63, $6E, $0E, $EC, $CC
    \byte $DD, $DC, $99, $9F, $BB, $B9, $33, $3E

    TITLE "GAME!"
    \byte "BASM"
    \byte $C0   ; GBC-only
    \word $0000 ; License code
    \byte $00   ; No SGB
    \byte $1B   ; MBC type: RAM+BAT
    \byte $07   ; ROM: 4096KiB
    \byte $04   ; SRAM: 128KiB
    \byte $01   ; Not Japan

