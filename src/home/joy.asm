; vim: ft=gbasm
\include "gb.inc"

\section "HRAM"

joyPressed::   \space 1
joyHeld::      \space 1
joyReleased::  \space 1

\section "HOME"

JoyUpdate::
    ; read directions
    ld a, (1 << GB_P1_BIT_READ_DIRECTION)
    ldh [GB_P1], a
    ldh a, [GB_P1]
    ldh a, [GB_P1]
    ; store bits in upper nibble
    cpl
    and a, $0F
    swap a
    ld l, a
    ; read buttons
    ld a, (1 << GB_P1_BIT_READ_ACTION)
    ldh [GB_P1], a
    ldh a, [GB_P1]
    ldh a, [GB_P1]
    ldh a, [GB_P1]
    ldh a, [GB_P1]
    ldh a, [GB_P1]
    ldh a, [GB_P1]
    ; store in lower nibble
    cpl
    and a, $0F
    or a, l
    ld l, a
    ; reset joypad
    ld a, (1 << GB_P1_BIT_READ_DIRECTION) | (1 << GB_P1_BIT_READ_ACTION)
    ldh [GB_P1], a
    ; Now we have the button states for this frame in `L`
    ; AND with the last frame to get the held buttons
    ldh a, [joyPressed]
    and a, l
    ldh [joyHeld], a
    ; AND with complement to get released buttons this frame
    ld a, l
    cpl
    ld h, a
    ldh a, [joyPressed]
    and a, h
    ldh [joyReleased], a
    ; Finally update the buttons for the current frame
    ; XOR with the current pressed so filter out held
    ldh a, [joyPressed]
    xor a, l
    ldh [joyPressed], a
    ret

