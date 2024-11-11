; vim: ft=gbasm
\include "gb.inc"

\section "HOME"

TimerInit::
    xor a, a
    ldh [GB_TIMA], a
    ldh [GB_TMA], a
    ldh [GB_TAC], a
    ldh [GB_DIV], a
    ret

