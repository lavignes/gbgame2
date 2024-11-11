; vim: ft=gbasm
\include "gb.inc"

\section "HOME"

SerialInit::
    xor a, a
    ldh [GB_SC], a
    ret

