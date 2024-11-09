; vim: ft=gbasm
\include "gb.inc"

\section "WRAM0"
stackTop: \space 256
stackBase:

\section "HOME"
Start::
    di
    ld sp, stackBase
    call DoubleSpeedMode
    jr **

DoubleSpeedMode:
    ret
