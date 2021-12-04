.macro expect,msg1,val1
    POP HL
    PUSH HL
    LD DE,val1
    OR A
    SBC HL,DE
    LD A,L
    OR H
    JR Z,expect%%M
    CALL enter
    .cstr "`",msg1,"`\\N`Actual: `\\P\\N"
    LD HL,val1
    PUSH HL
    CALL enter
    .cstr "`Expected: `."
    HALT
    .cstr
expect%%M:
    POP HL
.endm

.macro test,code1,val1
    CALL enter
    .cstr code1
    expect code1,val1
.endm

