.macro setTBuf,s1
    ld HL,buf1%%M
    ld (tbPtr),HL
    jr buf2%%M
buf1%%M:
    db s1,$5C,"q",$0D
buf2%%M:
.endm

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
    .cstr "`",msg1,"`\\n"
    LD HL,val1
    PUSH HL
    CALL crlf
    CALL enter
    .cstr "`Expected `.` Actual `."
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

