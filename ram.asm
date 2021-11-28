        .ORG RAMSTART
        
            DS DSIZE
stack:
dStack:        

            DS RSIZE
rStack:        

TIB:        
            DS TIBSIZE

; ****************************************************************
; USER variables
; ****************************************************************

userVars:
knownVars:

cS0:        DW 0                ; 0     \00                   
cTIB        DW 0                ; 1     \01
cDefs:      DW 0                ; 2     \02
cVars:      DW 0                ; 3     \03
cMacros:    DW 0                ; 4     \04
cUserVars:  DW 0                ; 5     \05
            DW 0                ; 6     \06
            DW 0                ; 7     \07
            DW 0                ; 8     \08
vTemp:      DW 0                ; 9     \09

vHeapPtr:   DW 0                ; 10
vBase16:    DW 0                ; 11
vTIBPtr:    DW 0                ; 12
vAltCodes:  DW 0                ; 13
vByteMode:  DW 0                ; 14
            DW 0                ; 15

GETCVEC:    DW 0                ; 16  
PUTCVEC:    DW 0                ; 17  
INTVEC:     DW 0                ; 18
NMIVEC:     DW 0                ; 19
BAUD        DW 0                ; 20
RST08:      DW 0                ; 21
RST10:      DW 0                ; 22
RST18:      DW 0                ; 23
RST20:      DW 0                ; 24
RST28:      DW 0                ; 25
RST30:      DW 0                ; 26
            DW 0                ; 27
            DW 0                ; 28
            DW 0                ; 29
            DW 0                ; 30
            DW 0                ; 31

BUF:        DS $80


; ****************************************************************
; Macros Table - holds $20 ctrl key macros
; ****************************************************************
macros:     DS $20 * 2

; ****************************************************************
; VARS Table - holds 26 16-bit user variables
; ****************************************************************
vars:       DS 26 * 2

; ****************************************************************
; DEFS Table - holds 26 addresses of user routines
; ****************************************************************
defs:       DS 26 * 2


tbPtr:      DW 0                ; reserved for tests

HEAP:         
