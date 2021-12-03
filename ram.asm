        .ORG RAMSTART
        
            DS DSIZE
stack:
dStack:        

            DS RSIZE
rStack:        

TIB:        
            DS TIBSIZE

            .align $100
; ****************************************************************
; System constants
; ****************************************************************
sysConsts:

cS0:        DW 0                ; 0                        
cTIB        DW 0                ; 1     
cDefs:      DW 0                ; 2     
cVars:      DW 0                ; 3     
cOpcodes    DW 0                ; 4     
cmacros:    DW 0                ; 5     
cUserVars:  DW 0                ; 6     
            DW 0                ; 7     

; ****************************************************************
; USER variables
; ****************************************************************
userVars:

vAlt:       DW 0                ; a
vBase16:    DW 0                ; b
vTIBPtr:    DW 0                ; c
RST08:      DW 0                ; d
RST10:      DW 0                ; e
vFlags      DW 0                ; f
vByteMode:  DW 0                ; g
vHeapPtr:   DW 0                ; h

BAUD        DW 0                ; i
INTVEC:     DW 0                ; j
NMIVEC:     DW 0                ; k
GETCVEC:    DW 0                ; l  
PUTCVEC:    DW 0                ; m  
RST18:      DW 0                ; n
RST20:      DW 0                ; o
RST28:      DW 0                ; p
RST30:      DW 0                ; q
            DW 0                ; r     
            DW 0                ; s
            DW 0                ; t
            DW 0                ; u
            DW 0                ; v
            DW 0                ; w
vTemp:      DW 0                ; x     

; ****************************************************************
; VARS Table - holds 26 16-bit user variables
; ****************************************************************
vars:       DS 26 * 2

; ****************************************************************
; DEFS Table - holds 26 addresses of user routines
; ****************************************************************
defs:       DS 26 * 2

BUF:        DS $80

tbPtr:      DW 0                ; reserved for tests

            .align $40
HEAP:         
