        .ORG RAMSTART
        
            DS DSIZE
stack:
dStack:        

            DS RSIZE
rStack:        

TIB:        DS TIBSIZE
BUF:        DS $80

RST08:      DW 0                ; 
RST10:      DW 0                ; 
RST18:      DW 0                ; 
RST20:      DW 0                ; 
RST28:      DW 0                ; 
RST30:      DW 0                ; 
BAUD        DW 0                ; 
INTVEC:     DW 0                ; 
NMIVEC:     DW 0                ; 
GETCVEC:    DW 0                ;   
PUTCVEC:    DW 0                ;   
tbPtr:      DW 0                ; reserved for tests

            .align $100
mintVars:
sysVars:

vS0:        DW 0                ; a
vBase16:    DW 0                ; b
vTIBPtr:    DW 0                ; c
            DW 0                ; d
            DW 0                ; e
            DW 0                ; f
            DW 0                ; g
vHeapPtr:   DW 0                ; h
            DW 0                ; i
            DW 0                ; j
            DW 0                ; k
            DW 0                ; l  
            DW 0                ; m  
            DW 0                ; n
            DW 0                ; o
            DW 0                ; p
            DW 0                ; q
            DW 0                ; r     
            DW 0                ; s
            DW 0                ; t
            DW 0                ; u
            DW 0                ; v
            DW 0                ; w
            DW 0                ; x     
            DW 0                ; y
            DW 0                ; z

vIFTEMode   DW 0                ; 
vByteMode:  DW 0                ; 

; ****************************************************************
; VARS Table - holds 26 16-bit user variables
; ****************************************************************
vars:       DS 26 * 2

; ****************************************************************
; DEFS Table - holds 26 addresses of user routines
; ****************************************************************
defs:       DS 26 * 2

            .align $40
HEAP:         
