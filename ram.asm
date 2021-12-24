        .ORG RAMSTART
        
            DS DSIZE
stack:
dStack:        

            DS RSIZE
rStack:        

TIB:        DS TIBSIZE

            .align $100
mintVars:
sysVars:

vS0:        DS 2                ; a
vBase16:    DS 2                ; b
vTIBPtr:    DS 2                ; c
vDefs:      DS 2                ; d
vEdited:    DS 2                ; e
            DS 2                ; f
            DS 2                ; g
vHeapPtr:   DS 2                ; h
            DS 2                ; i
            DS 2                ; j
            DS 2                ; k
            DS 2                ; l  
            DS 2                ; m  
            DS 2                ; n
            DS 2                ; o
            DS 2                ; p
            DS 2                ; q
            DS 2                ; r     
            DS 2                ; s
            DS 2                ; t
            DS 2                ; u
            DS 2                ; v
            DS 2                ; w
            DS 2                ; x     
            DS 2                ; y
            DS 2                ; z

            DS 2                ; 
vByteMode:  DS 2                ; 
            DS $30
tbPtr:      DS 2                ; reserved for tests

RST08:      DS 2                 
RST10:      DS 2                 
RST18:      DS 2                 
RST20:      DS 2                 
RST28:      DS 2                 
RST30:      DS 2                ; 
BAUD        DS 2                ; 
INTVEC:     DS 2                ; 
NMIVEC:     DS 2                ; 
GETCVEC:    DS 2                ;   
PUTCVEC:    DS 2                ;   
; ****************************************************************
; VARS Table - holds 26 16-bit user variables
; ****************************************************************
vars:       DS 26 * 2

; ****************************************************************
; DEFS Table - holds 26 addresses of user routines
; ****************************************************************
            .align $40
            .org $-12
            DS 12               ; vars for group 0 
defs:       DS GRPSIZE * NUMGRPS

HEAP:         
