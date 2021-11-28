; *************************************************************************
;
;        MINT1_18 Micro-Interpreter for the Z80
;
;        Ken Boak John Hardy and Craig Jones  November 25th 2021
;
;        Comparison Operators < and > return 0 (false) when equality is detected
;        Printhex routine shortened
;        Getchar and Putchar hooks into Small Computer Monitor added
;
;        Decimal entry bug fixed  24/11
;        Division routine shortened by 13 bytes 24/11
;
;
;        Includes serial routines getchar and putchar
;        printstring
;        printdec
;        printhex
;        crlf
;
;        Register Assignment:
;
;        BC is the instruction pointer IP
;        DE is a working register and 2nd on stack NOS
;        HL is a working register and Top of stack TOS
;        SP is data stack pointer
;        IX is used to implement the return stack
;        IY is used as a jump back to NEXT
;
;        All commands accessed via a byte wide look up table
;
;        Heap used for command storage (HERE)
;
;        Primitives are on two consecutive pages using a trampoline jump to the 2nd page.
;        
;        This allows single byte opcodes reducing the dispatch time from
;        64 t states to 33 t states
;
;
;        User defined commands and variables
;
;        User Commands  A-Z
;        User Variables a-z
;
;        Commands now available:
;
;        Maths
;
;        +     ADD
;		 -     SUB
;        *     MUL     (max product 65535)
;        /     DIV     Returns quotient and remainder
;        _     NEG
;
;        }     Shift Right (2/)
;        {     Shift Left  (2*)
;
;        Comparison - compare the top two elements on the stack
;        Puts 1 on the stack if condition is true, 0 if false
;
;        <     LT
;        =     EQ
;        >     GT
;
;        Logic
;
;        &     AND
;        |     OR
;        ^     XOR
;        ~     INV
;
;        Stack
;
;        "     DUP
;        '     DROP
;        $     SWAP
;		 %     OVER
;        .     DOT     (Print the value of the top of stack as a decimal)
;        ,     COMMA   (Print the value of the top of stack as a hexadecimal)
;		 #     HEX     Accept a hexadecimal number 
;
;        Memory
;
;        @     FETCH
;        !     STORE
;
;        User Definitions
;
;        :     Start a user definition
;        ;     End a user definition
;
;        \     QUIT    (Print OK and return to monitor)
;
;
;        Loops    - execute the code between parenthesis
;
;        The user variable i is used as the loop counter
;        It is decremented every time the loop is executed
;
;        10(repeat this code 10 times)
;
;        0(skip this code)
;
;        1(execute this code only once)
;
;        a@ b@ = (_print this if a=b_)
;
;       1000(i@.)    Print out the value of i from 999 to 0
;
;       10(a@ 1+ a! a@ .)  Increment a 10 times and print it out
;
;       User Commands are allocated to uppercase alpha characters A to Z
;
;       A user command can be defined by starting with a colon and
;       ending with a semicolon
;
;       Example  :A 123 456 + . ;
;
;       The A character represents a fixed address for the User routine
;       The interpreter copies all the characters after the A to a text buffer
;       located at address A
;       Each time A is encountered (outside of a colon definition)
;       it will execute the code  located there i.e. 123 456 + .
;
;       Variables are associated with lowercase characters a-z
;       Each variable is allocated 2 bytes located on even addresses
;       They run contiguously from $A800 (a) to $A830 (z)
;       They are accessed using the fetch and store commands @ and !
;
;       Examples:
;
;       1234 a!     store 1234 in a
;
;       b@ .        fetch the value from b and print it out
;
;       a@ b@ + .   fetch values from a and b, add them together and print the sum
;
;       a@ b!       copy the value in a and store it in b
;
;
; *****************************************************************************

;       vFLAGS has up to 16 flags    
;       fEDIT       EQU $0001


         ;ROMSTART    EQU $0
         ;RAMSTART    EQU $800
         ;EXTENDED    EQU 0

        ;ROMSIZE     EQU $800
        DSIZE       EQU $100
        RSIZE       EQU $100
        TIBSIZE     EQU $100
        TRUE        EQU 1
        FALSE       EQU 0

;        .ORG ROMSTART
		
; **************************************************************************
; Page 0  Initialisation
; **************************************************************************		
;        JP start

		.ORG ROMSTART + $180		

; **************************************************************************
; Macros must be written in Mint and end with ; 
; this code must not span pages
; **************************************************************************
iMacros:

.include "MINT-macros.asm"

start:
mint:
        LD SP,DSTACK
        CALL initialize
        call ENTER
        .cstr "`MINT V1.0`\\n"
        JP interpret

initialize:
        LD IX,RSTACK
        LD IY,NEXT			    ; IY provides a faster jump to NEXT
        LD HL,iUserVars
        LD DE,userVars
        LD BC,16 * 2
        LDIR
        LD HL,defs
        LD B,26
init1:
        LD (HL),lsb(empty_)
        INC HL
        LD (HL),msb(empty_)
        INC HL
        DJNZ init1
        LD B,$20
        LD DE,ctrlCodes
        LD HL,macros
init2:
        LD A,(DE)
        INC DE
        LD (HL),A
        INC HL
        LD (HL),msb(iMacros)
        INC HL
        DJNZ init2
        RET

interpret:
        call ENTER
        .cstr "\\n`> `"

interpret1:                     ; used by tests
        LD BC,0                 ; load BC with offset into TIB         
        LD (vTIBPtr),BC

interpret2:                     ; calc nesting (a macro might have changed it)
        LD E,0                  ; initilize nesting value
        PUSH BC                 ; save offset into TIB, 
                                ; BC is also the count of chars in TIB
        LD HL,TIB               ; HL is start of TIB
        JR interpret4

interpret3:
        LD A,(HL)               ; A = char in TIB
        INC HL                  ; inc pointer into TIB
        DEC BC                  ; dec count of chars in TIB
        call nesting            ; update nesting value

interpret4:
        LD A,C                  ; is count zero?
        OR B
        JR NZ, interpret3          ; if not loop
        POP BC                  ; restore offset into TIB
; *******************************************************************         
; Wait for a character from the serial input (keyboard) 
; and store it in the text buffer. Keep accepting characters,
; increasing the instruction pointer BC - until a newline received.
; *******************************************************************

waitchar:   
        CALL getchar            ; loop around waiting for character
        CP $20
        JR NC, waitchar1
        CP $0                   ; is it end of string?
        JR Z, endchar
        CP '\r'                 ; carriage return?
        JR Z, waitchar3
        LD D,0
        JP macro    

waitchar1:
        LD HL,TIB
        ADD HL,BC
        LD (HL),A               ; store the character in textbuf
        INC BC
        CALL putchar            ; echo character to screen
        CALL nesting
        JR  waitchar            ; wait for next character

waitchar3:
        LD HL,TIB
        ADD HL,BC
        LD (HL),"\r"            ; store the crlf in textbuf
        INC BC
        CALL crlf               ; echo character to screen
        LD A,E                  ; if zero nesting append and ETX after \r
        OR A
        JR NZ,waitchar
        LD (HL),$03             ; store end of text ETX in text buffer 
        INC BC

endchar:    
        LD (vTIBPtr),BC
        ; CALL crlf
        LD BC,TIB               ; Instructions stored on heap at address HERE
        DEC BC
                                ; Drop into the NEXT and dispatch routines

; ********************************************************************************
;
; Dispatch Routine.
;
; Get the next character and form a 1 byte jump address
;
; This target jump address is loaded into HL, and using JP (HL) to quickly 
; jump to the selected function.
;
; Individual handler routines will deal with each category:
;
; 1. Detect characters A-Z and jump to the User Command handler routine
;
; 2. Detect characters a-z and jump to the variable handler routine
;
; 3. All other characters are punctuation and cause a jump to the associated
; primitive code.
;
; Instruction Pointer IP BC is incremented
;
; *********************************************************************************

NEXT:   
        INC BC                      ; 6t    Increment the IP
        LD A, (BC)                  ; 7t    Get the next character and dispatch
		
dispatch:                        
        CP 0                        ;       NULL? exit Mint
        JP Z, exit_
        CP $03                      ;       ETX? interpret next line       
        JP Z,interpret
        SUB ' '                     ; 7t    remove char offset
        JR C,NEXT                   ;       ignore char 
        LD DE,opcodes               ; 7t    Start address of jump table         
        LD E,A                      ; 4t    Index into table
        LD A,(DE)                   ; 7t    get low jump address
        LD H,msb(page1)             ; 7t    Load H with the 1st page address
        LD L,A                      ; 4t    and put into L
        JP (HL)                     ; 4t    Jump to routine

ENTER:
        LD HL,BC
        CALL rpush              ; save Instruction Pointer
        POP BC
        DEC BC
        JP  (IY)                ; Execute code from User def

printdec:

;Number in hl to decimal ASCII

;inputs:	hl = number to ASCII
;example: hl=300 outputs '00300'
;destroys: af, de, hl
DispHL:
        ld	de,-10000
        call	Num1
        ld	de,-1000
        call	Num1
        ld	de,-100
        call	Num1
        ld	e,-10
        call	Num1
        ld	e,-1
Num1:	    
        ld	a,'0'-1
Num2:	    
        inc	a
        add	hl,de
        jr	c,Num2
        sbc	hl,de
        JP putchar

rpush:
        DEC IX                  
        LD (IX+0),H
        DEC IX
        LD (IX+0),L
        RET

rpop:
        LD L,(IX+0)         
        INC IX              
        LD H,(IX+0)
        INC IX                  
        RET

; crlf:       
;         LD A, '\r'
;         CALL putchar
;         LD A, '\n'           
;         JP putchar
        
; **************************************************************************
; Page 2  Jump Tables
; **************************************************************************
        .align $100
opcodes:
        DB    lsb(nop_)    ;    SP
        DB    lsb(store_)  ;    !            
        DB    lsb(dup_)    ;    "
        DB    lsb(hex_)    ;    #
        DB    lsb(swap_)   ;    $            
        DB    lsb(over_)   ;    %            
        DB    lsb(and_)    ;    &
        DB    lsb(drop_)   ;    '
        DB    lsb(begin_)  ;    (        
        DB    lsb(again_)  ;    )
        DB    lsb(mul_)    ;    *            
        DB    lsb(add_)    ;    +
        DB    lsb(hexp_)   ;    ,            
        DB    lsb(sub_)    ;    -
        DB    lsb(dot_)    ;    .
        DB    lsb(div_)    ;    /
        DB    lsb(num_)    ;    0            
        DB    lsb(num_)    ;    1        
        DB    lsb(num_)    ;    2            
        DB    lsb(num_)    ;    3
        DB    lsb(num_)    ;    4            
        DB    lsb(num_)    ;    5            
        DB    lsb(num_)    ;    6            
        DB    lsb(num_)    ;    7
        DB    lsb(num_)    ;    8            
        DB    lsb(num_)    ;    9        
        DB    lsb(def_)    ;    :        
        DB    lsb(ret_)    ;    ;
        DB    lsb(lt_)     ;    <
        DB    lsb(eq_)     ;    =            
        DB    lsb(gt_)     ;    >            
        DB    lsb(query_)  ;    ?
        DB    lsb(fetch_)  ;    @    
        DB    lsb(call_)   ;    A    
        DB    lsb(call_)   ;    B
        DB    lsb(call_)   ;    C
        DB    lsb(call_)   ;    D    
        DB    lsb(call_)   ;    E
        DB    lsb(call_)   ;    F
        DB    lsb(call_)   ;    G
        DB    lsb(call_)   ;    H
        DB    lsb(call_)   ;    I
        DB    lsb(call_)   ;    J
        DB    lsb(call_)   ;    K
        DB    lsb(call_)   ;    L
        DB    lsb(call_)   ;    M
        DB    lsb(call_)   ;    N
        DB    lsb(call_)   ;    O
        DB    lsb(call_)   ;    P
        DB    lsb(call_)   ;    Q
        DB    lsb(call_)   ;    R
        DB    lsb(call_)   ;    S
        DB    lsb(call_)   ;    T
        DB    lsb(call_)   ;    U
        DB    lsb(call_)   ;    V
        DB    lsb(call_)   ;    W
        DB    lsb(call_)   ;    X
        DB    lsb(call_)   ;    Y
        DB    lsb(call_)   ;    Z
        DB    lsb(arrDef_) ;    [
        DB    lsb(alt_)    ;    \
        DB    lsb(arrEnd_) ;    ]
        DB    lsb(xor_)    ;    ^
        DB    lsb(neg_)    ;    _
        DB    lsb(str_)    ;    `            
        DB    lsb(var_)    ;    a
        DB    lsb(var_)    ;    b
        DB    lsb(var_)    ;    c
        DB    lsb(var_)    ;    d
        DB    lsb(var_)    ;    e
        DB    lsb(var_)    ;    f
        DB    lsb(var_)    ;    g
        DB    lsb(var_)    ;    h
        DB    lsb(var_)    ;    i            
        DB    lsb(var_)    ;    j
        DB    lsb(var_)    ;    k
        DB    lsb(var_)    ;    l
        DB    lsb(var_)    ;    m
        DB    lsb(var_)    ;    n
        DB    lsb(var_)    ;    o
        DB    lsb(var_)    ;    p
        DB    lsb(var_)    ;    q            
        DB    lsb(var_)    ;    r
        DB    lsb(var_)    ;    s    
        DB    lsb(var_)    ;    t
        DB    lsb(var_)    ;    u
        DB    lsb(var_)    ;    v
        DB    lsb(var_)    ;    w
        DB    lsb(var_)    ;    x
        DB    lsb(var_)    ;    y
        DB    lsb(var_)    ;    z
        DB    lsb(shl_)    ;    {
        DB    lsb(or_)     ;    |            
        DB    lsb(shr_)    ;    }            
        DB    lsb(inv_)    ;    ~            
        DB    lsb(del_)    ;    backspace
		
; ***********************************************************************
; Alternate function codes		
; ***********************************************************************		
ctrlCodes:
altCodes:
        DB     lsb(empty_)     ; NUL ^@
        DB     lsb(empty_)     ; SOH ^A
        DB     lsb(toggleBase_); STX ^B
        DB     lsb(empty_)     ; ETX ^C
        DB     lsb(empty_)     ; EOT ^D
        DB     lsb(editt_)     ; ENQ ^E
        DB     lsb(empty_)     ; ACK ^F
        DB     lsb(empty_)     ; BEL ^G
        DB     lsb(backsp_)    ; BS  ^H
        DB     lsb(empty_)     ; TAB ^I
        DB     lsb(empty_)     ; LF  ^J
        DB     lsb(empty_)     ; VT  ^K
        DB     lsb(empty_)     ; FF  ^L
        DB     lsb(empty_)     ; CR  ^M
        DB     lsb(empty_)     ; SO  ^N
        DB     lsb(empty_)     ; SI  ^O
        DB     lsb(printStack_); DLE ^P
        DB     lsb(empty_)     ; DC1 ^Q
        DB     lsb(empty_)     ; DC2 ^R
        DB     lsb(empty_)     ; DC3 ^S
        DB     lsb(empty_)     ; DC4 ^T
        DB     lsb(empty_)     ; NAK ^U
        DB     lsb(empty_)     ; SYN ^V
        DB     lsb(empty_)     ; ETB ^W
        DB     lsb(empty_)     ; CAN ^X
        DB     lsb(empty_)     ; EM  ^Y
        DB     lsb(empty_)     ; SUB ^Z
        DB     lsb(escape_)    ; ESC ^[
        DB     lsb(empty_)     ; FS  ^\
        DB     lsb(empty_)     ; GS  ^]
        DB     lsb(empty_)     ; RS  ^^
        DB     lsb(empty_)     ; US  ^_)
        DB     lsb(nop_)       ; SP  ^`
        DB     lsb(cStore_)    ;    !            
        DB     lsb(nop_)       ;    "
        DB     lsb(nop_)       ;    
        DB     lsb(TIBPtr_)    ;    $  ( -- adr ) text input ptr           
        DB     lsb(nop_)       ;    %            
        DB     lsb(nop_)       ;    &
        DB     lsb(nop_)       ;    '
        DB     lsb(nop_)       ;    (        
        DB     lsb(nop_)       ;    )
        DB     lsb(nop_)       ;    *            
        DB     lsb(incr_)      ;    +  ( adr -- ) decrements variable at address
        DB     lsb(nop_)       ;    ,            
        DB     lsb(decr_)      ;    -  ( adr -- ) increments variable at address
        DB     lsb(nop_)       ;    .  
        DB     lsb(nop_)       ;    /
        DB     lsb(knownVar_)  ;    0  ( -- adr ) start of data stack constant         
        DB     lsb(knownVar_)  ;    1  ; returns HERE variable
        DB     lsb(knownVar_)  ;    2  ( -- adr ) TIBPtr variable          
        DB     lsb(knownVar_)  ;    3  ( -- adr ) isHex variable
        DB     lsb(knownVar_)  ;    4            
        DB     lsb(knownVar_)  ;    5            
        DB     lsb(knownVar_)  ;    6            
        DB     lsb(knownVar_)  ;    7
        DB     lsb(knownVar_)  ;    8            
        DB     lsb(knownVar_)  ;    9        
        DB     lsb(adef_)      ;    :  start defining a macro        
        DB     lsb(nop_)       ;    ;  
        DB     lsb(nop_)       ;    <
        DB     lsb(nop_)       ;    =            
        DB     lsb(nop_)       ;    >            
        DB     lsb(nop_)       ;    ?
        DB     lsb(cFetch_)    ;    @      
        DB     lsb(nop_)       ;    A    
        DB     lsb(nop_)       ;    B
        DB     lsb(nop_)       ;    C
        DB     lsb(nop_)       ;    D    
        DB     lsb(edit_)      ;    E  
        DB     lsb(nop_)       ;    F
        DB     lsb(nop_)       ;    G
        DB     lsb(nop_)       ;    H  
        DB     lsb(inPort_)    ;    I  ( port -- val )   
        DB     lsb(nop_)       ;    J
        DB     lsb(nop_)       ;    K  
        DB     lsb(nop_)       ;    L
        DB     lsb(max_)       ;    M  ( a b -- c ) return the maximum value
        DB     lsb(nop_)       ;    N
        DB     lsb(outPort_)   ;    O  ( val port -- )
        DB     lsb(nop_)       ;    P
        DB     lsb(nop_)       ;    Q
        DB     lsb(nop_)       ;    R
        DB     lsb(nop_)       ;    S
        DB     lsb(nop_)       ;    T
        DB     lsb(nop_)       ;    U
        DB     lsb(nop_)       ;    V
        DB     lsb(nop_)       ;    W
        DB     lsb(exec_)      ;    X
        DB     lsb(nop_)       ;    Y
        DB     lsb(nop_)       ;    Z
        DB     lsb(cArrDef_)   ;    [
        DB     lsb(comment_)   ;    \  comment text, skips reading until end of line
        DB     lsb(cArrEnd_)   ;    ]
        DB     lsb(charCode_)  ;    ^
        DB     lsb(sign_)      ;    _)  ( n -- b ) returns true if -ve 
        DB     lsb(strDef_)    ;    `            
        DB     lsb(nop_)       ;    a
        DB     lsb(base16_)    ;    b
        DB     lsb(nop_)       ;    c
        DB     lsb(depth_)     ;    d  ( -- val ) depth of data stack
        DB     lsb(emit_)      ;    e  ( val -- ) emits a char to output
        DB     lsb(nop_)       ;    f
        DB     lsb(go_)        ;    g  ( -- ? ) execute mint definition
        DB     lsb(heapPtr_)   ;    h  ; returns heap ptr variable
        DB     lsb(i_)         ;    i  ; returns index variable of current loop          
        DB     lsb(j_)         ;    j  ; returns index variable of outer loop
        DB     lsb(key_)       ;    k  ( -- val )  read a char from input
        DB     lsb(nop_)       ;    l
        DB     lsb(min_)       ;    m  ( a b -- c ) return the minimum value
        DB     lsb(newln_)     ;    n  ; prints a newline to output
        DB     lsb(nop_)       ;    o
        DB     lsb(dots_)      ;    p  ( -- ) non-destructively prints stack
        DB     lsb(quit_)      ;    q  ; quits from Mint REPL         
        DB     lsb(nop_)       ;    r
        DB     lsb(nop_)       ;    s 
        DB     lsb(type_)      ;    t
        DB     lsb(userVar_)   ;    u
        DB     lsb(nop_)       ;    v   
        DB     lsb(while_)     ;    w  ; ( b -- ) if false, skip to end of loop 
        DB     lsb(exec_)      ;    x
        DB     lsb(nop_)       ;    y
        DB     lsb(nop_)       ;    z
        DB     lsb(nop_)       ;    {
        DB     lsb(nop_)       ;    |            
        DB     lsb(nop_)       ;    }            
        DB     lsb(nop_)       ;    ~           
        DB     lsb(nop_)       ;    BS		

iUserVars:
        DW dStack               ; \0 cS0
        DW TIB                  ; \1 cTIB
        DW defs                 ; \2 cDefs
        DW vars                 ; \3 cVars
        DW macros               ; \4 cMacros
        DW userVars             ; \5 cUserVars
        DW 0                    ; \6 
        DW 0                    ; \7 
        DW 0                    ; \8 
        DW 0                    ; \9 vTemp

        DW HEAP                 ; vHeapPtr
        DW FALSE                ; vBase16
        DW TIB                  ; vTIBPtr
        DW altCodes             ; vAltCodes
        DW $0                   ; 
        DW $0                   ;
        

; **********************************************************************			 
; Page 4 primitive routines 
; **********************************************************************
        .align $100
page1:

alt_:        
        JP alt
exit_:
        INC BC
        LD DE,BC                
        CALL rpop               ; Restore Instruction pointer
        LD BC,HL
        EX DE,HL
        JP (HL)
        
num_:   
        JP  number

call_:
        LD HL,BC
        CALL rpush              ; save Instruction Pointer
        LD A,(BC)
        SUB "A"                 ; Calc index
        ADD A,A
        LD HL,DEFS
        LD E,A
        LD D,0
        ADD HL,DE
        LD C,(HL)
        INC HL
        LD B,(HL)
        DEC BC
        JP  (IY)                ; Execute code from User def

ret_:
        CALL rpop               ; Restore Instruction pointer
        LD BC,HL                
        JP (IY)             

var_:
        LD A,(BC)
        SUB "a"                 ; Calc index
        ADD A,A
        LD HL,VARS
        LD E,A
        LD D,0
        ADD HL,DE
        PUSH HL
        JP (IY)
        
fetch_:                     ; Fetch the value from the address placed on the top of the stack      
        POP     HL          ; 10t
        LD      E,(HL)      ; 7t
        INC     HL          ; 6t
        LD      D,(HL)      ; 7t
        PUSH    DE          ; 11t
        JP      (IY)        ; 8t
                            ; 49t 
        
store_:                     ; Store the value at the address placed on the top of the stack
        POP    HL           ; 10t
        POP    DE           ; 10t
        LD     (HL),E       ; 7t
        INC    HL           ; 6t
        LD     (HL),D       ; 7t
        JP     (IY)         ; 8t
                            ; 48t
    
dup_:        
        POP     HL          ; Duplicate the top member of the stack
        PUSH    HL
        PUSH    HL
        JP (IY)

; $ swap                    ; a b -- b a Swap the top 2 elements of the stack
swap_:        
        POP HL
        EX (SP),HL
        PUSH HL
        JP (IY)
        
over_:  
        POP     HL          ; Duplicate 2nd element of the stack
        POP     DE
        PUSH    DE
        PUSH    HL
        PUSH    DE          ; And push it to top of stack
        JP (IY)        
    
drop_:                      ; Discard the top member of the stack
        POP     HL
        JP      (IY)

and_:        
        POP     DE          ; 10t Bitwise AND the top 2 elements of the stack
        POP     HL          ; 10t
        LD      A,E         ; 4t
        AND     L           ; 4t
        LD      L,A         ; 4t
        LD      A,D         ; 4t
        AND     H           ; 4t
        LD      H,A         ; 4t
        PUSH    HL          ; 11t
        JP      (IY)        ; 8t
        
                            ; 63t
    
    
or_: 		 
        POP     DE             ; Bitwise OR the top 2 elements of the stack
        POP     HL
        LD      A,E
        OR      L
        LD      L,A
        LD      A,D
        OR      H
        LD      H,A
        PUSH    HL
        JP      (IY)
    
    
xor_:		 
        POP     DE            ; Bitwise XOR the top 2 elements of the stack
xor1:
        POP     HL
        LD      A,E
        XOR     L
        LD      L,A
        LD      A,D
        XOR     H
        LD      H,A
        PUSH    HL
        JP      (IY)

inv_:						    ; Bitwise INVert the top member of the stack
        LD DE, $FFFF            ; by xoring with $FFFF
        JR xor1        
   
add_:                          ; Add the top 2 members of the stack
        POP     DE             ; 10t
        POP     HL             ; 10t
        ADD     HL,DE          ; 11t
        PUSH    HL             ; 11t
        JP      (IY)           ; 8t
                               ; 50t

;  Left shift { is multply by 2		
shl_:   
        POP HL                  ; Duplicate the top member of the stack
        ADD HL,HL
        PUSH HL                 ; shift left fallthrough into add_     
        JP (IY)                 ; 8t
    

;  Right shift } is a divide by 2		
		
shr_:    
        POP HL                  ; Get the top member of the stack
        SRL H
        RR L
        PUSH HL
        JP (IY)                 ; 8t

neg_:   LD HL, 0    		    ; NEGate the value on top of stack (2's complement)
        POP DE                  ; 10t
        JR SUB_2                ; use the SUBtract routine
    
sub_:       				    ; Subtract the value 2nd on stack from top of stack 
        
        POP     DE              ; 10t
sub_1:  POP     HL              ; 10t  Entry point for INVert
sub_2:  AND     A               ;  4t  Entry point for NEGate
        SBC     HL,DE           ; 15t
        PUSH    HL              ; 11t
        JP      (IY)            ; 8t
                                ; 58t
eq_:    POP      HL
        POP      DE
        AND      A              ; reset the carry flag
        SBC      HL,DE          ; only equality sets HL=0 here
        JR       Z, equal
        LD       HL, 0
        JR       less           ; HL = 1    

       
gt_:    POP      DE
        POP      HL
        JR       cmp_
        
lt_:    POP      HL
        POP      DE
cmp_:   AND      A              ; reset the carry flag
        SBC      HL,DE          ; only equality sets HL=0 here
		JR       Z, less        ; equality returns 0  KB 25/11/21
        LD       HL, 0
        JP       M, less
equal:  INC      L              ; HL = 1    
less:     
        PUSH     HL
        JP       (IY) 
        
hex_:   CALL     get_hex
        JR       less           ; piggyback for ending

dot_:       
        POP HL
        LD A,(vBase16)
        OR A
        JR Z,dot1
        CALL printhex
        JR dot2
dot1:
        CALL printdec
dot2:
        CALL space
        JP (IY)

str_:       JP str
hexp_:      JP hexp                 ; print hexadecimal
query_:     JR query
del_:       JR del
mul_:       JR mul
div_:       JR div
def_:       JP def
begin_:     JR begin                   
again_:     JP again
arrDef_:    JP arrDef
arrEnd_:    JP arrEnd
nop_:       JP NEXT                 ; hardwire white space to always go to NEXT (important for arrays)
quit_:      RET                     ; exit interpreter

;*******************************************************************
; Page 5 primitive routines 
;*******************************************************************
        .align   $100           

query:
        JP       (IY) 
                
del:      
        JP       (IY) 
        
mul:                       ; 16-bit multiply  

        POP  DE             ; get first value
        POP  HL
        PUSH BC             ; Preserve the IP
        LD B,H              ; BC = 2nd value
        LD C,L
        
        LD HL,0
        LD A,16
Mul_Loop_1:
        ADD HL,HL
        RL E
        RL D
        JR NC,$+6
        ADD HL,BC
        JR NC,$+3
        INC DE
        DEC A
        JR NZ,Mul_Loop_1
		
		JR   mul_end

; ********************************************************************
; 16-bit division subroutine.
;
; BC: divisor, DE: dividend, HL: remainder

; *********************************************************************            
; This divides DE by BC, storing the result in DE, remainder in HL
; *********************************************************************

; 1382 cycles
; 35 bytes (reduced from 48)
		

div:  

        POP  DE             ; get first value
        POP  HL             ; get 2nd value
        PUSH BC             ; Preserve the IP
        LD B,H              ; BC = 2nd value
        LD C,L		
		
        ld hl,0    	        ; Zero the remainder
        ld a,16    	        ; Loop counter

div_loop:		            ;shift the bits from BC (numerator) into HL (accumulator)
        sla c
        rl b
        adc hl,hl

        sbc hl,de			;Check if remainder >= denominator (HL>=DE)
        jr c,div_adjust
        inc c
        jr div_done

div_adjust:		            ; remainder is not >= denominator, so we have to add DE back to HL
        add hl,de

div_done:
        dec a
        jr nz,div_loop
        
        LD D,B              ; Result from BC to DE
        LD E,C
        
        EX DE,HL            ; swap them over?
			
mul_end:
div_end:    
        POP  BC             ; Restore the IP
   
        PUSH DE             ; Push Result
        PUSH HL             ; Push remainder             

        JP       (IY)
        
        
; *************************************
; Loop Handling Code
; *************************************
        	        
begin:                      ; Left parentesis begins a loop
        POP HL
        LD A,L              ; zero?
        OR H
        JR Z,begin1
        
        DEC HL
        LD DE,-6
        ADD IX,DE
        LD (IX+0),0                 ; loop var
        LD (IX+1),0                 
        LD (IX+2),L                 ; loop limit
        LD (IX+3),H                 
        LD (IX+4),C                 ; loop address
        LD (IX+5),B                 

        JP (IY)
begin1:
        LD E,1
begin2:
        INC BC
        LD A,(BC)
        CALL nesting
;         CP '`'
;         JR NZ,begin3
;         LD A,$80
;         XOR E
;         LD E,A
;         JR begin2
; begin3:
;         CP '['
;         JR Z,begin4
;         CP '('
;         JR NZ,begin5
; begin4:
;         INC E
;         JR begin2
; begin5:
;         CP ']'
;         JR Z,begin6
;         CP ')'
;         JR NZ,begin2
; begin6:
;         DEC E
        XOR A
        OR E
        JR NZ,begin2
        JP (IY)

; ********************************************************************************
; Number Handling Routine - converts numeric ascii string to a 16-bit number in HL
; Read the first character. 
;			
; Number characters ($30 to $39) are converted to digits by subtracting $30
; and then added into the L register. (HL forms a 16-bit accumulator)
; Fetch the next character, if it is a number, multiply contents of HL by 10
; and then add in the next digit. Repeat this until a non-number character is 
; detected. Add in the final digit so that HL contains the converted number.
; Push HL onto the stack and proceed to the dispatch routine.
; ********************************************************************************
         
								 
number:
		LD HL,$0000				; 10t Clear HL to accept the number
		LD A,(BC)				; 7t  Get the character which is a numeral
        
number1:                        ; corrected KB 24/11/21

        SUB $30                 ; 7t    Form decimal digit
        ADD A,L                 ; 4t    Add into bottom of HL
        LD  L,A                 ; 4t
        LD A,00                 ; 4t    Clear A
        ADC	A,H	                ; Add with carry H-reg
	    LD	H,A	                ; Put result in H-reg
      
        INC BC                  ; 6t    Increment IP
        LD A, (BC)              ; 7t    and get the next character
        CP $30                  ; 7t    Less than $30
        JR C, endnum            ; 7/12t Not a number / end of number
        CP $3A                  ; 7t    Greater or equal to $3A
        JR NC, endnum           ; 7/12t Not a number / end of number
       
times10:                        ; Multiply digit(s) in HL by 10
        ADD HL,HL               ; 11t    2X
        LD  E,L                 ;  4t    LD DE,HL
        LD  D,H                 ;  4t
        ADD HL,HL               ; 11t    4X
        ADD HL,HL               ; 11t    8X
        ADD HL,DE               ; 11t    2X  + 8X  = 10X
                                ; 52t cycles

        JP  number1
                
endnum:
        PUSH HL                 ; 11t   Put the number on the stack
        DEC BC
        JP (IY)                 ; and process the next character

again:
        LD E,(IX+0)                 ; peek loop var
        LD D,(IX+1)                 
        LD L,(IX+2)                 ; peek loop limit
        LD H,(IX+3)                 
        OR A
        SBC HL,DE
        JR Z,again1
        INC DE
        LD (IX+0),E                 ; poke loop var
        LD (IX+1),D                 
        LD C,(IX+4)                 ; peek loop address
        LD B,(IX+5)                 
        JP (IY)
again1:   
        LD DE,6                     ; drop loop frame
        ADD IX,DE
        JP (IY)

alt:
        INC BC
        LD A,(BC)
        LD HL,(vAltCodes)
        ADD A,L
        LD L,A
        LD L,(HL)           ; 7t    get low jump address
        LD H, msb(page5)    ; Load H with the 5th page address
        JP  (HL)                    ; 4t    Jump to routine
        
; **************************************************************************             
; Print the string between the `backticks`

str:                       
        INC BC
        
nextchar:            
        LD A, (BC)
        INC BC
        CP "`"              ; ` is the string terminator
        JR Z,stringend
        CALL putchar
        JR   nextchar

stringend:  
        DEC BC
        JP   (IY) 

hexp:                       ; Print HL as a hexadecimal
        POP     HL
        CALL    printhex
        CALL    space
        JP      (IY)

; Miscellaneous ************************************************************           

space:       
        LD A, ' '           
        JP putchar

crlf:       
        LD A, '\r'
        CALL putchar
        LD A, '\n'           
        JP putchar

; Byte ARRAY compilation routine ***********************************************
ccompNEXT:
        POP DE          ; DE = return address
        LD HL,(vHeapPtr)    ; load heap ptr
        LD (HL),E       ; store lsb
        INC HL          
        LD (vHeapPtr),HL    ; save heap ptr
        JP NEXT

; **************************************************************************
; Page 6 Alt primitives
; **************************************************************************
        .align $100
page5:
aDef_:
        PUSH HL             ; Save HL
        LD HL, MACROS       ; Start address of jump table         
        INC BC
        LD  A,(BC)          ; Get the next character
        INC BC
        SUB "@"             ; Calc index
        JP def1

base16_:
        LD HL,vBase16
        PUSH HL
        JP (IY)

cFetch_:
        POP     HL          ; 10t
        LD      E,(HL)      ; 7t
        LD      D,0         ; 7t
        PUSH    DE          ; 11t
        JP      (IY)        ; 8t
                            ; 49t 
charCode_:
        INC BC
        LD A,(BC)
        LD H,0
        LD L,A
        PUSH HL
        JP (IY)

comment_:
comment:
        INC BC              ; point to next char
        LD A,(BC)
        CP "\r"             ; terminate at newline 
        JR NZ,comment
        DEC BC
        JP   (IY) 

cStore_:	  
        POP    HL           ; 10t
        POP    DE           ; 10t
        LD     (HL),E       ; 7t
        JP     (IY)         ; 8t
                            ; 48t
        
depth_:
        LD HL,0
        ADD HL,SP
        EX DE,HL
        LD HL,DSTACK
        OR A
        SBC HL,DE
        SRL H
        RR L
        PUSH HL
        JP (IY)

emit_:
        POP HL
        LD A,L
        CALL putchar
        JP (IY)

exec_:
        POP HL              
        JP (HL)

go_:
        LD HL,BC
        CALL rpush              ; save Instruction Pointer
        POP BC
        DEC BC
        JP  (IY)                ; Execute code from User def

heapPtr_:
        LD HL,vHeapPtr
        PUSH HL
        JP (IY)

i_:
        PUSH IX
        JP (IY)

; \+    a b -- [b]+a            ; increment variable at b by a
incr_:
        POP HL
        POP DE
        LD A,E
        ADD A,(HL)
        LD (HL),A
        INC HL
        LD A,D
        ADC A,(HL)
        LD (HL),A
        JP (IY)

; \-    a b -- [b]-a            ; decrement variable at b by a
decr_:
        POP HL
        POP DE
        LD A,(HL)
        SUB E
        LD (HL),A
        INC HL
        LD A,(HL)
        SBC A,D
        LD (HL),A
        JP (IY)


inPort_:
        POP HL
        LD C,L
        IN L,(C)
        LD H,0
        PUSH HL
        JP (IY)        

j_:
        PUSH IX
        POP HL
        LD DE,6
        ADD HL,DE
        PUSH HL
        JP (IY)

key_:
        CALL getchar
        LD L,A
        LD H,0
        PUSH HL
        JP (IY)

min_:                           ; a b -- c
        POP DE
        POP HL
        PUSH HL
        OR A
        SBC HL,DE
        JR NC,max1
        JP (IY)

max_:                           ; a b -- c
        POP DE
        POP HL
        PUSH HL
        OR A
        SBC HL,DE
        JR C,max1
        JP (IY)
max1:
        EX DE,HL
        EX (SP),HL
        JP (IY)

newln_:
        call crlf
        JP (IY)        

outPort_:
        POP HL
        LD C,L
        POP HL
        OUT (C),L
        JP (IY)        

sign_:
        POP HL
        BIT 7,H
        LD HL,0
        JR Z, sign2
        INC HL
sign2:
        PUSH HL
        JP (IY)

TIBPtr_:
        LD HL,vTIBPtr
        PUSH HL
        JP (IY)
dots_:
        JP dots
knownVar_:
        JP knownVar 
while_:
        JP while
cArrDef_:                   
        JP cArrDef
cArrEnd_:
        JP cArrEnd

.if EXTENDED = 1


edit_:
        JP edit
        
strDef_:
        JP strDef
type_:
        JP type

userVar_:
        JP userVar

.else

edit_:
strDef_:
type_:
userVar_:
        JP   (IY) 

.endif
        
; **************************************************************************
; Page 6 primitive routines 
; **************************************************************************

cArrDef:                        ; define a character array
        LD IY,ccompNEXT 
        JP arrDef1

cArrEnd:
        CALL rpop               ; DE = start of array
        PUSH HL
        EX DE,HL
        LD HL,(vHeapPtr)        ; HL = heap ptr
        OR A
        SBC HL,DE               ; bytes on heap 
        JP arrEnd2                  

dots:
        call ENTER
        DB "\\0@2-\\d1-\\9!\\9@\\_0=(\\9@(",$22,"@.2-))'",0
        JP (IY)

knownVar:
        LD A,(BC)
        SUB "0"                 ; Calc index
        LD L,A
        LD H,0
        LD DE,knownVars
knownVar2:
        ADD HL,HL
        ADD HL,DE
        PUSH HL
        JP (IY)

while: 
        POP HL
        LD A,L                  ; zero?
        OR H
        JR Z,while1
        JP (IY)
while1:
        LD DE,6                     ; drop loop frame
        ADD IX,DE
        JP begin1                   ; skip to end of loop        



; *********************************************************************
; * extended or non-core routines
; *********************************************************************

.if EXTENDED = 1


edit:
        LD A,":"
        LD (TIB),A
        CALL putchar
        INC BC
        LD A,(BC)
        LD (TIB+1),A
        CALL putchar
        LD A,(BC)
        SUB "A"
        ADD A,A
        LD HL,defs
        LD E,A
        LD D,0
        ADD HL,DE
        LD E,(HL)
        INC HL
        LD D,(HL)
        EX DE,HL
        LD DE,TIB+2
        JR edit2
edit1:
        CALL putchar
        INC HL
        INC DE
edit2:        
        LD A,(HL)
        LD (DE),A
        LD A,(HL)
        CP ";"
        JR NZ, edit1
        CALL putchar
        EX DE,HL
        LD DE,TIB-1
        OR A
        SBC HL,DE
        LD (vTIBPtr),HL
        LD HL,(vFLAGS)
        SET 0,L
        LD (vFLAGS),HL
        JP (IY)

strDef:
        INC BC                  ; point to next char
        PUSH BC                 ; push string address
        LD DE,0                 ; count = 0
        JR strDef2
strDef1:
        INC BC                  ; point to next char
        INC DE                  ; increase count
strDef2:
        LD A,(BC)
        CP "`"                  ; ` is the string terminator
        JR NZ,strDef1
        PUSH DE                 ; push count
        JP   (IY) 
type:
        call ENTER
        .cstr "(",$22,"\\@\\e1+)"
        JP (IY)

userVar:
        LD DE,userVars
        POP HL
        JP knownVar2
        JP      (IY)

.endif

;*******************************************************************
; Page 5 primitive routines continued
;*******************************************************************

; ARRAY compilation routine ***********************************************
compNEXT:
        POP DE          ; DE = return address
        LD HL,(vHeapPtr)    ; load heap ptr
        LD (HL),E       ; store lsb
        INC HL          
        LD (HL),D
        INC HL
        LD (vHeapPtr),HL    ; save heap ptr
        JP NEXT

; define a word array
arrDef:      
        LD IY,compNEXT  
arrDef1:      
        LD HL,(vHeapPtr)    ; HL = heap ptr
        CALL rpush          ; save start of array \[  \]
        JP NEXT         ; hardwired to NEXT

; end a word array
arrEnd:
        CALL rpop               ; DE = start of array
        PUSH HL
        EX DE,HL
        LD HL,(vHeapPtr)        ; HL = heap ptr
        OR A
        SBC HL,DE               ; bytes on heap 
        SRL H           ; BC = m words
        RR L
arrEnd2:
        PUSH HL 
        LD IY,NEXT
        JP (IY)         ; hardwired to NEXT

; **************************************************************************             
; def is used to create a colon definition
; When a colon is detected, the next character (usually uppercase alpha)
; is looked up in the vector table to get its associated code field address
; This CFA is updated to point to the character after uppercase alpha
; The remainder of the characters are then skipped until after a semicolon  
; is found.
; ***************************************************************************

def:                       ; Create a colon definition
        PUSH HL             ; Save HL
        LD HL, DEFS         ; Start address of jump table         
        INC BC
        LD  A,(BC)          ; Get the next character
        INC BC
        SUB "A"             ; Calc index
def1:
        ADD A,A             ; Double A to index even addresses
        LD E,A              ; Index into table
        LD D,0
        ADD HL,DE
        LD DE,(vHeapPtr)       ; start of defintion
        LD (HL),E           ; Save low byte of address in CFA
        INC HL              
        LD (HL),D           ; Save high byte of address in CFA+1
        POP HL              ; Restore HL
nextbyte:                   ; Skip to end of definition   
        LD A,(BC)           ; Get the next character
        INC BC              ; Point to next character
        LD (DE),A
        INC DE
        CP ";"                  ; Is it a semicolon 
        JR z, end_def           ; end the definition
        JR  nextbyte            ; get the next element

end_def:    
        LD (vHeapPtr),DE        ; bump heap ptr to after definiton
        DEC BC
        JP (IY)       

get_hex:
		LD HL,$0000				; 10t Clear HL to accept the number
		LD A,(BC)				; 7t  Get the character which is a numeral
        
get_hex1:        
        BIT 6,A                ; 7t    is it alpha?
        JR Z, ASCHX1           ; no 
        ADD A,$09              ; add 9 to make $A - $F
aschx1:
        AND $0F                 ; form hex nybble
        ADD A,L                 ; 4t    Add into bottom of HL
        LD  L,A                 ; 4t
        
                                ;  15t cycles
      
        INC BC                  ; 6t    Increment IP
        LD A, (BC)              ; 7t    and get the next character
        CP $30                  ; 7t    Is it a space terminator?
        JR C, endhex            ; 7/12t Not a number / end of number
        
        
       
times16:                        ; Multiply digit(s) in HL by 16
        ADD HL,HL               ; 11t    2X
        ADD HL,HL               ; 11t    4X
        ADD HL,HL               ; 11t    8X
        ADD HL,HL               ; 11t   16X     
                                ; 44t cycles

        JR  get_hex1
                
endhex:
;        PUSH HL                ; 11t   Put the number on the stack
        
;        JR dispatch            ; and process the next character

        RET
        
printhex:       

                                ; Display HL as a 16-bit number in hex.
        PUSH BC                 ; preserve the IP
        LD	A,H
		CALL	Print_Hex8
		LD	A,L
		CALL	Print_Hex8
		POP BC
		RET

; Print an 8-bit HEX number  - shortened KB 25/11/21
; A: Number to print
;
Print_Hex8:		
        LD	C,A
		RRA 
		RRA 
		RRA 
		RRA 
	    CALL conv
	    LD A,C

conv:		
        AND	0x0F
		ADD	A,0x90
		DAA
		ADC	A,0x40
		DAA
		CALL putchar
		RET            

macro:
        ADD A,A
        LD HL,MACROS
        LD D,0
        LD E,A
        ADD HL,DE
        LD (vTIBPtr),BC
        LD E,(HL)
        INC HL
        LD D,(HL)
        PUSH DE
        call ENTER
        .cstr "\\g"
        LD BC,(vTIBPtr)
        JP interpret2

; calculate nesting value
; A is char to be tested, 
; E is the nesting value (initially 0)
; E is increased by ( and [ 
; E is decreased by ) and ]
; E has its bit 7 toggled by `
; limited to 127 levels
nesting:
        CP '`'
        JR NZ,nesting1
        BIT 7,E
        JR Z,nesting1a
        RES 7,E
        RET
nesting1a: 
        SET 7,E
        RET
nesting1:
        CP '['
        JR Z,nesting2
        CP '('
        JR NZ,nesting3
nesting2:
        INC E
        RET
nesting3:
        CP ']'
        JR Z,nesting4
        CP ')'
        RET NZ
nesting4:
        DEC E
        RET 




