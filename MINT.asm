; *************************************************************************
;
;        MINT1_13 Micro-Interpreter for the Z80
;
;        Ken Boak and John Hardy November 10th 2021 
;
;	     Interim snapshot file to be merged later when confirmed  
;
;        New in this version 1_13:
;
;        Macros and new loop code
;
;
;		 Invert inv and Negate neg now use the Subtraction routine.
;
;        Shift Left { and shift right } operations added
;
;        Over reverted to %
;
;        Additional commands available using backslash prefix \
;  
;        Mod operator removed, / is now /mod
;
;        Division routine now working
;
;        Hexadecimal number entry routine #ABCD
;
;        Comma , is used to output in hexadecimal format
;
;		 Invert inv is ~ and Negate neg is _ now use the Subtraction routine.
;
;        Grave is now used to enclose  `text strings`
;
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

        ;ROMSTART    EQU $8000
        ;RAMSTART    EQU $8800

        ROMSIZE     EQU $800
        DSIZE       EQU $100
        RSIZE       EQU $100
        TIBSIZE     EQU $100
        TRUE        EQU 1
        FALSE       EQU 0

.macro _rpush,reghi,reglo

        DEC IX                  
        LD (IX+0),reghi
        DEC IX
        LD (IX+0),reglo

.endm

.macro _rpop, reghi, reglo
        LD reglo,(IX+0)         
        INC IX              
        LD reghi,(IX+0)
        INC IX                  
.endm

.macro _isZero, reghi, reglo
        LD A,reglo
        OR reghi
.endm

        .ORG ROMSTART
        
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

iUserVars:
        DW HEAP                 ; vHeapPtr
        DW FALSE                ; vBase16
        DW TIB                  ; vTibPtr
        DW getCharImpl          ; vGetChar
        DW altcodes             ; vAltCodes
        
        DW dStack               ; \0 cS0
        DW TIB                  ; \1 cTIB
        DW defs                 ; \2 cDefs
        DW vars                 ; \3 cVars
        DW macros               ; \4 cMacros

; **********************************************************************
; 
; defs that are written in Mint - placed here to fill up zeroth page
; Note: opcode zero (i.e. exit_) can exit Mint and go into machine code
; Mint can be reentered from machine code by CALL enter
;
; **********************************************************************

empty_:
        .cstr ";"

backsp_:
        .cstr "1_\\2\\+8\\e` `8\\e;"

toggleBase_:
        .cstr "\\b@1^\\b!;"

printStack_:
        .cstr "`=> `\\p\\n\\n`> `;"

initialize:
        LD IX,RSTACK
        LD IY,NEXT			; IY provides a faster jump to NEXT
        LD HL,iUserVars
        LD DE,userVars
        LD BC,10 * 2
        LDIR
        LD HL,defs
        LD B,26
init1:
        LD (HL),lsb(empty_)
        INC HL
        LD (HL),msb(empty_)
        INC HL
        DJNZ init1
        LD BC,$20 * 2
        LD DE,macros
        LD HL,ctrlcodes
        LDIR
        RET

mint:
        LD SP,DSTACK
        CALL initialize
        CALL enter
        .cstr "`MINT V1.0 by Ken Boak and John Hardy`\\n"
interpret:
        CALL enter
        .cstr "\\n\\n`> `"
interpret1:
        LD BC,TIB

; *******************************************************************         
; Wait for a character from the serial input (keyboard) 
; and store it in the text buffer. Keep accepting characters,
; increasing the instruction pointer BC - until a newline received.
; *******************************************************************

waitchar:   
        CALL getchar        ; loop around waiting for character
        CP $7F              ; Greater or equal to $7F
        JR NC, endchar             
        CP $20
        JR NC, waitchar1
        CP $0               ; is it end of string?
        JR Z, endchar
        CP '\n'              ; newline?
        JR Z, waitchar2
        CP '\r'              ; carriage return?
        JR Z, waitchar3
        
        ADD A,A
        LD HL,MACROS
        LD D,0
        LD E,A
        ADD HL,DE
        LD (vTibPtr),BC
        LD E,(HL)
        INC HL
        LD D,(HL)
        PUSH DE
        CALL enter
        .cstr "\\g"
        LD BC,(vTibPtr)
        JP waitchar

waitchar1:
        LD (BC), A          ; store the character in textbuf
        INC BC

waitchar2:        
        CALL putchar        ; echo character to screen
        JR  waitchar        ; wait for next character

waitchar3:
        LD (BC), A          ; store the character in textbuf
        INC BC

endchar:    
        LD (vTibPtr),BC
        CALL crlf

        LD BC,TIB           ; Instructions stored on heap at address HERE
        DEC BC
                            ; Drop into the NEXT and dispatch routines

; ********************************************************************************
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
; *********************************************************************************

NEXT:   
        INC BC                      ; 6t    Increment the IP
        LD A, (BC)                  ; 7t    Get the next character and dispatch
		
dispatch:                        
        sub ' '                     ; 7t    remove char offset
        JR NC,dispatch1
        CP 0 - ' '                  ;       expected values: 0 or '\r'
        JP Z, exit_
        JR interpret                ;       back to OK prompt
dispatch1:
        LD DE, opcodes              ; 7t    Start address of jump table         
        LD E,A                      ; 4t    Index into table
        LD A,(DE)                   ; 7t    get low jump address
        LD L,A                      ; 4t    and put into L
        LD H, msb(page1)            ; 7t    Load H with the 1st page address
        JP  (HL)                    ; 4t    Jump to routine

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
        call putchar
        ret 
        
crlf:       
        LD A, '\r'
        CALL putchar
        LD A, '\n'           
        CALL putchar
        RET
        
space:       
        LD A, ' '           
        CALL putchar
        RET

enter:
        _rpush B,C              ; save Instruction Pointer
        POP BC
        DEC BC
        JP  (IY)                ; Execute code from User def

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
        
number1:        
        SUB $30                 ; 7t    Form decimal digit
        ADD A,L                 ; 4t    Add into bottom of HL
        LD  L,A                 ; 4t
        
                                ;  15t cycles
      
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

        .align $100             ; page boundary

ctrlcodes:
altcodes:
        DW empty_       ; NUL ^@
        DW empty_       ; SOH ^A
        DW toggleBase_  ; STX ^B
        DW empty_       ; ETX ^C
        DW empty_       ; EOT ^D
        DW empty_       ; ENQ ^E
        DW empty_       ; ACK ^F
        DW empty_       ; BEL ^G
        DW backsp_      ; BS  ^H
        DW empty_       ; TAB ^I
        DW empty_       ; LF  ^J
        DW empty_       ; VT  ^K
        DW empty_       ; FF  ^L
        DW empty_       ; CR  ^M
        DW empty_       ; SO  ^N
        DW empty_       ; SI  ^O
        DW printStack_  ; DLE ^P
        DW empty_       ; DC1 ^Q
        DW empty_       ; DC2 ^R
        DW empty_       ; DC3 ^S
        DW empty_       ; DC4 ^T
        DW empty_       ; NAK ^U
        DW empty_       ; SYN ^V
        DW empty_       ; ETB ^W
        DW empty_       ; CAN ^X
        DW empty_       ; EM  ^Y
        DW empty_       ; SUB ^Z
        DW empty_       ; ESC ^[
        DW empty_       ; FS  ^\
        DW empty_       ; GS  ^]
        DW empty_       ; RS  ^^
        DW empty_       ; US  ^_
        DW empty_       ; SP  ^`
        DW   cStore_    ;    !            
        DW   nop_       ;    "
        DW   access_    ;    #  ( idx adr -- adr ) access item in array
        DW   nop_       ;    $            
        DW   nop_       ;    %            
        DW   nop_       ;    &
        DW   nop_       ;    '
        DW   nop_       ;    (        
        DW   nop_       ;    )
        DW   nop_       ;    *            
        DW   incr_      ;    +  ( adr -- ) decrements variable at address
        DW   nop_       ;    ,            
        DW   nop_       ;    -  ( adr -- ) increments variable at address
        DW   nop_       ;    .  
        DW   nop_       ;    /
        DW   knownVar_  ;    0  ( -- adr ) start of data stack constant         
        DW   knownVar_  ;    1  ; returns HERE variable
        DW   knownVar_  ;    2  ( -- adr ) tibPtr variable          
        DW   knownVar_  ;    3  ( -- adr ) isHex variable
        DW   knownVar_  ;    4            
        DW   knownVar_  ;    5            
        DW   knownVar_  ;    6            
        DW   knownVar_  ;    7
        DW   knownVar_  ;    8            
        DW   knownVar_  ;    9        
        DW   adef_      ;    :  start defining a macro        
        DW   nop_       ;    ;  
        DW   nop_       ;    <
        DW   nop_       ;    =            
        DW   nop_       ;    >            
        DW   nop_       ;    ?
        DW   cFetch_    ;    @      
        DW   nop_       ;    A    
        DW   nop_       ;    B
        DW   nop_       ;    C
        DW   nop_       ;    D    
        DW   nop_       ;    E  
        DW   nop_       ;    F
        DW   nop_       ;    G
        DW   nop_       ;    H  
        DW   inPort_    ;    I  ( port -- val )   
        DW   nop_       ;    J
        DW   nop_       ;    K  
        DW   nop_       ;    L
        DW   nop_       ;    M
        DW   nop_       ;    N
        DW   outPort_   ;    O  ( val port -- )
        DW   nop_       ;    P
        DW   nop_       ;    Q
        DW   nop_       ;    R
        DW   nop_       ;    S
        DW   nop_       ;    T
        DW   nop_       ;    U
        DW   nop_       ;    V
        DW   nop_       ;    W
        DW   exec_      ;    X
        DW   nop_       ;    Y
        DW   nop_       ;    Z
        DW   cArrDef_   ;    [
        DW   comment_   ;    \  comment text, skips reading until end of line
        DW   cArrEnd_   ;    ]
        DW   nop_       ;    ^
        DW   nop_       ;    _
        DW   strDef_    ;    `            
        DW   nop_       ;    a
        DW   base16_    ;    b
        DW   nop_       ;    c
        DW   depth_     ;    d  ( -- val ) depth of data stack
        DW   emit_      ;    e  ( val -- ) emits a char to output
        DW   nop_       ;    f
        DW   go_        ;    g  ( -- ? ) execute mint definition
        DW   heapPtr_   ;    h  ; returns haep ptr variable
        DW   i_         ;    i  ; returns index variable of current loop          
        DW   j_         ;    j  ; returns index variable of outer loop
        DW   key_       ;    k  ( -- val )  read a char from input
        DW   nop_       ;    l
        DW   nop_       ;    m
        DW   newln_     ;    n  ; prints a newline to output
        DW   nop_       ;    o
        DW   dots_      ;    p  ( -- ) non-destructively prints stack
        DW   quit_      ;    q  ; quits from Mint REPL         
        DW   nop_       ;    r
        DW   nop_       ;    s    
        DW   nop_       ;    t
        DW   nop_       ;    u
        DW   nop_       ;    v   
        DW   nop_       ;    w
        DW   exec_      ;    x
        DW   nop_       ;    y
        DW   nop_       ;    z
        DW   nop_       ;    {
        DW   nop_       ;    |            
        DW   nop_       ;    }            
        DW   not_       ;    ~ ( b -- notb ) logical not           
        DW   nop_       ;    BS

        

        .align $100
; **********************************************************************			 
     
; Start of primitive routines 

; **********************************************************************
page1:

alt_:        
        JP alt
exit_:
        INC BC
        LD HL,BC
        _rpop B,C               ; Restore Instruction pointer
        JP (HL)
        
num_:   JP  number

call_:
        _rpush B,C              ; save Instruction Pointer
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
        _rpop B,C               ; Restore Instruction pointer
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

swap_:        
        POP     HL          ; Swap the top 2 elements of the stack
        POP     DE
        PUSH    HL
        PUSH    DE
        JP (IY)
        
over_:  POP     HL          ; Duplicate 2nd element of the stack
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
        POP     HL
        LD      A,E
        XOR     L
        LD      L,A
        LD      A,D
        XOR     H
        LD      H,A
        PUSH    HL
        JP      (IY)

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
		
shr:    
        POP HL                  ; Get the top member of the stack
        SRL H
        RR L
        PUSH HL
        JP (IY)                 ; 8t

inv_:						    ; Bitwise INVert the top member of the stack
        LD DE, $FFFF            ; by subtracting from $FFFF
        JR      SUB_1        
   
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
    
; **************************************************************************
;  Comparison Operations
;  Put 1 on stack if condition is true and 0 if it is false
; **************************************************************************

eq_:    POP      HL
        POP      DE
        AND      A         ; reset the carry flag
        SBC      HL,DE     ; only equality sets HL=0 here
        JR       Z, equal
        LD       HL, 0
        JR       less       ; HL = 1    

       
gt_:    POP      DE
        POP      HL
        JR       cmp_
        
lt_:    POP      HL
        POP      DE
cmp_:   AND      A         ; reset the carry flag
        SBC      HL,DE     ; only equality sets HL=0 here
        LD       HL, 0
        JP       M, less
equal:  INC      L          ; HL = 1    
less:     
        PUSH     HL
        JP       (IY) 
        
;       Trampoline Jumps to Page 2 of primitives


hex_:   CALL     get_hex
        JR       less      ; piggyback for ending

str_:       JP str
dot_:       JP dot
hexp_:      JP hexp ; Print HL as a hexadecimal
query_:     JR query
shr_:       JR shr
del_:       JR del
mul_:       JR mul
div_:       JR div
def_:       JP def
begin_:     JR begin                   
again_:     JP again
arrDef_:    JP arrDef
arrEnd_:    JP arrEnd
nop_:       JP NEXT             ; hardwire white space to always go to NEXT (important for arrays)

           
;*******************************************************************
; Page 2 Primitives
;*******************************************************************
           

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
div:  

        POP  DE             ; get first value
        POP  HL             ; get 2nd value
div1:   PUSH BC             ; Preserve the IP
        LD B,H              ; BC = 2nd value
        LD C,L

Div16:
        ld hl,0
        ld a,b
        ld b,8
Div16_Loop1:
        rla
        adc hl,hl
        sbc hl,de
        jr nc,Div16_NoAdd1
        add hl,de
Div16_NoAdd1:
        djnz Div16_Loop1
        rla
        cpl
        ld b,a
        ld a,c
        ld c,b
        ld b,8
Div16_Loop2:
        rla
        adc hl,hl
        sbc hl,de
        jr nc,Div16_NoAdd2
        add hl,de
Div16_NoAdd2:
        djnz Div16_Loop2
        rla
        cpl
        ld d,c
        ld e,a
		
	EX DE,HL

mul_end:
div_end:    
        POP  BC             ; Restore the IP
   
        PUSH DE             ; Push Result
        PUSH HL             ; Push remainder             

        JP       (IY)
        	        
        

		
begin:                               ; Left parentesis begins a loop
        POP HL
        _isZero H,L
        JR Z,begin1
        _rpush B,C                  ; push loop address
        DEC HL
        _rpush H,L                  ; push loop limit
        _rpush 0,0                  ; push loop var=0
        JP (IY)
begin1:
        LD E,1
begin2:
        INC BC
        LD A,(BC)
        CP '_'
        JR NZ,begin3
        LD A,$80
        XOR E
        LD E,A
        JR begin2
begin3:
        CP '['
        JR Z,begin4
        CP '('
        JR NZ,begin5
begin4:
        INC E
        JR begin2
begin5:
        CP ']'
        JR Z,begin6
        CP ')'
        JR NZ,begin2
begin6:
        DEC E
        JR NZ,begin2
        JP (IY)

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
        INC IX                      ; drop loop var
        INC IX
        INC IX                      ; drop loop limit
        INC IX
        INC IX                      ; drop loop address
        INC IX
        JP (IY)

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
        CP ";"              ; Is it a semicolon 
        JR z, end_def       ; end the definition
        JR  nextbyte        ; get the next element
end_def:    
        LD (vHeapPtr),DE        ; bump heap ptr to after definiton
        DEC BC
        JP (IY)       

alt:
        INC BC
        LD A,(BC)
        ADD A,A
        LD HL,altcodes
        LD L,A
        LD E,(HL)
        INC HL
        LD D,(HL)
        EX DE,HL
        JP (HL)                 ; Execute code from Alt

dot:        
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
        
; **************************************************************************             
; Print the string between underscores
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
        
hexp:                      ; Print HL as a hexadecimal
        POP     HL
        CALL    printhex
        CALL    space
        JP      (IY)

userVar_:
        LD DE,userVars
        JP access1

access_:
        POP DE
access1:
        POP HL
access2:
        ADD HL,HL
        ADD HL,DE
        PUSH HL
        JP (IY)

compNEXT:
        POP DE          ; DE = return address
        LD HL,(vHeapPtr)    ; load heap ptr
        LD (HL),E       ; store lsb
        INC HL          
        LD (HL),D
        INC HL
        LD (vHeapPtr),HL    ; save heap ptr
        JP NEXT

ccompNEXT:
        POP DE          ; DE = return address
        LD HL,(vHeapPtr)    ; load heap ptr
        LD (HL),E       ; store lsb
        INC HL          
        LD (vHeapPtr),HL    ; save heap ptr
        JP NEXT

; define a word array
arrDef:      
        LD IY,compNEXT  
        JR arrDef1

; define a character array
cArrDef_:
        LD IY,ccompNEXT 
arrDef1:      
        LD HL,(vHeapPtr)    ; HL = heap ptr
        _rpush H,L      ; save start of array \[  \]
        JP NEXT         ; hardwired to NEXT

; end a word array
arrEnd:
        _rpop D,E       ; DE = start of array
        PUSH DE         
        LD HL,(vHeapPtr)    ; HL = heap ptr
        OR A
        SBC HL,DE       ; bytes on heap 
        SRL H           ; BC = m words
        RR L
        JR arrEnd2

; end a character array

cArrEnd_:
        _rpop D,E       ; DE = start of array
        PUSH DE         
        LD HL,(vHeapPtr)    ; HL = heap ptr
        OR A
        SBC HL,DE       ; bytes on heap 
arrEnd2:
        PUSH HL 
        LD IY,NEXT
        JP (IY)         ; hardwired to NEXT

cFetch_:                    ; Fetch the value from the address placed on the top of the stack      
        POP     HL          ; 10t
        LD      E,(HL)      ; 7t
        LD      D,0         ; 7t
        PUSH    DE          ; 11t
        JP      (IY)        ; 8t
                            ; 49t 
        
cStore_:                    ; Store the value at the address placed on the top of the stack
        POP    HL           ; 10t
        POP    DE           ; 10t
        LD     (HL),E       ; 7t
        JP     (IY)         ; 8t
                            ; 48t

adef_:
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

comment_:
        INC BC              ; point to next char
        LD A,(BC)
        CP "\r"             ; terminate at newline 
        JR NZ,comment_
        DEC BC
        JP   (IY) 

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

dots_:
        CALL enter
        DB "\\0@ 2-\\d1-(",$22,"@.2-)'",0
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
        _rpush B,C              ; save Instruction Pointer
        POP BC
        DEC BC
        JP  (IY)                ; Execute code from User def

heapPtr_:
        LD HL,vHeapPtr
        PUSH HL
        JP (IY)

inPort_:
        POP HL
        LD C,L
        IN L,(C)
        LD H,0
        PUSH HL
        JP (IY)        

i_:
        PUSH IX
        JP (IY)

incr_:
        POP HL                  ; HL = addr, save BC
        POP DE                  ; DE = incr
        LD A,E                  ; A = lsb(addr@)
        ADD A,(HL)              ; add lsb(incr) and A 
        LD (HL),A               ; store A in lsb(addr@)
        INC HL
        LD A,D                  ; A = msb(addr@)
        ADC A,(HL)              ; add with carry msb(addr@)
        LD (HL),A               ; store A in msb(addr@)
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

newln_:
        call crlf
        JP (IY)        

not_:
        POP HL
        LD A,L
        INC A
        AND $01
        LD L,A
        XOR A
        LD H,A
        PUSH HL
        JP (IY)        

outPort_:
        POP HL
        LD C,L
        POP HL
        OUT (C),L
        JP (IY)        

quit_:
        RET                     ; display OK and exit interpreter

strDef_:
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
        
knownVar_:
        LD A,(BC)
        SUB "0"                 ; Calc index
        LD L,A
        LD H,0
        LD DE,knownVars
        JP access2

; ************************SERIAL HANDLING ROUTINES**********************        
;
;        Includes drivers for 68B50 ACIA 
;		 serial interface I/O primitive routines getchar and putchar
;        printstring
;        printdec
;        printhex
;        crlf         

; **********************************************************************
; **  Device Driver                             by Stephen C Cousins  **
; **  Hardware:  RC2014                                               **
; **  Interface: Serial 6850 ACIA                                     **
; **********************************************************************

; This module is the driver for the RC2014 serial I/O interface which is
; based on the 6850 Asynchronous Communications Interface Adapter (ACIA)
;
; Base addresses for ACIA externally defined. eg:
kACIA1:    .EQU 0x80           ;Base address of serial ACIA #1
kACIA2:    .EQU 0x80           ;Base address of serial ACIA #2
;
; RC2014 addresses for 68B50 number 2:
; 0x40   Control registers (read and write)
; 0x41   Data registers (read and write)
;
; Control registers (read and write)
; Bit   Control write              Control read
;  0    Counter divide select 1    Receive data register full
;  1    Counter divide select 2    Transmit data register empty
;  2    Word select 1              Data carrier detect (/DCD) input
;  3    Word seelct 2              Clear to send (/CTS) input
;  4    Word select 3              Framing error
;  5    Transmit contol 1          Receiver overrun
;  6    Transmit control 2         Parity error
;  7    Receive interrupt enable   Interrupt request
;
; Control register write
; Bit   7   6   5   4   3   2   1   0
;       |   |   |   |   |   |   |   |
;       |   |   |   |   |   |   0   0     Clock divide 1
;       |   |   |   |   |   |   0   1     Clock divide 16
; >     |   |   |   |   |   |   1   0  >  Clock divide 64
;       |   |   |   |   |   |   1   1     Master reset
;       |   |   |   |   |   |
;       |   |   |   0   0   0     7 data bits, even parity, 2 stop bits
;       |   |   |   0   0   1     7 data bits, odd parity,  2 stop bits
;       |   |   |   0   1   0     7 data bits, even parity, 1 stop bit
;       |   |   |   0   1   1     7 data bits, odd parity,  1 stop bit
;       |   |   |   1   0   0     8 data bits, no parity,   2 stop bits
;       |   |   |   1   0   1  >  8 data bits, no parity,   1 stop bit
;       |   |   |   1   1   0     8 data bits, even parity, 1 stop bit
;       |   |   |   1   1   1     8 data bits, odd parity,  1 stop bit
;       |   |   |
;       |   0   0  >  /RTS = low (ready), tx interrupt disabled
;       |   0   1     /RTS = low (ready), tx interrupt enabled
;       |   1   0     /RTS = high (not ready), tx interrupt disabled 
;       |   1   1     /RTS = low, tx break, tx interrupt disabled
;       |
;       0  >  Receive interrupt disabled
;       1     Receive interrupt enabled
;
; Control register read
; Bit   7   6   5   4   3   2   1   0
;       |   |   |   |   |   |   |   |
;       |   |   |   |   |   |   |   +-------  Receive data register full
;       |   |   |   |   |   |   +-------  Transmit data register empty
;       |   |   |   |   |   +-------  Data carrier detect (/DCD)
;       |   |   |   |   +-------  Clear to send (/CTS)
;       |   |   |   +-------  Framing error
;       |   |   +-------  Receiver overrun 
;       |   +-------  Parity error
;       +-------  Interrupt request

; 6850 #1 registers derived from base address (above)
kACIA1Cont: .EQU kACIA1+0       ;I/O address of control register
kACIA1Data: .EQU kACIA1+1       ;I/O address of data register
; 6850 #2 registers derived from base address (above)
kACIA2Cont: .EQU kACIA2+0       ;I/O address of control register
kACIA2Data: .EQU kACIA2+1       ;I/O address of data register

; Control register values
k6850Reset: .EQU 0b00000011     ;Master reset
k6850Init:  .EQU 0b00010110     ;No int, RTS low, 8+1, /64

; Status (control) register bit numbers
k6850RxRdy: .EQU 0              ;Receive data available bit number
k6850TxRdy: .EQU 1              ;Transmit data empty bit number

; Device detection, test 1
; This test just reads from the devices' status (control) register
; and looks for register bits in known states:
; /CTS input bit = low
; /DCD input bit = low
; WARNING
; Sometimes at power up the Tx data reg empty bit is zero, but
; recovers after device initialised. So test 1 excludes this bit.
k6850Mask1: .EQU  0b00001100    ;Mask for known bits in control reg
k6850Test1: .EQU  0b00000000    ;Test value following masking

; Device detection, test 2
; This test just reads from the devices' status (control) register
; and looks for register bits in known states:
; /CTS input bit = low
; /DCD input bit = low
; Transmit data register empty bit = high
k6850Mask2: .EQU  0b00001110    ;Mask for known bits in control reg
k6850Test2: .EQU  0b00000010    ;Test value following masking

; RC2014 serial 6850 initialise
;   On entry: No parameters required
;   On exit:  Z flagged if device is found and initialised
;             AF BC DE HL not specified
;             IX IY I AF' BC' DE' HL' preserved
; If the device is found it is initialised
serial_init:
; First look to see if the device is present
; Test 1, just read from chip, do not write anything
        IN   A,(kACIA1Cont) ;Read status (control) register
        AND  k6850Mask1     ;Mask for known bits in control reg
        CP   k6850Test1     ;and check for known values
        RET  NZ             ;If not found return with NZ flag
; Attempt to initialise the chip
        LD   A,k6850Reset   ;Master reset
        OUT  (kACIA1Cont),A ;Write to ACIA control register
        LD   A,k6850Init    ;No int, RTS low, 8+1, /64
        OUT  (kACIA1Cont),A ;Write to ACIA control register
; Test 2, perform tests on chip following initialisation
        IN   A,(kACIA1Cont) ;Read status (control) register
        AND  k6850Mask2     ;Mask for known bits in control reg
        CP   k6850Test2     ;Test value following masking
;           RET  NZ             ;Return not found NZ flagged
        RET                 ;Return Z if found, NZ if not


; RC2014 serial 6850 input character
;   On entry: No parameters required
;   On exit:  A = Character input from the device
;             NZ flagged if character input
;             BC DE IX IY I AF' BC' DE' HL' preserved
;             HL destroyed
; This function does not return until a character is available
getchar:
        LD HL,(vGetChar)
        JP (HL)
        
getCharImpl:
        IN   A,(kACIA1Cont) ;Address of status register
        AND  $01            ;Receive byte available
        JR   Z, getchar     ;Return Z if no character
        IN   A,(kACIA1Data) ;Read data byte
        RET                 ;NZ flagged if character input


; RC2014 serial 6850 output character
;   On entry: A = Character to be output to the device
;   On exit:  If character output successful (eg. device was ready)
;               NZ flagged and A != 0
;             If character output failed (eg. device busy)
;               Z flagged and A = Character to output
;             BC DE HL IX IY I AF' BC' DE' HL' preserved
putchar:
        PUSH BC
        LD   C,kACIA1Cont   ;ACIA control register
        IN   B,(C)          ;Read ACIA control register
        BIT  k6850TxRdy,B   ;Transmit register full?
        POP  BC
        JR  Z, putchar      ;Return Z as character not output
        OUT  (kACIA1Data),A ;Write data byte
        OR   0xFF           ;Return success A=0xFF and NZ flagged
        RET
        
        
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
            PUSH BC             ; preserve the IP
            LD	A,H
			CALL	Print_Hex8
			LD	A,L
			CALL	Print_Hex8
			POP BC
			RET

; Print an 8-bit HEX number
; A: Number to print
;
Print_Hex8:		
            LD	C,A
			RRA 
			RRA 
			RRA 
			RRA 
		

conv:		AND	0x0F
			ADD	A,0x90
			DAA
			ADC	A,0x40
			DAA
			CALL putchar
			LD A,C
			AND	0x0F
			ADD	A,0x90
			DAA
			ADC	A,0x40
			DAA
			CALL putchar
            RET                


        .ORG RAMSTART
        
        DS DSIZE
dStack:        

        DS RSIZE
rStack:        
tib:
        DS TIBSIZE

; ****************************************************************
; VARS Table - holds 26 16-bit user variables
; ****************************************************************
vars:
        DS 26 * 2

; ****************************************************************
; CDEFS Table - holds $20 ctrl key macros
; ****************************************************************
macros:
        DS $20 * 2

; ****************************************************************
; DEFS Table - holds 26 addresses of user routines
; ****************************************************************
defs:
        DS 26 * 2

userVars:

vHeapPtr:   DW 0                ; 
vBase16:    DW 0                ; 
vTibPtr:    DW 0                ; 
vGetChar:   DW 0                ;  
vAltCodes:  DW 0                ; 

knownVars:

cS0:        DW 0                ; \0                   
cTIB        DW 0                ; \4
cDefs:      DW 0                ; \1
cVars:      DW 0                ; \2
cMacros:    DW 0                ; \3

tbPtr:      DW 0                ; reserved for tests

        
HEAP:         

