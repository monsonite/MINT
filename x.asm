; *************************************************************************
;
;        MINT_1.11 Micro-Interpreter for the Z80 based on SIMPL
;
;        Ken Boak October 27th 2021 
;
;	     Including major reconfiguration by John Hardy 24-10-21
;
;        New in this version 1.11:
;  
;        handler for hardware specific system calls
;   
;        sys_call jump table  sdefs:
;
;        over added and linked to %
;
;        placeholder for hexadecimal number routine #ABCD
;
;		 invert inv and negate neg now use the Subtraction routine.
;
;        comparison operators tidied up to save 12 bytes
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
;        MINT consists of 4 major sections:
;
;		 1.  A vector table to dispatch the 128 possible opcodes   128 bytes
;
;        2.  An interpreter kernel and serial interface routines   384 bytes
;
;        3.  An area containing the Primitive function code fields: up to 512 bytes
;
;        4.  A text buffer area to hold keyboard input and User routines  up to 1K bytes
;
;        The first 3 can be placed in a 1K ROM or RAM, the last item requires at least 1K RAM
;        
;        For simplicity on a modern system, everything is placed in RAM.
;
;
;
;        
;        Major rework by John Hardy 24-10-21
;
;		 NEXT followed by dispatch is just 46 t states
;
;
;
;        All commands accessed via a byte wide look up table
;
;        Heap used for command storage (HERE and HERE1)
;
;        Primitives are on page $82xx continued using a trampoline jump to page $83xx.
;        
;        This allows single byte opcodes reducing the dispatch time from
;        64 t states to 33 t states
;
;        Fits into 1024 bytes of ROM, with 163 bytes to spare.
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
;        /     DIV
;        %     MOD
;        _     NEG
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
;        .     DOT     (Print the value of the top of stack as a decimal)
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
;       Loops    - execute the code between parenthesis
;
;       The user variable i is used as the loop counter
;       It is decremented every time the loop is executed
;
;       10(repeat this code 10 times)
;
;        0(skip this code)
;
;        1(execute this code only once)
;
;       a@ b@ = (_print this if a=b_)
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

        ; TESTMODE    EQU 0
        ; ROMSTART    EQU $8000
        ; RAMSTART    EQU $8400

        ROMSIZE     EQU $800
        DSIZE       EQU $100
        RSIZE       EQU $100
        TBUFSIZE    EQU $100
        TRUE        EQU -1
        FALSE       EQU 0

        loopstart EQU  $A760   ; Loop code storage area
        loopcount EQU  $A810   ; Hold the loopcounter in variable i

.macro rpush,reghi,reglo

        DEC IX                  
        LD (IX+0),reghi
        DEC IX
        LD (IX+0),reglo

.endm

.macro rpop, reghi, reglo

        LD reglo,(IX+0)         
        INC IX              
        LD reghi,(IX+0)
        INC IX                  

.endm

        .ORG ROMSTART
        
opcodes:
        DB    lsb(exit_)   ;    NUL
        DB    lsb(sys_)    ;    SOH
        DB    lsb(sys_)    ;    STX
        DB    lsb(sys_)    ;    ETX
        DB    lsb(sys_)    ;    EOT
        DB    lsb(sys_)    ;    ENQ
        DB    lsb(sys_)    ;    ACK
        DB    lsb(sys_)    ;    BEL
        DB    lsb(sys_)    ;    BS
        DB    lsb(sys_)    ;    TAB
        DB    lsb(sys_)    ;    LF
        DB    lsb(sys_)    ;    VT
        DB    lsb(sys_)    ;    FF
        DB    lsb(finish)  ;    CR   This jumps through finish to interp
        DB    lsb(sys_)    ;    SO
        DB    lsb(sys_)    ;    SI
        DB    lsb(sys_)    ;    DLE
        DB    lsb(sys_)    ;    DC1
        DB    lsb(sys_)    ;    DC2
        DB    lsb(sys_)    ;    DC3
        DB    lsb(sys_)    ;    DC4
        DB    lsb(sys_)    ;    NAK
        DB    lsb(sys_)    ;    SYN
        DB    lsb(sys_)    ;    ETB
        DB    lsb(sys_)    ;    CAN
        DB    lsb(sys_)    ;    EM
        DB    lsb(sys_)    ;    SUB
        DB    lsb(sys_)    ;    ESC
        DB    lsb(sys_)    ;    FS
        DB    lsb(sys_)    ;    GS
        DB    lsb(sys_)    ;    RS
        DB    lsb(sys_)    ;    US
        DB    lsb(sys_)    ;    SP
        DB    lsb(store_)  ;    !            
        DB    lsb(dup_)    ;    "
        DB    lsb(hex_)    ;    #
        DB    lsb(swap_)   ;    $            
        DB    lsb(over_)   ;    %            
        DB    lsb(and_)    ;    &
        DB    lsb(drop_)   ;    '
        DB    lsb(begin_)  ;    (        
        DB    lsb(end_)    ;    )
        DB    lsb(mul_)    ;    *            
        DB    lsb(add_)    ;    +
        DB    lsb(quit_)   ;    ,            
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
        DB    lsb(open_)   ;    [
        DB    lsb(quit_)   ;    \
        DB    lsb(close_)  ;    ]
        DB    lsb(xor_)    ;    ^
        DB    lsb(str_)    ;    _
        DB    lsb(tick_)   ;    `            
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
        DB    lsb(save_)   ;    {
        DB    lsb(or_)     ;    |            
        DB    lsb(load_)   ;    }            
        DB    lsb(inv_)    ;    ~            
        DB    lsb(del_)    ;    backspace

sysdefs:  ; Addresses for sys_calls

		DW  nop_,   nop_,   nop_,   nop_,   nop_,   nop_,   nop_,   nop_    ;  
        DW  nop_,   nop_,   nop_,   nop_,   nop_,   nop_,   nop_,   nop_    ; 
		DW  nop_,   nop_,   nop_,   nop_,   nop_,   nop_,   nop_,   nop_    ;    
        DW  nop_,   nop_,   nop_,   nop_,   nop_,   nop_,   nop_,   nop_    ; 
        	
		

idefs:  DW  nop_,   nop_,   nop_,   nop_,   nop_,   nop_,   nop_,   nop_    ; ABCDEFGH    
        DW  nop_,   nop_,   nop_,   nop_,   nop_,   nop_,   nop_,   nop_    ; IJKLMNOP    
        DW  nop_,   nop_,   nop_,   nop_,   util_,  nop_,   nop_,   exec_   ; QRSTUVWX    
        DW  nop_,   nop_                                                    ; YZ    

mint:
        LD IX,RSTACK
        LD IY,NEXT			; IY provides a faster jump to NEXT
        LD BC,HEAP
        LD (HERE),BC
        LD (HERE1),BC
        LD A,FALSE
        LD (DEFINE),A
        LD HL,idefs
        LD DE,defs
        LD BC,26 * 2
        LDIR
interp:
        CALL crlf
        CALL ok             ; friendly prompt
        CALL crlf           ; newline
        LD A,(DEFINE)
        OR A
        JR NZ,interp1
        LD BC,(HERE)
        LD (HERE1),BC
        JR waitchar
interp1:
        LD BC,(HERE1)             
        LD (HERE),BC

; *******************************************************************         
; Wait for a character from the serial input (keyboard) 
; and store it in the text buffer. Keep accepting characters,
; increasing the instruction pointer BC - until a newline received.
; *******************************************************************

waitchar:   
        CALL getchar        ; loop around waiting for character
        CP $0A              ; Less than $0A
        JR Z, endchar       ; Return char
        CP $7F              ; Greater or equal to $7F
        JR NC, endchar             

        LD (BC), A          ; store the character in textbuf
        INC BC
        CP  $0              ; is it end of string?
        JR Z, endchar
        CP  $0D             ; is it a newline?
        JR Z, endchar
        
        CP ":"
        JR NZ,waitchar1
        
        LD A,TRUE
        LD (DEFINE),A
        LD A,":"
        
waitchar1:        
        CALL putchar        ; echo character to screen
        
        JR  waitchar        ; wait for next character
        
endchar:    
        LD (HERE1),BC
        LD A, $0D           ; Send out a CRLF
        CALL putchar
        LD A, $0A
        CALL putchar
			

        LD BC,(HERE)            ; Instructions stored on heap at address HERE
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
; 1. Detect characters A-Z where their table address will begin $A4xx
; and handle it as a user command
;
; 2. Detect characters a-z where their table address will begin $A8xx
; and handle it as a user variable
;
; 3. All other characters are punctuation and cause a jump to the associated
; primitive code.
;
; Instruction Pointer IP BC is incremented
; *********************************************************************************

NEXT:   
        INC BC                   ; 6t    Increment the IP
        LD A, (BC)               ; 7t    Get the next character and dispatch
		
dispatch:                        

        LD DE, opcodes           ; 7t    Start address of jump table         
        LD E,A                   ; 4t    Index into table
        LD A,(DE)                ; 7t    get low jump address
        LD L,A                   ; 4t    and put into L
        LD H, msb(page1)         ; 7t    Load H with the 1st page address
        JP  (HL)                 ; 4t    Jump to routine
        
                                 ; 33t  (previously 64t)


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
        PUSH HL                ; 11t   Put the number on the stack
        
        JR dispatch            ; and process the next character
        

        
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
        LD A, $0A           ; Send a CRLF
        CALL putchar
        LD A, $0D
        CALL putchar
        RET
        
ok:    CALL enter
        .cstr "_Ok_"
        RET

; ok:         
;         LD A, $4F           ; Print OK
;         CALL putchar
;         LD A, $4B
;         CALL putchar
;         RET            

printhex:

        ;Display a 16- or 8-bit number in hex.
DispHLhex:
; Input: HL
        ld  c,h
        call  OutHex8
        ld  c,l
OutHex8:
; Input: C
        ld  a,c
        rra
        rra
        rra
        rra
        call  Conv
        ld  a,c
Conv:
        and  $0F
        add  a,$90
        daa
        adc  a,$40
        daa
        call putchar  
        ret                
        
; There are 150 spare bytes here (for bitbang serial comms?)        

enter:
        rpush B,C               ; save Instruction Pointer
        POP BC
        DEC BC
        JP  (IY)                ; Execute code from User def

        .align $100
; **********************************************************************			 
     
; Start of primitive routines 

; **********************************************************************
page1:

quit_:        
        JR    ok                ; Print OK and return to monitor

exit_:
        INC BC
        LD HL,BC
        rpop B,C                 ; Restore Instruction pointer
        JP (HL)
        
sys_:   JP      (IY)            ; 8t Sys Call handler to be inserted    

num_:   JP  number

call_:
        rpush B,C               ; save Instruction Pointer
        LD A,(BC)
        SUB "A"                 ; Calc index
        ADD A,A
        LD HL,DEFS
        LD L,A
        LD C,(HL)
        INC HL
        LD B,(HL)
        DEC BC
        JP  (IY)                ; Execute code from User def

ret_:
        rpop B,C                ; Restore Instruction pointer
        JP (IY)             

var_:
        LD A,(BC)
        SUB "a"                 ; Calc index
        ADD A,A
        LD HL,VARS
        LD L,A
        PUSH HL
        JP (IY)
        
fetch_:                     ; Fetch the value from the address placed on the top of the stack      
        POP     HL          ; 10t
        LD      E,(HL)      ; 7t
        INC     HL          ; 6t
        LD      D,(HL)      ; 7t
        PUSH    DE          ; 11t
        
nop_:                       ; Sneakily piggy back nop here
        JP      (IY)        ; 8t
        
                            ; 49t 
        
         
store_:                    ; Store the value at the address placed on the top of the stack
        POP    HL          ; 10t
        POP    DE          ; 10t
        LD     (HL),E      ; 7t
        INC    HL          ; 6t
        LD     (HL),D      ; 7t
        JP     (IY)        ; 8t
        
                        ; 48t
    
dup_:        
        POP     HL         ; Duplicate the top member of the stack
        PUSH    HL
        PUSH    HL
        JP (IY)

swap_:        
        POP     HL         ; Swap the top 2 elements of the stack
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
                               
                               
inv_:						   ; Bitwise INVert the top member of the stack
        LD DE, $FFFF           ; by subtracting from $FFFF
        JR      SUB_1        
   
neg_:   LD HL, 0    		   ; NEGate the value on top of stack (2's complement)
        POP     DE             ; 10t
        EX DE,HL
        JR     SUB_2           ; use the SUBtract routine
    
sub_:       				   ; Subtract the value 2nd on stack from top of stack 
        
        POP     DE             ; 10t
sub_1:  POP     HL             ; 10t  Entry point for INVert
sub_2:  AND     A              ;  4t  Entry point for NEGate
        SBC     HL,DE          ; 15t
        PUSH    HL             ; 11t
        JP      (IY)           ; 8t
                               ; 58t
    


; ***********************************************************************************
; Loop Handling Code
; 
; On finding a left bracket, the interpreter copies the code to a loop buffer
; beginning at loopstart, until it finds a right bracket 
; Putting the loop code at a fixed address makes it easier to execute multiple times
; The loopcount is held in RAM at user variable i so that it can be used within the loop
; ***********************************************************************************

loopBegin:

    INC BC
    PUSH BC
    POP HL
    PUSH HL
    LD L,A
    OR H
    JR nz,loopBegin2

loopBegin1:    

    LD A,(BC)
    CP ')'
    JR z,loopBegin2
    INC BC
    JR loopBegin1

loopBegin2:    
    
    JP (IY)
    
    
loopEnd:

    POP HL
    LD A,L
    OR H
    JR Z,loopEnd1
    rpop B,C
    rpush B,C
    JR loopEnd2

loopEnd1:

    rpop H,L

loopEnd2:

    POP HL
    JP (IY)

begin_:                     ; Left parentesis begins a loop

        POP  HL             ; Get the loopcounter off the stack
        LD  (loopcount), HL ; Store the Loop counter in variable i
        LD  DE,loopstart    ; Loop code is then copied to buffer at loopstart 

loopbyte:   
        INC BC              ; Point to next character after the (
        LD A, (BC)          ; Get the next character
        CP $29              ; Is it a right bracket $29
        JR z, end_loop      ; end the code copy
        LD (DE), A          ; store the character at the loop buffer
        INC DE
        JR  loopbyte        ; get the next element

end_loop:   
        LD (DE), A          ; Store the closing bracket at the end of the loop           semicolon at end of definition
        LD A, $0D
        INC DE
        LD (DE), A          ; and a final Newline
        
        DEC  BC             ; IP now points to the loop terminator )
        
        JP   (IY)           ; Execute the closing  )
        
;*************************************************************************            
; This code executes the loop
; The loopcount is retrieved from RAM and loaded into HL
; The IP is set to point to the start of the loop
; HL is checked for zero and if zero the loop is terminated
; HL is decremented and stored back in the loopcount variable
; JP NEXT will execute each instruction between the brackets in turn
; including the closing ) which causes this routine to repeat until HL is zero
; ************************************************************************
end_:    
        LD HL, (loopcount)    ; get the loop count
        
loopagain:  
        LD  BC, loopstart -1  ; Point the IP to loopstart

        LD DE,     $0000  
        OR       A            ; reset the carry flag
        SBC      HL,DE        ; test if HL = 0
        JR      Z, finish     ; end the loop


        DEC     HL            ; While HL > zero
        LD  (loopcount), HL   ; preserve the loop count
        JP      (IY)          ; execute the next instruction
         
finish:     
        JP     interp         ; back to OK prompt


; **************************************************************************             
; Print the string between underscores
str_:                       
        INC BC
        
nextchar:            
        LD A, (BC)
        INC BC
        CP "_"              ; _ is the string terminator
        JR Z,stringend
        CALL putchar
        JR   nextchar


;       Trampoline Jumps to Page 2 of primitives
tick_:       JR tick
dot_:        JR dot
lt_:         JR lt
gt_:         JR gt
eq_:         JR eq
hex_:        JR hex
query_:      JR query
open_:       JR open
close_:      JR close
save_:       JR save
load_:       JR load
del_:        JR del
def_:        JR def_1
mul_:        JR mul_1
div_:        JR div_1
mod_:        JR mod_1

;*******************************************************************
; Page 2 Primitives
;*******************************************************************

stringend:  
        CALL crlf
        DEC BC
        JP   (IY) 
        
tick:                               ; execute a loop
        LD    BC, loopstart - 1
        JP   (IY)
        
dot:        
        POP     HL
        CALL    printdec
        CALL    crlf
        JP      (IY)
        



hex:       
        JP       (IY)             
        
; **************************************************************************
;  Comparison Operations
;  Put 1 on stack if condition is true and 0 if it is false
; **************************************************************************

eq:     POP      HL
        POP      DE
        AND      A         ; reset the carry flag
        SBC      HL,DE     ; only equality sets HL=0 here
        JR       Z, equal
        LD       HL, 0
        JR       less       ; HL = 1    

       
gt:     POP      DE
        POP      HL
        JR       cmp_
        
lt:     POP      HL
        POP      DE
cmp_:   AND      A         ; reset the carry flag
        SBC      HL,DE     ; only equality sets HL=0 here
        LD       HL, 0
        JP       M, less
equal:  INC      L          ; HL = 1    
less:     
        PUSH     HL
        JP       (IY) 
           
           


query:
        JP       (IY) 
        
open:      
        JP       (IY)
        
close:      
        JP       (IY)
        
        
        
save:       
        JP       (IY)
        
load:      
        JP       (IY)
        
del:      
        JP       (IY) 
        
; **************************************************************************             
; def is used to create a colon definition
; When a colon is detected, the next character (usually uppercase alpha)
; is looked up in the vector table to get its associated code field address
; This CFA is updated to point to the character after uppercase alpha
; The remainder of the characters are then skipped until after a semicolon  
; is found.
; ***************************************************************************

def_1:      
def:                       ; Create a colon definition
        INC BC
        LD  A,(BC)          ; Get the next character
        INC BC
                            ; Look up CFA in vector table
        SUB "A"             ; Calc index
        ADD A,A             ; Double A to index even addresses
        PUSH HL             ; Save HL
        LD HL, DEFS         ; Start address of jump table         
        LD L,A              ; Index into table
        LD (HL),C           ; Save low byte of IP in CFA
        INC HL              
        LD (HL),B           ; Save high byte of IP in CFA+1
        POP HL              ; Restore HL
nextbyte:                   ; Skip to end of definition   
        LD A,(BC)           ; Get the next character
        INC BC              ; Point to next character
        CP ";"              ; Is it a semicolon 
        JR z, end_def       ; end the definition
        JR  nextbyte        ; get the next element
end_def:    
        DEC BC
        JP (IY)       

; Second bounce trampoline jumps

mul_1:  JR mul
div_1:  JR div
mod_1:  JR mod

; ********************************************************
; Page 3 Primitives
;*********************************************************
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
        
        POP  BC
   
        PUSH DE
        PUSH HL

        JP       (IY)
        
; *********************************************************************            
; This divides DE by BC, storing the result in DE, remainder in HL
; *********************************************************************
div:  

        POP  DE             ; get first value
        POP  HL             ; get 2nd value
        PUSH BC             ; Preserve the IP
        LD B,H              ; BC = 2nd value
        LD C,L
                              
                        ;1281-2x, x is at most 16
        ld a,16         ;7
        ld hl,0         ;10
        jp $+5          ;10  
    
DivLoop:

        add hl,bc       ;--
        dec a           ;64
        JR Z, div_end   ;86

        sla e           ;128
        rl d            ;128
        adc hl,hl       ;240
        sbc hl,bc       ;240
        jr nc,DivLoop   ;23|21
        inc e           ;--
        jp DivLoop+1

div_end:    
        POP  BC         ; Restore the IP
   
        PUSH DE         ; Push Result
        PUSH HL         ; Push remainder             

        JP       (IY) 
        
; ***************************************************************
; MOD is a 16 / 8 Division
; ***************************************************************

mod:        
        POP  DE            ; get first value TOS
        POP  HL            ; get 2nd value   NOS
        PUSH BC            ; Preserve the IP
        LD B,16            ; C = 1st  value
        LD D,E
        
        
        ld b,16
        xor a
        
Div8_loop:            
        add hl,hl
        rla
        cp d
        jr c,Div8_next
        inc l
        sub d
Div8_next:            
        
        djnz Div8_loop
        
        
        POP  BC         ; Restore the IP
        
        LD   D, 0
        LD   E, A
   
        PUSH DE         ; Push Remainder 
        PUSH HL         ; Push Quotient      
        JP       (IY)
        

                              

; **********************************************************************
; 
; routines that are written in Mint
; Note: opcode zero can exit Mint and go into machine code
; Mint can be reentered from machine code by CALL enter
;
; **********************************************************************

util_:
        DB 0                ; exit Mint
        POP HL              ; get TOS
        LD A,L
        JP dispatch 

exec_:
        DB 0                ; exit Mint
        POP HL              ; get TOS
        JP (HL)
        


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
;             BC DE HL IX IY I AF' BC' DE' HL' preserved
; This function does not return until a character is available
getchar:
.if TESTMODE=1
        PUSH HL
        LD HL,(tbPtr)
        LD A,(HL)
        INC HL
        LD (tbPtr),HL
        POP HL
.else
        IN   A,(kACIA1Cont) ;Address of status register
        AND  $01            ;Receive byte available
        JR   Z, getchar     ;Return Z if no character
        IN   A,(kACIA1Data) ;Read data byte
.endif
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
        
        
        ; There are a few spare bytes here 

;**************************************************************

        .ORG RAMSTART

        DS DSIZE
DSTACK:        

        DS RSIZE
RSTACK:        

HERE:
        DW 0
HERE1:
        DW 0
DEFINE:
        DB 0

.if TESTMODE=1

tbPtr:
        DW 0

.endif
    

; ****************************************************************
; DEFS Table - holds 26 addresses of user routines
; ****************************************************************
        .align $100
DEFS:
        DS 26 * 2

; ****************************************************************
; VARS Table - holds 26 16-bit user variables
; ****************************************************************
        .align $100
VARS:
        DS 26 * 2
        
        .align $100
HEAP:         
