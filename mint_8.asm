; *************************************************************************
;
;        MINT_8 micro-interpreter based on SIMPL
;
;        Ken Boak July 2021
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
;
;        MINT consists of 4 major sections:
;
;        1.  An interpreter kernel and serial interface routines
;
;        2.  A vector table to dispatch the 96 possible opcodes
;
;        3.  A text buffer area to hold keyboard input and User routines
;
;        4.  An area containing the Primitive function code fields
;
; ***********************************************************************


         tabstart  EQU  $82     ; High Address of Vector table
         
         textbuf   EQU  $8100   ; Input text buffer
         
         stringbuf EQU  $8800
         
         
         .ORG $8000
; *******************************************************************         
; Wait for a character from the serial input (keyboard) 
; and store it in the text buffer. Keep accepting characters,
; increasing the instruction pointer BC - until a newline received.
; *******************************************************************

            
inchar:     LD      BC,textbuf  ; input buffer is at $8100      
            
waitchar:   CALL    getchar     ; loop around waiting for character

            CP $0A              ; Less than $0A
            JR C, endchar       ; Return char
            CP $7F              ; Greater or equal to $7F
            JR NC, endchar             

            LD (BC), A          ; store the character in textbuf
            INC BC
            CP  $0D             ; is it a newline?
            JR Z, endchar
            
            CALL putchar        ; echo character to screen
            
            JR  waitchar        ; wait for next character
            
endchar:    LD A, $0D           ; Send out a CRLF
            CALL putchar
            LD A, $0A
            CALL putchar
			
; ********************************************************************************
; Number Handling Routine - converts numeric ascii string to a 16-bit number in HL
; On receipt of a newline character, reset BC to the start of the text buffer
; Read the first character. If not a number (0-9) jump to the dispatch routine
;			
; Number characters ($30 to $39) are converted to digits by subtracting $30
; and then added into the L register. (HL forms a 16-bit accumulator)
; Fetch the next character, if it is a number, multiply contents of HL by 10
; and then add in the next digit. Repeat this until a non-number character is 
; detected. Add in the final digit so that HL contains the converted number.
; Push HL onto the stack and proceed to the dispatch routine.
; ********************************************************************************
         
        LD BC, textbuf-1        ; Instructions begin at $8100
        
NEXT:   LD HL, $0000            ; 10t   Clear HL to accept new number
        INC BC                  ; 6t    Increment the IP
        LD A, (BC)              ; 7t    Get the next character
        CP $30                  ; 7t    Less that $30
        JR C, dispatch          ; 7/12t Not a number
        CP $3A                  ; 7t    Greater or equal to $3A
        JR NC, dispatch         ; 7/12t Not a number
        
        SUB $30                 ; 7t    Form decimal digit
        ADD A,L                 ; 4t    Add into bottom of HL
        LD  L,A                 ; 4t
      
number1:                               
        INC BC                  ; 6t    Increment IP
        LD A, (BC)              ; 7t    Get the next character
        CP $30                  ; 7t    Less than $30
        JR C, endnum            ; 7/12t Not a number / end of number
        CP $3A                  ; 7t    Greater or equal to $3A
        JR NC, endnum           ; 7/12t Not a number / end of number
       
times10:                        ; Multiply digit(s) in HL by 10
        ADD HL,HL               ; 11t    2 X
        LD  E,L                 ;  4t    LD DE,HL
        LD  D,H                 ;  4t
        ADD HL,HL               ; 11t    4 X
        ADD HL,HL               ; 11t    8 X
        ADD HL,DE               ; 11t    10 X
                                ; 52t cycles
        
        SUB $30                 ;  7t   Form next decimal digit in A
        ADD A,L                 ;  4t   Add into bottom of HL
        LD  L,A                 ;  4t
                                ;  15t cycles
        
        JP  number1
                
endnum:
         PUSH HL                ; 11t   Put the number on the stack
		 
; ********************************************************************************
; Dispatch Routine.
; The Instruction Pointer IP BC is pushed to preserve its contents.
; The current character from the text buffer is temporarily held in A.
; It is doubled so that it forms an even number and will point to even address.
; B is loaded with the high byte of the vector table tabstart and the doubled		 
; value of A is loaded into C, so that it indexes into the vector table.
; At the selected table location is a 2-byte jump address to the selected function
; This target jump address is loaded into HL, and BC is restored.
; The routine uses HL to jump to the selected function.
; *********************************************************************************

dispatch:                       ; The character at IP is not a number

         PUSH BC                ; 11t   Push the current IP
         ADD A,A                ; 4t    Double A to index even addresses
         LD B, tabstart         ; 7t    Start address of jump table         
         LD C,A                 ; 4t    Index into table
         LD A,(BC)              ; 7t    get low jump address
         LD L,A                 ; 4t    and put into L
         INC BC                 ; 6t
         LD A,(BC)              ; 7t    Get high jump address
         LD H,A                 ; 4t    and put into H
         POP BC                 ; 10t   Get IP back
         JP (HL)                ; 4t    Jump to routine
                                ; 68t cycles
                                
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
            
            
printstring:

            LD HL, stringbuf
nextchar:            
            LD A, (HL)
            INC HL
            CP $00
            JR Z,stringend
            CALL putchar
            
;            LD B,20
; L1:        DJNZ L1
;            CALL  putchar
            
            JR   nextchar
            
stringend:  RET


printdec:

;Number in hl to decimal ASCII
;Thanks to z80 Bits
;inputs:	hl = number to ASCII
;example: hl=300 outputs '00300'
;destroys: af, bc, hl, de used
DispHL:
	        ld	bc,-10000
	        call	Num1
	        ld	bc,-1000
	        call	Num1
	        ld	bc,-100
	        call	Num1
	        ld	c,-10
	        call	Num1
	        ld	c,-1
Num1:	    ld	a,'0'-1
Num2:	    inc	a
	        add	hl,bc
	        jr	c,Num2
	        sbc	hl,bc
	        call putchar
	        ret 
	        
	        
crlf:       LD A, $0A
            CALL putchar
            LD A, $OD
            CALL putchar
            RET
            
            
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

; ****************************************************************
; Vector Table - holds the 16-bit target addresses of 
; the 96 possible functions as used by the dispatch routine
; 192 bytes
; ****************************************************************
            .ORG $8240
            
tabstart:                       ; Jump Table
            
             DEFW    space      ;
             DEFW    store      ; !   
             DEFW    dup_       ; "
             DEFW    lit_       ; #
			 DEFW    swap       ; $   
             DEFW    mod_       ; %   
             DEFW    and_       ; &
             DEFW    drop       ; '
			 DEFW    begin_     ; (  
             DEFW    end_       ; )
             DEFW    mul_       ; *   
             DEFW    add_       ; +
			 DEFW    quit       ; ,   
             DEFW    sub_       ; -
             DEFW    dot_       ; .
             DEFW    div_       ; /
             DEFW    num_       ; 0   
             DEFW    num_       ; 1  
             DEFW    num_       ; 2   
             DEFW    num_       ; 3
			 DEFW    num_       ; 4   
             DEFW    num_       ; 5   
             DEFW    num_       ; 6   
             DEFW    num_       ; 7
             DEFW    num_       ; 8   
             DEFW    num_       ; 9  
             DEFW    def_       ; :  
             DEFW    ret_       ; ;
			 DEFW    lt_        ; <
             DEFW    eq_        ; =   
             DEFW    gt_        ; >   
             DEFW    query      ; ?

			 DEFW    fetch      ; @   
             DEFW    $A420      ; A 
             DEFW    $A440      ; B
             DEFW    $A460      ; C
			 DEFW    $A480      ; D 
             DEFW    $A4A0      ; E
             DEFW    $A4C0      ; F
             DEFW    $A4E0      ; G
			 DEFW    $A500      ; H
             DEFW    $A520      ; I
             DEFW    $A540      ; J
             DEFW    $A560      ; K
			 DEFW    $A580      ; L
             DEFW    $A5A0      ; M
             DEFW    $A5C0      ; N
             DEFW    $A5E0      ; O
             DEFW    $A600      ; P
             DEFW    $A620      ; Q
             DEFW    $A640      ; R
             DEFW    $A660      ; S
			 DEFW    $A680      ; T
             DEFW    $A6A0      ; U
             DEFW    $A6C0      ; V
             DEFW    $A6E0      ; W
             DEFW    $A700      ; X
             DEFW    $A720      ; Y
             DEFW    $A740      ; Z
             DEFW    open       ; [
			 DEFW    quit       ; \
             DEFW    close      ; ]
             DEFW    xor_       ; ^
             DEFW    neg_       ; _

             DEFW    tick       ; `   
             DEFW    $A820   
             DEFW    $A840   
             DEFW    $A860
			 DEFW    $A880   
             DEFW    $A8A0   
             DEFW    $A8C0   
             DEFW    $A8E0
			 DEFW    $A900   
             DEFW    $A920   
             DEFW    $A940   
             DEFW    $A960
			 DEFW    $A980   
             DEFW    $A9A0   
             DEFW    $A9C0   
             DEFW    $A9E0 
             DEFW    $AA00   
             DEFW    $AA20   
             DEFW    $AA40   
             DEFW    $AA60
			 DEFW    $AA80   
             DEFW    $AAA0   
             DEFW    $AAC0   
             DEFW    $ABE0
             DEFW    $AB00   
             DEFW    $AB20   
             DEFW    $AB40   
             DEFW    save       ; {
			 DEFW    or_        ; |   
             DEFW    load       ; }   
             DEFW    inv_       ; ~   
             DEFW    del_       ; BS
			 
; **********************************************************************			 
             
; $ A000 Start of primitive routines 

; **********************************************************************

             .ORG  $A000
             
space:       
             JP      NEXT

            
             
store:       
             JP      NEXT
             
dup_:        POP     HL
             PUSH    HL
             PUSH    HL
             JP      NEXT
             
lit_:       
             JP      NEXT
             
swap:        POP     HL
             POP     DE
             PUSH    HL
             PUSH    DE
             JP      NEXT
             
drop:        
             JP       NEXT
            
mod_:        
             JP       NEXT

begin_:        
             JP       NEXT            

end_:        
             JP       NEXT
            
mul_:        
             JP       NEXT
            
div_:        
             JP       NEXT            

and_:        POP     DE                      ; 10t
		     POP     HL                      ; 10t
		     LD      A,E                     ; 4t
		     AND     L                       ; 4t
		     LD      L,A                     ; 4t
             LD      A,D                     ; 4t
             AND     H                       ; 4t
             LD      H,A                     ; 4t
             PUSH    HL                      ; 11t
             JP      NEXT                    ; 10t
		
		
or_: 		 POP     DE
		     POP     HL
		     LD      A,E
		     OR      L
		     LD      L,A
             LD      A,D
             OR      H
             LD      H,A
             PUSH    HL
             JP      NEXT
		
		
xor_:		 POP     DE
		     POP     HL
		     LD      A,E
		     XOR     L
		     LD      L,A
             LD      A,D
             XOR     H
             LD      H,A
             PUSH    HL
             JP      NEXT
             
add_:        POP     DE
             POP     HL
             ADD     HL,DE
             PUSH    HL
             JP      NEXT
             
sub_:       						; Subtract the value 2nd on stack from top of stack 
			 POP     HL
			 POP     DE
             LD A,   E
			 CPL					; Invert E
			 LD E,   A
			 LD A,   D
			 CPL					; Invert D
			 LD D,   A
			 INC     DE			    ; Add 1 to DE
			 ADD     HL,DE
			 PUSH    HL
			 JP      NEXT

inv_:								; Bitwise INVert the top member of the stack
			 POP     HL
             LD A,   L
			 CPL					; Invert L
			 LD L,   A
			 LD A,   H
			 CPL					; Invert H
			 LD H,   A
			 PUSH    HL
			 JP      NEXT
			 
neg_:       						; NEGate the value on top of stack (2's complement)
			 POP     HL
             LD A,   L
			 CPL					; Invert L
			 LD L,   A
			 LD A,   H
			 CPL					; Invert H
			 LD H,   A
			 INC     HL
			 PUSH    HL
			 JP      NEXT		             
             
             
dot_:        POP     HL
             CALL    printdec
             CALL    crlf
            ; JP      NEXT
             
quit:        RET   


num_:
            JP       NEXT
             
def_:
            JP       NEXT             
		            
ret_:
            JP       NEXT
            
lt_:
            JP       NEXT            

eq_:
            JP       NEXT            

gt_:
            JP       NEXT
            
query:
            JP       NEXT 
            
fetch:      
            JP       NEXT
            
open:      
            JP       NEXT

close:      
            JP       NEXT
            
tick:      
            JP       NEXT
            
save:       
            JP       NEXT
            
load:      
            JP       NEXT
            
del_:      
            JP       NEXT            
            
            