;        Start of MINT micro-interpreter based on SIMPL
;        Ken Boak July 2021

;        Includes serial routines getchar and putchar
;        printstring
;        Printdec
;        printhex
;        crlf

;		 This version uses 6850 ACIA serial driver routines by Stephen C Cousins 


;        BC is instruction pointer
;        DE is working register and 2nd on stack
;        HL is working register and Top of stack
;        SP is data stack pointer

; **********************************************************************************

         TABSTART  EQU  $90
         
         stringbuf EQU  $8800
         
         
         .ORG $8000
         
        LD BC, $87FF            ; Instructions begin at $8800
        
NEXT:   LD HL, $0000            ; Clear HL to accept new number
        INC BC                  ; Increment the IP
        LD A, (BC)              ; Get the next character
        CP $30                  ; Less that $30
        JR C, notnum            ; Not a number
        CP $3A                  ; Greater or equal to $3A
        JR NC, notnum           ; Not a number
        
        SUB $30                 ; Form decimal digit
        ADD A,L                 ; Add into bottom of HL
        LD  L,A
      
number1:                               
        INC BC                  ; Increment IP
        LD A, (BC)              ; Get the next character
        CP $30                  ; Less that $30
        JR C, endnum            ; Not a number / end of number
        CP $3A                  ; Greater or equal to $3A
        JR NC, endnum           ; Not a number / end of number
       
times10:                        ; Multiply digit(s) in HL by 10
        ADD HL,HL               ; 2 X
        LD  DE,HL
        ADD HL,HL               ; 4 X
        ADD HL,HL               ; 8 X
        ADD HL,DE               ; 10 X
        
        SUB $30                 ; Form next decimal digit in A
        ADD A,L                 ; Add into bottom of HL
        LD  L,A
        
        JP  number1
        
        
endnum:
        PUSH HL                 ; Put the number on the stack

       ; CALL printdec          ; HL contains 16-bit integer
       ; CALL crlf
       ; RET                    ; Return to SCM
        
        
notnum:                         ; The character at IP is not a number

         PUSH BC                ; Push the current IP
         ADD A,A                ; Double A to index even addresses
         LD B, TABSTART         ; Start address of jump table         
         LD C,A                 ; Index into table
         LD A,(BC)              ; get low jump address
         LD L,A                 ; and put into L
         INC BC
         LD A,(BC)              ; Get high jump address
         LD H,A                 ; and put into H
         POP BC                 ; Get IP back
         JP (HL)                ; Jump to routine
         
; ---------------------------------------------------------------------        
         
        CALL getchar
        JP Z, $8000
        CALL putchar
        XOR  A
        JP   $08000
         
        LD HL,$0000
ldhl:        
        PUSH HL
        CALL printhex
        POP HL
        INC HL
        CALL crlf               ; add a CRLF
        LD B,00
        CALL delay
        JP  ldhl
        
delay:

        DJNZ delay
        RET
        
        
        CALL printstring
        JP  $8000
         
         

 ;        CALL serial_init
         
         
outchar:         
         
         CALL getchar
         JP Z,  outchar
         
         
         CALL putchar
        
        JP    outchar
         
         
         
       
		
		

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
            BIT  k6850RxRdy,A   ;Receive byte available
            RET  Z              ;Return Z if no character
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

; **********************************************************************
; **  End of driver: RC2014, Serial 6850 ACIA                         **
; **********************************************************************
 
 
            .ORG $9040          ; Jump Table
            
             DEFW    space  
             DEFW    store   
             DEFW    dup_       ; "
             DEFW    $A060
			 DEFW    swap       ; $   
             DEFW    $A0A0   
             DEFW    and_       ; &
             DEFW    $A0E0
			 DEFW    $A100   
             DEFW    $A120   
             DEFW    $A140   
             DEFW    add_       ; +
			 DEFW    quit       ; ,   
             DEFW    sub_       ; -
             DEFW    dot_       ; .
             DEFW    $A1E0 
             DEFW    $A200   
             DEFW    $A220   
             DEFW    $A240   
             DEFW    $A260
			 DEFW    $A280   
             DEFW    $A2A0   
             DEFW    $A2C0   
             DEFW    $A2E0
             DEFW    $A300   
             DEFW    $A320   
             DEFW    $A340   
             DEFW    $A360
			 DEFW    $A380   
             DEFW    $A3A0   
             DEFW    $A3C0   
             DEFW    $A3E0

			 DEFW    $A400   
             DEFW    $A420   
             DEFW    $A440   
             DEFW    $A460
			 DEFW    $A480   
             DEFW    $A4A0   
             DEFW    $A4C0   
             DEFW    $A4E0
			 DEFW    $A500   
             DEFW    $A520   
             DEFW    $A540   
             DEFW    $A560
			 DEFW    $A580   
             DEFW    $A5A0   
             DEFW    $A5C0   
             DEFW    $A5E0 
             DEFW    $A600   
             DEFW    $A620   
             DEFW    $A640   
             DEFW    $A660
			 DEFW    $A680   
             DEFW    $A6A0   
             DEFW    $A6C0   
             DEFW    $A6E0
             DEFW    $A700   
             DEFW    $A720   
             DEFW    $A740   
             DEFW    $A760
			 DEFW    $A780   
             DEFW    $A7A0   
             DEFW    $A7C0   
             DEFW    $A7E0

             DEFW    $A800   
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
             DEFW    $AB60
			 DEFW    $AB80   
             DEFW    $ABA0   
             DEFW    $ABC0   
             DEFW    $ABE0
             
; $ A000 Start of primitive routines 

             .ORG  $A000
             
space:       LD A, $21
             JP    NEXT

            
             
store:       LD A, $22
             JP    NEXT
             
             
             
dup_:        POP     HL
             PUSH    HL
             PUSH    HL
             JP      NEXT
             
swap:        POP     HL
             POP     DE
             PUSH    HL
             PUSH    DE
             JP      NEXT
             

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
             JP      NEXT
             
quit:        RET             
             
             
		            
             