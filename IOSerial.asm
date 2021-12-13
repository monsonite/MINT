        ; ROM code
        ; Targets:
        ; TEC-1,TEC-1D,TEC-1F,Southern Cross,RC2014
        ; Memory Map: 2k ROM/RAM, 8K ROM/RAM, RC2014
        ; Serial: Bit Bang, 6850 ACIA
        
.if  TEC_1
.if  BITBANG

        ; bit bang baud rate constants @ 4MHz
        B300:	.EQU	0220H
        B1200:	.EQU	0080H
        B2400:	.EQU	003FH
        B4800:	.EQU	001BH
        B9600:	.EQU	000BH

.else ;6850

        ;
        ; 6850 ACIA registers
        ;----------------------
        CONTROL         .EQU      $80   ;(write) 
        STATUS          .EQU      $80   ;(read)
        TDR             .EQU      $81   ;(write)
        RDR             .EQU      $81   ;(read)
        ;
        ; control register bits
        ;----------------------
        ;
        ;clock divisor
        ;
        MRESET  .EQU  $03        ;master reset the ACIA
        ; DIV_0    .EQU  $00        ;CLOCK/1
        ; DIV_16   .EQU  $01        ;CLOCK/16
        DIV_64   .EQU  $02        ;CLOCK/64
        ;
        ; format select
        ;
        F7E2    .EQU   $00        ;7 data bits, EVEN parity, 2 stop bits (1+7+1+2= 11 bits)
        F7O2    .EQU   $04        ;7 data bits, ODD parity, 2 stop bits (1+7+1+2= 11 bits)
        F7E1    .EQU   $08        ;7 data bits, EVEN parity, 1 stop bit (1+7+1+1= 10 bits)
        F7O1    .EQU   $0C        ;7 data bits, ODD parity, 1 stop bit (1+7+1+1= 10 bits)
        F8N2    .EQU   $10        ;8 data bits, NO parity, 2 stop bits (1+8+0+2= 11 bits)
        F8N1    .EQU   $14        ;8 data bits, NO parity, 1 stop bit (1+8+0+1= 10 bits)
        F8E1    .EQU   $18        ;8 data bits, EVEN parity, 1 stop bit (1+8+1+1= 11 bits)
        F8O1    .EQU   $1C        ;8 data bits, ODD parity,1 stop bit (1+8+1+1= 11 bits)
        ;
        ; transmitter control
        ;
        RTSLID .EQU   $00        ;RTS LOW, transmit interrupt disabled
        RTSLIE .EQU   $20        ;RTS LOW, transmit interrupt enabled
        RTSHID .EQU   $40        ;RTS HIGH, transmit interrupt disabled
        RTSLIDB .EQU  $60        ;RTS LOW, transmit interrupt disabled and 'break' transmitted
        ;
        ; receiver interrupt
        ;
        RIE    .EQU   $80        ;receiver interrupt enabled
        ;
        ; status register bits
        ;---------------------
        RDRF   .EQU   0          ;receive data register full
        TDRE   .EQU   1          ;transmit data register empty
        DCD    .EQU   2          ;data carrier detect
        CTS    .EQU   3          ;clear to send
        FE     .EQU   4          ;framing error
        OVRN   .EQU   5          ;overrun
        PE     .EQU   6          ;parity error
        IRQ    .EQU   7          ;interrupt request

.endif
.endif

; I/O port addresses

.if TEC_1
        KEYBUF:      .EQU 00H             ;MM74C923N KEYBOARD ENCODER
        SCAN:        .EQU 01H             ;DISPLAY SCAN LATCH
        DISPLY:      .EQU 02H             ;DISPLAY LATCH
        PORT3:       .EQU 03H             ;ST3 (8X8), STROBE (RELAY BOARD) DATLATCH (DAT BOARD)
        PORT4:       .EQU 04H             ;ST4 (8X8), LCD 'E' (DAT BOARD)
        PORT5:       .EQU 05H
        PORT6:       .EQU 06H
        PORT7:       .EQU 07H             ;ENABLE/DISABLE SINGLE STEPPER (IF INSTALLED)
.else ;SC
        IO0:         .EQU 80H             ;IO PORT 0
        IO1:         .EQU 81H             ;IO PORT 1
        IO2:         .EQU 82H             ;IO PORT 2
        IO3:         .EQU 83H             ;IO PORT 3
        DISPLY:      .EQU 84H             ;DISPLAY LATCH
        SCAN:        .EQU 85H             ;DISPLAY SCAN LATCH
        KEYBUF:      .EQU 86H             ;KEYBOARD BUFFER
        IO7:         .EQU 87H             ;ENABLE/DISABLE SINGLE STEPPER (IF INSTALLED)
.endif

; ASCII codes
ESC:     .EQU   1BH
CR:      .EQU   0DH
LF:      .EQU   0AH

        .ORG ROMSTART
;reset
RSTVEC:
        JP	RESET
;RST 1
    	.ORG	ROMSTART+$08
    	PUSH	HL
    	LD	HL,(RST08)
    	JP	(HL)
    
;RST 2
        .ORG ROMSTART+$10
    	PUSH	HL
    	LD	HL,(RST10)
    	JP	(HL)

;RST 3
        .ORG ROMSTART+$18 
    	PUSH	HL
    	LD	HL,(RST18)
    	JP	(HL)
    
;RST 4
        .ORG ROMSTART+$20
    	PUSH	HL
    	LD	HL,(RST20)
    	JP	(HL)

;RST 5
    	.ORG ROMSTART+$28
    	PUSH	HL
    	LD	HL,(RST28)
    	JP	(HL)

;RST 6
    	.ORG ROMSTART+$30
        PUSH	HL
    	LD	HL,(RST30)
    	JP	(HL)

;RST 7 Interrupt
    	.ORG	ROMSTART+$38
    	PUSH	HL
    	LD	HL,(INTVEC)
    	JP	(HL)
        RETI

        .ORG    ROMSTART+$40

;hexadecimal to 7 segment display code table
.if TEC_1

sevensegment:
            .DB 0EBH,28H,0CDH,0ADH ;0,1,2,3
            .DB 2EH,0A7H,0E7H,29H ;4,5,6,7
            .DB 0EFH,2FH,6FH,0E6H ;8,9,A,B
            .DB 0C3H,0ECH,0C7H,47H ;C,D,E,F
.else ;SC

sevensegment:
            .DB 3FH,06H,5BH,4FH ;0,1,2,3
            .DB 66H,6DH,7DH,07H ;4,5,6,7
            .DB 7FH,6FH,77H,7CH ;8,9,A,B
            .DB 39H,5EH,79H,71H ;C,D,E,F
.endif


;---------------
; BIT TIME DELAY
;---------------
;DELAY FOR ONE SERIAL BIT TIME
;ENTRY : HL = DELAY TIME
; NO REGISTERS MODIFIED
;
PWRUP:   
        LD    hl,$2000
BITIME:
        PUSH  HL
        PUSH  DE
        LD    DE,0001H
BITIM1:  
        SBC   HL,DE
        JP    NC,BITIM1
        POP   DE
        POP   HL
IntRet:  
        RET

;RST 8  Non Maskable Interrupt
        .ORG ROMSTART+$66
        PUSH	HL
        LD	HL,(NMIVEC)
        JP	(HL)


.if  BITBANG

;------------------------
; SERIAL TRANSMIT ROUTINE
;------------------------
;TRANSMIT BYTE SERIALLY ON DOUT
;
; ENTRY : A = BYTE TO TRANSMIT
;  EXIT : NO REGISTERS MODIFIED
;

TxChar:
TXDATA:
    	PUSH	AF
    	PUSH	BC
    	PUSH	HL
    	LD	HL,(BAUD)
    	LD	C,A
;
; TRANSMIT START BIT
;
	XOR	A
	OUT	(SCAN),A
	CALL	BITIME
;
; TRANSMIT DATA
;
	LD	B,08H
	RRC	C
NXTBIT:	
    RRC	C	;SHIFT BITS TO D6,
	LD	A,C	;LSB FIRST AND OUTPUT
	AND	40H	;THEM FOR ONE BIT TIME.
	OUT	(SCAN),A
	CALL	BITIME
	DJNZ	NXTBIT
;
; SEND STOP BITS
;
    LD	A,40H
    OUT	(SCAN),A
    CALL  BITIME
    CALL	BITIME
	POP	HL
	POP	BC
	POP	AF
	RET
;-----------------------
; SERIAL RECEIVE ROUTINE
;-----------------------
;RECEIVE SERIAL BYTE FROM DIN
;
; ENTRY : NONE
;  EXIT : A= RECEIVED BYTE IF CARRY CLEAR
;
; REGISTERS MODIFIED A AND F
;
RxChar:
RXDATA:
	PUSH	BC
	PUSH	HL
;
; WAIT FOR START BIT 
;
RXDAT1: IN	A,(KEYBUF)
	    BIT	7,A
	    JR	NZ,RXDAT1	;NO START BIT
;
; DETECTED START BIT
;
	LD	HL,(BAUD)
	SRL	H
	RR	L 	;DELAY FOR HALF BIT TIME
	CALL 	BITIME
	IN	A,(KEYBUF)
	BIT	7,A
	JR	NZ,RXDAT1	;START BIT NOT VALID
;
; DETECTED VALID START BIT,READ IN DATA
;
	LD	B,08H
RXDAT2:	
    LD	HL,(BAUD)
	CALL	BITIME	;DELAY ONE BIT TIME
	IN	A,(KEYBUF)
	RL	A
	RR	C	;SHIFT BIT INTO DATA REG
	DJNZ	RXDAT2
	LD	A,C
	OR	A	;CLEAR CARRY FLAG
    POP	HL
    POP	BC
	RET
    
.else ;6850
;
; transmit a character in a
;--------------------------
TXDATA:
TxChar:  
        push  bc
        ld    b,a                   ;save the character  for later
TxChar1: 
        in    a,(STATUS)            ;get the ACIA status
        bit   1,a
;        bit   TDRE,a                ;is the TDRE bit high?
        jr    z,TxChar1             ;no, the TDR is not empty
        ld    a,b                   ;yes, get the character
        out   (TDR),a               ;and put it in the TDR
        pop   bc
        ret
;
; receive  a character in a
;---------------------------------
RXDATA:
RxChar:  
        in    a,(STATUS)         ;get the ACIA status
        bit   0,a
;        bit   RDRF,a             ;is the RDRF bit high?
        jr    z,RxChar           ;no, the RDR is empty
        in    a,(RDR)            ;yes, read the received char
        ret
.endif

.if LOADER
    ;   .ORG   ROMSTART + $0700
;-----------------------
; RECEIVE INTEL HEX FILE
;-----------------------
INTELH:	
    LD	IX,BUF
;
; WAIT FOR RECORD MARK
;
INTEL1:	
    XOR	A
	LD	(IX+3),A	;CLEAR CHECKSUM
	CALL	RXDATA	;WAIT FOR THE RECORD MARK
	CP	':'	;TO BE TRANSMITTED
	JR	NZ,INTEL1	;NOT RECORD MARK
;
; GET RECORD LENGTH
;
	CALL	GETBYT
	LD	(IX+0),A	;NUMBER OF DATA BYTES
;
; GET ADDRESS FIELD
;
	CALL	GETBYT
	LD	(IX+2),A	;LOAD ADDRESS HIGH BYTE
	CALL	GETBYT
	LD	(IX+1),A	;LOAD ADDRESS LOW BYTE
;
; GET RECORD TYPE
;
	CALL	GETBYT
	JR	NZ,INTEL4	;END OF FILE RECORD
;
; READ IN THE DATA
;
	LD	B,(IX+0)	;NUMBER OF DATA BYTES
	LD	H,(IX+2)	;LOAD ADDRESS HIGH BYTE
	LD	L,(IX+1)	;LOAD ADDRESS LOW BYTE

INTEL2:	
    CALL	GETBYT	;GET DATA BYTE
	LD	(HL),A	;STORE DATA BYTE
	INC	HL
	DJNZ	INTEL2	;LOAD MORE BYTES
;
; GET CHECKSUM AND COMPARE
;
	LD	A,(IX+3)	;CONVERT CHECKSUM TO
	NEG		;TWO'S COMPLEMENT
	LD	(IX+4),A	;SAVE COMPUTED CHECKSUM
	CALL	GETBYT
	LD	(IX+3),A	;SAVE RECORD CHECKSUM
	CP	(IX+4)	;COMPARE CHECKSUM
	JR	Z,INTEL1	;CHECKSUM OK,NEXT RECORD
    RET             ;NZ=CHECKSUM ERROR
;
; END OF FILE RECORD
;
INTEL4:	
    LD	A,(IX+3)	;CONVERT CHECKSUM TO
	NEG		;TWO'S COMPLEMENT
	LD	(IX+4),A	;SAVE COMPUTED CHECKSUM
	CALL	GETBYT
	LD	(IX+3),A	;SAVE EOF CHECKSUM
	CP	(IX+4)	;COMPARE CHECKSUM
	RET  	    ;NZ=CHECKSUM ERROR
;--------------------------
; GET BYTE FROM SERIAL PORT
;--------------------------
GETBYT:	
    PUSH	BC
	CALL	RXDATA
	BIT	6,A
	JR	Z,GETBT1
	ADD	A,09H
GETBT1:	
    AND	0FH
	SLA 	A
	SLA	A
	SLA	A
	SLA	A
	LD	C,A
;
; GET LOW NYBBLE
;
	CALL	RXDATA
	BIT	6,A
	JR	Z,GETBT2
	ADD	A,09H
GETBT2	AND	0FH
	OR	C
	LD	B,A
	ADD	A,(IX+3)
	LD	(IX+3),A	;ADD TO CHECKSUM
	LD	A,B
	AND	A	;CLEAR CARRY
    POP	BC
	RET
.endif

; in this example code just wait for an INTEL Hex file download
;just going to send a char to let you know I'm here
.if LOADER

Load:  
        ld     a,'L'  ; L for load
        call   TxChar
        call INTELH
        jp   z,RAMSTART          ;assume the downloaded code starts here
        ld   a,'0'   ;0 is false
        call TxChar
        jr   load    ;if at first you don't succeed...
.endif

getchar:
        LD HL,(GETCVEC)
        JP (HL)

putchar:
        PUSH HL
        LD HL,(PUTCVEC)
        EX (SP),HL
        RET

RESET:   
        ld SP,stack
        LD HL,IntRet
    	LD (RST08),HL
    	LD (RST10),HL
    	LD (RST18),HL
    	LD (RST20),HL
    	LD (RST28),HL
    	LD (RST30),HL
        LD (INTVEC),HL
        LD (NMIVEC),HL

        LD HL,RXDATA
        LD (GETCVEC),HL
        LD HL,TXDATA
        LD (PUTCVEC),HL

.if TEC_1
.if BITBANG = 0

        ld    a,MRESET
        out   (CONTROL),a           ;reset the ACIA

.endif
.endif

        call PWRUP
        IM  1
        EI

.if TEC_1
.if BITBANG

;inline serial initialisation
        LD    A,$40
        LD    C,SCAN
        OUT   (C),A
        LD    HL,B4800
        LD    (BAUD),HL

.else ;6850      

        ld     a,RTSLID+F8N2+DIV_64
        out   (CONTROL),a           ;initialise ACIA  8 bit word, No parity 2 stop divide by 64 for 115200 baud

.endif
.endif
        

        