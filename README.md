# MINT
A minimal interpreter in Z80 assembly for the RC2014 Micro.


MINT is a small Z80 assembly language program that provides a framework for a bytecode interpreter based on the printable ascii character set. The basic principles, once grasped could be applied to any other 8-bit micro, such as the 6502 or similar.


In my case I am using a Z80 based RC2014 "Micro", a single board computer that has a serial terminal interface based on the 68B50 ACIA. Credit should be given here to Grant Searle, Spencer Owen and Stephen C Cousins for making these retro systems available to the wider hobbyist community.


The framework consists of the minimum number of routines that will provide interactive support for developing simple applications. Once in place, it will mean that the user can develop applications without having to resort to native assembly language.


A basic serial terminal interface is provided, communicating with the familiar getchar and putchar routines. Further support routines are included to allow decimal and hexadecimal numerical output, decimal input, string printing etc.

MINT provides a simple interactive interpreter based on printable ascii characters. This provides a greater level of human readability compared to assembly language. It is designed to use single ascii character commands - for example, when the interpreter recieves the "+" character, it will perform a 16-bit integer addition. There are approximately 30 such commands, mostly consisting of arithmetic and punctuation characters.


mint_8 is the latest upload.  On the RC2014 Micro it loads at address $8000.

Mint consists of 4 main sections:

1.  A kernel between $8000 and $80FF that implements the bytecode interpreter and includes the serial ACIA drivers and routines for putchar, getchar and numerical input and output. The kernel is approximately 256 bytes long.

2.  A text buffer area beginning at $8100 and further storage for the User's definitions

3.  A vector table beginning at $8240 which allocates a code field address (CFA) to each of the primitive and user defined functions. The vector table is 192 bytes long, which allocates 2 byte addresses for each of the 96 functions.

4.  A storage area for the primitive function definitions- currently located at $A000.


Whilst these 4 areas are relatively widely spaced in RAM - this is only an artifact of the code development process. In reality, the 4 main sections may be concatenated such that the total memory requirement is about 1K bytes. 






