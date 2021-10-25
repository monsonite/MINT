# MINT
A Minimal INTerpreter in Z80 assembly language for the RC2014 Micro and other simple Z80 systems such as the TEC-1.

## What is MINT ?

MINT is a tiny, stack based language based on Forth. On the Z80 it can be implemented in fewer than 1024 bytes of machine code - and it is lightning quick.

It is interactive and interpreted, so you don't have to wait for your code to compile, it happens immediately.

It uses reverse Polish notation (RPN) so you have to put the operands before the operator. It's just like the old HP calculators from 50 years ago.

If you want to add two numbers you just type:

123 456 + . 

When you hit return the result will be displayed thus

00579

OK

But MINT is more than just a glorified calculator - you can write real programs too.


## How MINT Works.

MINT is an interpreted language that uses printable ascii characters as its "instructions". There are 95 such characters:

26 Uppercase letters - used as User Commands
26 Lowercase letters - used as User Variables
10 Numerals - used for number entry
33 arithmetic and punctuation symbols - used to select the program operation

The interpreter scans a text string, held in a text buffer, one character at a time. It then uses a look-up table to broadly categorise the current character into one of the above groups.

For each category of character there is a handling routine, which determines how the character should be processed.

NUMBERS

A number string such as 1234 will be scanned one digit at a time and converted into a 16-bit binary number using a routine called num_ .  The converted binary number is then placed on the top of the data stack which is used as a means of temporary storage, before being used later. Multiple numbers may be entered in a sequence separated by spaces: 1234 5678 3579 When the return key is pressed they will be processed in turn and each placed onto the stack. They may then be used as operands or parameters for a calculation or other function.

VARIABLES

User Variables are assigned to the lowercase alpha characters using a routine called var_  The user variables are stored in an array of 26 pairs of bytes in RAM. The lowercase character is a shorthand way of addressing the pair of bytes that holds the variable. It is not usually necessary to know specifically at what address the variable is held at, as it can always be accessed using its name.

For example, the variable addressed by the lowercase character "a" is held in RAM locations 34816 and 34817. Variable "b" will be held in the next locations  34818 and 34819 and so on up to "z".

When a lowercase character is interpreted the variable handler routine converts it to a 16-bit address, and places that address on the top of the stack.

COMMANDS

User Commands are what gives MINT its power and flexibility. Each uppercase letter is a substitute for an address in RAM, where the users code routines are held. For example you may have a routine which produces a hexadecimal dump of the contents of memory. You choose to use the D command to initiate this routine, D for DUMP. You may also pass parameters to a user routine via the stack. In the case of a hex dump routine it would be common to give it the starting address of the section you want to dump, and this might be written 1234 D. On pressing return, the command will be interpreted and the dump routine will commence printing from location 1234. There are clearly 26 User Commands which is usually enough for most small applications.


## mint1_10 is the latest upload.



## Using MINT

MINT was developed for the RC2014 Micro Z80 Single Board Computer.  This board is supplied with a comprehensive Monitor program (The Small Computer Monitor by Stephen Cousins). A 32K ROM contains the monitor and BASIC between $0000 and $7FFF. The 32K RAM starts at $8000, and MINT is loaded in to run from address $8080.

MINT was assembled using asm80.com, an online 8-bit assembler. It will generate an Intel Hex file that can be pasted into RAM at addresss $8000 using a serial terminal program. I use TeraTerm when working within the windows environment.

Once the MINT code image is pasted into RAM you can run it using the Go command "G8080"

On initialisation it will present a user prompt "OK" followed by a CR and LF. It is now ready to accept commands from the keyboard.  MINT currently uses decimal numbers for calculations - a maximum integer of 65535.

MINT has about 30 built in commands called primitives. They are mostly allocated to arithmetical and punctuation symbols.

There are 26 User Defined Commands that use uppercase alpha characters A-Z

There are 26 User Variables that are assigned to lowercase alpha characters a-z

The variable i is assigned to the loop counter.

MINT turns the Z80 into a 16-bit Virtual Machine with 30 instructions, 26 Macros and 26 Registers (variables). This relieves you from the tedium of Z80 assembly language, and presents the user with a very compact, human readable, interactive, extendable bytecode language.

## Examples

Spaces are shown for clarity, but only necessary to separate consecutive number strings. Most other operators can be concatenated without spaces.

1234 5678 + .               ; ADD 1234 to 5678 and print the result

1234 5678 - .               ; Subtract 1234 from 5678 and print the result

1234 a!                     ; Store 1234 in the variable a

5678 b!                     ; Store 5678 in the variable b

b@ .                        ; print the value stored in b

a@ b@ + .                   ; add the contents of a to b and print the sum

a@ b!                       ; copy the contents of a into b

##Loops

0(this code will not be executed but skipped)
1(this code will be execute once)
10(this code will execute 10 times)

You can use the comparison operators < = and > to compare 2 values and conditionally execute the code between the brackets.

## MINT - a Description


MINT is a small Z80 assembly language program that provides a framework for a bytecode interpreter based on the printable ascii character set. The basic principles, once grasped could be applied to any other 8-bit micro, such as the 6502 or similar.

MINT is a simple tool to provide an interactive user interface through a set of commands - based on single printable ascii characters. MINT allocates the uppercase letter A to Z as User Definable  Commands. Using these commands, the User can develop simple applications, which could be anything from a few commands to exercise I/O, flash LEDs or even drive stepper motors. 


However simple or complex the application, MINT provides an easy means to interract with the hardware, where ideas can be tried out quickly without having to keep going around the edit, compile, reflash cycle.

Using familliar uppercase characters allows the User to create shortcut commands that make sense to the specific application.  For example a simple hex monitor application might use the following:

A   Specify an Address
E   Edit memory
D   Show a hex Dump of the memory
G   Go to the address and run the program.

For exercising simple I/O you might like to define O as send to an Output port and I as read an Input port. A more complex system might be a 2D-plotter or 3D printer where you might choose commands X, Y and Z to move the head to a specific position.

MINT provides a framework that makes it easy to define new commands and attach whatever functional code you wish to them.

Commands are of little use without parameters, so MINT allows several parameters to be attached to a command. Parameters are placed in order on a data stack before the command is called. Parameters are 16-bit positive integers - so any value between 0 and 65535.

In the hex Dump example, you might choose to specify a starting address and the number of bytes to dump:

8192 256 D   this would display 256 bytes starting at address 8192

Commands are executed only when the return key is pressed.

********************************************************************************************
In my case I am using a Z80 based RC2014 "Micro", a single board computer that has a serial terminal interface based on the 68B50 ACIA. Credit should be given here to Grant Searle, Spencer Owen and Stephen C Cousins for making these retro systems available to the wider hobbyist community.


The framework consists of the minimum number of routines that will provide interactive support for developing simple applications. Once in place, it will mean that the user can develop applications without having to resort to native assembly language.


A basic serial terminal interface is provided, communicating with the familiar getchar and putchar routines. Further support routines are included to allow decimal and hexadecimal numerical output, decimal input, string printing etc.

MINT provides a simple interactive interpreter based on printable ascii characters. This provides a greater level of human readability compared to assembly language. It is designed to use single ascii character commands - for example, when the interpreter recieves the "+" character, it will perform a 16-bit integer addition. There are approximately 30 such commands, mostly consisting of arithmetic and punctuation characters.


mint1_8A is the latest upload. It fits into under 1K bytes of RAM.  

On the RC2014 Micro it loads at address $8000, but is executes from $8100

Mint consists of 4 main sections:

1.  A kernel between $8000 and $80FF that implements the bytecode interpreter and includes the serial ACIA drivers and routines for putchar, getchar and numerical input and output. The kernel is approximately 256 bytes long.

2.  A text buffer area beginning at $8100 and further storage for the User's definitions

3.  A vector table beginning at $8240 which allocates a code field address (CFA) to each of the primitive and user defined functions. The vector table is 192 bytes long, which allocates 2 byte addresses for each of the 96 functions.

4.  A storage area for the primitive function definitions- currently located at $A000.


Whilst these 4 areas are relatively widely spaced in RAM - this is only an artifact of the code development process. In reality, the 4 main sections may be concatenated such that the total memory requirement is about 1K bytes. 


Operation

Mint is a bytecode interpreter - this means that all of its instructions are 1 byte long. However, the choice of instruction uses printable ascii characters, as a human readable alternative to assembly language. The interpreter handles 16-bit integers and addresses which is sufficient for small applications running on an 8-bit cpu.

There are roughly 30 punctuation and arithmetical symbols available in the printable ascii codes. These are assigned to the primitive functions, from which more complex programs can be built.

Maths Operators:

+   16-bit integer addition  ADD


-   16-bit integer subtraction  SUB


*   8-bit by 8-bit integer multiplication  MUL


/   16-bit by 8-bit division DIV


%   16-bit by 8-bit modulo MOD


<   16-bit comparison LT


=   16 bit comparison EQ


>   16-bit comparison GT



Logical Operators:

~   16-bit bitwise inversion INV


(#) 16-bit negation (2's complement) NEG


&   16-bit bitwise AND


|   16-bit bitwise OR


^   16-bit bitwise XOR



Stack Operations:

"   Duplicate the top member of the stack DUP


'   Drop the top member of the stack DROP


$   Swap the top 2 members of the stack  SWAP


.   Print the top member of the stack as a decimal number DOT


Memory Operations:

@   FETCH a value from memory


!   STORE a value to memory


User Definitions:

:   Define a new word  DEF


;   End of user definition  END

Variables:

}   SAVE the top of stack to a variable


{   LOAD the top of stack from a variable


Loops and conditional execution:

(   BEGIN a loop or conditionally executed code block


)   END a loop or conditionally executed code block


[   OPEN an array


]   CLOSE an array

Miscellaneous:

_   STRING  _Everything between underscores is printed as a string_


\   QUIT   Return to the monitor program


?   QUERY

,   Separate array members


`   TICK Execute the last conditional code







