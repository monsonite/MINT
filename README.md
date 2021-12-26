# MINT

A Minimal Interpreter in Z80 assembly language for small Z80 systems such as the TEC-1 and RC2014.

## What is MINT ?

MINT is a tiny, stack based language based on Forth. On the Z80 it can be implemented in fewer than 2048 bytes of machine code - and it is relatively quick compared to other interpreted languages.

It uses reverse Polish notation (RPN) so you have to put the operands before the operator. It's just like the old HP calculators from 50 years ago.

If you want to add two numbers you just type:

123 456 + .

When you hit return the result will be displayed thus

00579

> This is the cursor / prompt that confirms that the code has been executed and control has been passed back to the User.

## Fundamentals

Like other small interpreted languages, the intention of MINT is to create a 16-bit virtual machine by combining the mostly 8-bit operations available on the Z80, to provide 16-bit integer arithmetic and variable handling.

The language needs the basic arithmetic operations of ADD, SUBTRACT, MULTIPLY and DIVIDE. These are implemented as 16-bit integer operations and invoked using the familiar characters +, -, \* and /.

These are augmented by the bitwise Boolean operators AND, OR, XOR, INVERT and 2's complement NEGATE.

With MINT, these instructions are just one byte long and a look-up table is used instead of a switch-case structure. When using an 8-bit microprocessor, such as the Z80, it is simpler and faster to handle 8-bit instructions, so MINT uses a bytecode system, rather than the 16-bit threaded code that is used by a conventional Forth.

In the example above 123 456 + .

The numerical strings 123 and 456 are evaluated as 16-bit binary numbers and placed on the data stack. The plus symbol is interpreted as a jump to the routine that performs a 16-bit addition of the top two elements on the data stack, placing their sum on the top of the data stack. The dot character prints out the top value of the data stack, consuming it at the same time.

In addition to the arithmetic and boolean operations, there are also the three comparison operators Greater Than, Less Than and Equal to, represented by symbols > < and =.

The top two elements on the stack will be compared, resulting in 1 if the comparison is TRUE and 0 if the comparison is FALSE.

With the comparison operators, it becomes possible to develop conditionally executed code, which forms the basis of program control words, such as IF, THEN, ELSE, and looping and branching structures.

In total there are approximately 30 characters that are recognised as the internal instruction set, or primitives. From these characters the user can construct further definitions to extend the usefulness of the language.

## How MINT Works.

MINT is an interpreted language that uses printable ascii characters as its "instructions". There are 95 such characters:

- 10 Numerals - 0-9 used for decimal number entry
- 11 Alphanumerics - 0-F used for hexadecimal number entry (only uppercase chars are recognized as hex digits)
- 26 lowercase letters - used as User Variables
- 26 system variables - most are available for general use
- 33 arithmetic and punctuation codes - used to select the program operation aka "primitives"
- 22 "alternate" codes - to extend the basic set of alphanumeric characters, these are prefixed by \
- 26 uppercase letters - used as User Commands

The interpreter scans a text string, held in a text buffer, one character at a time. It then uses a look-up table to broadly categorise the current character into one of the above groups.

For each category of character there is a handling routine, which determines how the character should be processed.

NUMBERS

A number string such as 1234 will be scanned one digit at a time and converted into a 16-bit binary number using a routine called num\_ . The converted binary number is then placed on the top of the data stack which is used as a means of temporary storage, before being used later. Multiple numbers may be entered in a sequence separated by spaces: 1234 5678 3579 When the return key is pressed they will be processed in turn and each placed onto the stack. They may then be used as operands or parameters for a calculation or other function.

VARIABLES

User Variables are assigned to the lowercase alpha characters using a routine called var\_ The user variables are stored in an array of 26 pairs of bytes in RAM. The lowercase character is a shorthand way of addressing the pair of bytes that holds the variable. It is not usually necessary to know specifically at what address the variable is held at, as it can always be accessed using its name.

When a lowercase character is interpreted the variable handler routine converts it to a 16-bit address, and places that address on the top of the stack.

SYSTEM VARIABLES
System variables contain values which MINT uses internally but are available for programmatic use. These are the lowercase letters preceded by a \ e.g. \a, \b, \c etc. However Mint only uses a few of these variables so the user may use the other ones as they like.

PRIMITIVES

A primitive is a built-in function, normally stored in ROM and not usually needed to be modified by the User. Primitives will include the familiar mathematical functions such as ADD, SUBtract, MULtiply and DIVide, and also boolean logic operations such as AND, OR, XOR and INVert.

There are also a small group of primitives that perform operations on the stack, DUP is used to duplicate the top item, DROP will remove the top item, making the second item available. SWAP will exchange the top two items, effectively placing the second item on top.

In total, MINT contains 33 primitives which are executed when the interpreter finds the relevant symbol. Some of these will be commonly used arithmetic symbols like "+" and "-" Others are allocated to punctuation symbols. The full-stop, or dot character is used to print out the number held on the top of the stack.

ALTERNATE CODES
Because ASCII provides only a limited set of symbols to use as primitives, MINT extends the basic set with a set of symbols prefixed by a \. An alternate code is any symbol or uppercase letter starting with a \ e.g. \+ \D etc. Alternate lowercase letters serve as system variables

USER COMMANDS

User Commands are what gives MINT its power and flexibility. Each uppercase letter can be assigned a routine written by the user in the Mint language. For example you may have a routine which produces a hexadecimal dump of the contents of memory. You could define a routine at D for this DUMP operation. You may also pass parameters to a user routine via the stack. In the case of a hex dump routine it would be common to give it the starting address of the section you want to dump, and this might be written 1234 D. On pressing return, the command will be interpreted and the dump routine will commence printing from location 1234. There are clearly 26 User Commands which is usually enough for most small applications.

## Using MINT on the TEC-1

MINT was designed for for small Z80 based systems but specifically with the small memory configuration of the TEC-1 single board computer. It is only 2K to work with the original TEC-1 and interfaces to the serial interface via a simple adapter.

On initialisation it will present a user prompt ">" followed by a CR and LF. It is now ready to accept commands from the keyboard.

## Using MINT on the RC2014

MINT was developed for the RC2014 Micro Z80 Single Board Computer. This board is supplied with a comprehensive Monitor program (The Small Computer Monitor (SCM) by Stephen Cousins). A 32K ROM contains the monitor and BASIC between $0000 and $7FFF. The 32K RAM starts at $8000, and MINT is loaded in to run from address $8000.

Install the Intel Hex file RC2014_MINT.hex by pasting it into the SCM. At the Ready prompt, type G8000 to execute.

If necessary, you can use the serial getchar and putchar routines that are available within the Small Computer Monitor

See the User Manual pages 45 and 46 on how this is done

https://smallcomputercentral.files.wordpress.com/2018/05/scmon-v1-0-userguide-e1-0-0.pdf

MINT was assembled using asm80.com, an online 8-bit assembler. It will generate an Intel Hex file that can be pasted into RAM at address $8000 using a serial terminal program. I use TeraTerm when working within the windows environment.

Once the MINT code image is pasted into RAM you can run it using the Go command "G8000"

On initialisation it will respond:

MINT V1.0

On initialisation it will present a user prompt ">" followed by a CR and LF. It is now ready to accept commands from the keyboard.

## Examples

Spaces are shown for clarity, but only necessary to separate consecutive number strings. Most other operators can be concatenated without spaces.

1234 5678 + . ; ADD 1234 to 5678 and print the result

1234 5678 - . ; Subtract 1234 from 5678 and print the result

1234 a! ; Store 1234 in the variable a

5678 b! ; Store 5678 in the variable b

b@ . ; print the value stored in b

a@ b@ + . ; add the contents of a to b and print the sum

a@ b! ; copy the contents of a into b

##Loops

0(this code will not be executed but skipped)
1(this code will be execute once)
10(this code will execute 10 times)

You can use the comparison operators < = and > to compare 2 values and conditionally execute the code between the brackets.

ARRAYS

An array of 16-bit numbers can be defined by enclosing them within square brackets:

[1 2 3 4 5 6 7 8 9 0]

Defining an array puts its start address and length onto the stack

These can then be allocated to a variable, which acts as a pointer to the array in memory

[1 2 3 4 5 6 7 8 9 0] $ a!

The swap $ is used to get the starting address onto the top of the stack and then store that into the variable a.

To fetch the Nth member of the array, we can create a colon definition N

:N @ $ {+ @. ;

### LIST OF PRIMITIVES

Mint is a bytecode interpreter - this means that all of its instructions are 1 byte long. However, the choice of instruction uses printable ASCII characters, as a human readable alternative to assembly language. The interpreter handles 16-bit integers and addresses which is sufficient for small applications running on an 8-bit cpu.

### Maths Operators

| Symbol | Description                               | Effect   |
| ------ | ----------------------------------------- | -------- |
| -      | 16-bit integer subtraction SUB            | a b -- c |
| {      | shift the number to the left (2\*)        | a -- b   |
| }      | shift the number to the right (2/)        | a -- b   |
| /      | 16-bit by 8-bit division DIV              | a b -- c |
| \_     | 16-bit negation (2's complement) NEG      | a -- b   |
| \*     | 8-bit by 8-bit integer multiplication MUL | a b -- c |
| \\\_   | sign of number                            | n -- b   |
| \>     | 16-bit comparison GT                      | a b -- c |
| +      | 16-bit integer addition ADD               | a b -- c |
| <      | 16-bit comparison LT                      | a b -- c |
| =      | 16 bit comparison EQ                      | a b -- c |

### Logical Operators

| Symbol | Description                  | Effect   |
| ------ | ---------------------------- | -------- |
| \|     | 16-bit bitwise OR            | a b -- c |
| &      | 16-bit bitwise AND           | a b -- c |
| ^      | 16-bit bitwise XOR           | a b -- c |
| ~      | 16-bit bitwise inversion INV | a -- b   |

Note: logical NOT can be achieved with 0=

### Stack Operations

| Symbol | Description                                                          | Effect         |
| ------ | -------------------------------------------------------------------- | -------------- |
| '      | drop the top member of the stack DROP                                | a a -- a       |
| "      | duplicate the top member of the stack DUP                            | a -- a a       |
| \\D    | returns the depth of the stack                                       | -- n           |
| \\R    | rotate the top 2 members of the stack ROT                            | a b c -- b c a |
| %      | over - take the 2nd member of the stack and copy to top of the stack | a b -- a b a   |
| $      | swap the top 2 members of the stack SWAP                             | a b -- b a     |

### Input & Output Operations

| Symbol | Description                                               | Effect      |
| ------ | --------------------------------------------------------- | ----------- |
| ,      | print the number on the stack as a hexadecimal            | a --        |
| .      | print the top member of the stack as a decimal number DOT | a --        |
| \\E    | emits a char to output                                    | val --      |
| \\I    | input from a I/O port                                     | port -- val |
| \\K    | read a char from input                                    | -- val      |
| \\N    | prints a CRLF to output                                   | --          |
| \\O    | output to an I/O port                                     | val port -- |
| \\P    | non-destructively prints stack                            | --          |
| \\Z    | print definition by number                                | n --        |
| \`     | print the literal string between \` and \`                | --          |
| #      | the following number is in hexadecimal                    | a --        |

### User Definitions

| Symbol  | Description                | Effect |
| ------- | -------------------------- | ------ |
| ;       | end of user definition END |        |
| :<CHAR> | define a new word DEF      |        |
| ?<CHAR> | get the address of the def | -- adr |
| \{      | enter group NUM            | num -- |
| \}      | exit group                 | --     |

NOTE:
<CHAR> is an uppercase letter immediately following operation which is the name of the definition
<NUM> is the group number. There are currently 5 groups numbered 0 - 4

### Loops and conditional execution

| Symbol | Description                                       | Effect |
| ------ | ------------------------------------------------- | ------ |
| (      | BEGIN a loop or conditionally executed code block | n --   |
| )      | END a loop or conditionally executed code block   | --     |
| \\(    | beginIFTE \\(`true`)(`false`)                     | b --   |
| \\B    | if true break out of loop                         | b --   |
| \\i    | loop counter variable                             | -- adr |
| \\j    | outer loop counter variable                       | -- adr |

### Memory and Variable Operations

| Symbol | Description                                 | Effect        |
| ------ | ------------------------------------------- | ------------- |
| !      | STORE a value to memory                     | val adr --    |
| [      | begin an array definition                   | --            |
| ]      | end an array definition                     | -- adr nwords |
| @      | FETCH a value from memory                   | -- val        |
| \\!    | STORE a byte to memory                      | val adr --    |
| \\[    | begin a byte array definition               | --            |
| \\@    | FETCH a byte from memory                    | -- val        |
| \\+    | increments variable at address by an amount | val adr --    |

### System Variables

| Symbol | Description                          | Effect |
| ------ | ------------------------------------ | ------ |
| \\a    | data stack start variable            | -- adr |
| \\b    | base16 flag variable                 | -- adr |
| \\c    | text input buffer pointer variable   | -- adr |
| \\d    | start of user definitions            | -- adr |
| \\h    | heap pointer variable                | -- adr |
| \\i    | See: Loops and conditional execution | -- adr |
| \\j    | See: Loops and conditional execution | -- adr |

### Miscellaneous

| Symbol | Description                                   | Effect   |
| ------ | --------------------------------------------- | -------- |
| \\\\   | comment text, skips reading until end of line | --       |
| \\G    | execute mint code at address                  | adr -- ? |
| \\Q    | quits from Mint interpreter                   | --       |
| \\X    | execute machine code at address               | adr -- ? |

### Control keys

| Symbol | Description                     |
| ------ | ------------------------------- |
| ^B     | toggle base decimal/hexadecimal |
| ^E     | edit a definition               |
| ^H     | backspace                       |
| ^J     | re-edit                         |
| ^L     | list definitions                |
| ^P     | print stack                     |
