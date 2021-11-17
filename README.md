# MINT

A Minimal Interpreter in Z80 assembly language for the RC2014 Micro and other simple Z80 systems such as the TEC-1.

## What is MINT ?

MINT is a tiny, stack based language inspired by Forth. On the Z80 the core of the language can be implemented in fewer than 1024 bytes of machine code - and it is relatively quick compared to other interpreted languages.

It uses reverse Polish notation (RPN) so you have to put the operands before the operator. It's just like the old HP calculators from 50 years ago.

If you want to add two numbers you just type:

```
123 456 + .
```

When you hit return the result will be displayed thus

```
00579

OK
```

OK confirms that the code has been executed and control has been passed back to the User.

## Fundamentals

An interpreter can be reduced to a switch-case structure contained within a loop.

With MINT, the instructions are just one byte long and a look-up table is used instead of a switch-case structure. When using an 8-bit microprocessor, such as the Z80, it is simpler and faster to handle 8-bit instructions, so MINT uses a bytecode system, rather than the 16-bit threaded code that is used by a conventional Forth.

In the example above `123 456 + .`

The numerical strings 123 and 456 are evaluated as 16-bit binary numbers and placed on the data stack. The plus symbol is interpreted as a jump to the routine that performs a 16-bit addition of the top two elements on the data stack, placing their sum on the top of the datastack. The dot character prints out the top value of the data stack, consuming it at the same time.

Any tiny language needs the basic arithmetic operations of ADD, SUBTRACT, MULTIPLY and DIVIDE. These are implemented as 16-bit integer operations and invoked using the familiar characters +, -, \* and /.

These are augmented by the bitwise Boolean operators AND, OR, XOR, INVERT and 2's complement NEGATE.

There are also the three comparison operators Greater Than, Less Than and Equal to, represented by symbols > < and =.

The top two elements on the stack will be compared, resulting in 1 if the comparison is TRUE and 0 if the comparison is FALSE.

With the comparison operators, it becomes possible to develop conditionally executed code, which forms the basis of program control words, such as IF, THEN, ELSE, and looping and branching structures.

## How MINT Works.

MINT is an interpreted language that uses printable ASCII characters as its "instructions". There are 95 such characters:

26 Uppercase letters - used as User Commands
26 Lowercase letters - used as User Variables
10 Numerals - used for number entry
33 arithmetic and punctuation symbols - used to select the program operation

The interpreter scans a text string, held in a text buffer, one character at a time. It then uses a look-up table to broadly categorise the current character into one of the above groups.

For each category of character there is a handling routine, which determines how the character should be processed.

NUMBERS

A number string such as 1234 will be scanned one digit at a time and converted into a 16-bit binary number using a routine called num\_ . The converted binary number is then placed on the top of the data stack which is used as a means of temporary storage, before being used later. Multiple numbers may be entered in a sequence separated by spaces: 1234 5678 3579 When the return key is pressed they will be processed in turn and each placed onto the stack. They may then be used as operands or parameters for a calculation or other function.

VARIABLES

User Variables are assigned to the lowercase alpha characters using a routine called var\_ The user variables are stored in an array of 26 pairs of bytes in RAM. The lowercase character is a shorthand way of addressing the pair of bytes that holds the variable. It is not usually necessary to know specifically at what address the variable is held at, as it can always be accessed using its name.

For example, the variable addressed by the lowercase character "a" is held in RAM locations 34816 and 34817. Variable "b" will be held in the next locations 34818 and 34819 and so on up to "z".

When a lowercase character is interpreted the variable handler routine converts it to a 16-bit address, and places that address on the top of the stack.

COMMANDS

User Commands are what gives MINT its power and flexibility. Each uppercase letter is a substitute for an address in RAM, where the users code routines are held. For example you may have a routine which produces a hexadecimal dump of the contents of memory. You choose to use the D command to initiate this routine, D for DUMP. You may also pass parameters to a user routine via the stack. In the case of a hex dump routine it would be common to give it the starting address of the section you want to dump, and this might be written 1234 D. On pressing return, the command will be interpreted and the dump routine will commence printing from location 1234. There are clearly 26 User Commands which is usually enough for most small applications.

PRIMITIVES

A primitive is a built in function, normally stored in ROM and not usually needed to be modified by the User. Primitives will include the familliar mathematical functions such as ADD, SUBtract, MULtiply and DIVide, and also boolean logic operations such as AND, OR, XOR and INVert.

There are also a small group of primitives that perform operations on the stack, DUP is used to duplicate the top item, DROP will remove the top item, making the second item available. SWAP will exchange the top two intems, effectively placing the second item on top.

In total, MINT contains 33 primitives which are executed when the interpreter finds the relevant symbol. Some of these will be commonly used arithmetic symbols like "+" and "-" Others are allocated to punctuation symbols. The full-stop, or dot character is used to print out the number held on the top of the stack.

## Using MINT

MINT was originally developed for the RC2014 Micro Z80 Single Board Computer. This board is supplied with a comprehensive Monitor program (The Small Computer Monitor by Stephen Cousins). A 32K ROM contains the monitor and BASIC between $0000 and $7FFF. The 32K RAM starts at $8000, and MINT is loaded in to run from address $8100.

MINT was assembled using asm80.com, an online 8-bit assembler. It will generate an Intel Hex file that can be pasted into RAM at addresss $8000 using a serial terminal program. We use TeraTerm when working within the windows environment and Serial when working on the Mac.

Once the MINT code image is pasted into RAM you can run it using the Go command "G8100"

On initialisation it will present a user prompt "OK" followed by a CR and LF. It is now ready to accept commands from the keyboard. MINT currently uses decimal numbers for calculations - a maximum integer of 65535.

MINT has about 30 built in commands called primitives. They are mostly allocated to arithmetical and punctuation symbols.

There are 26 User Defined Commands that use uppercase alpha characters A-Z

There are 26 User Variables that are assigned to lowercase alpha characters a-z

MINT turns the Z80 into a 16-bit Virtual Machine with 30 instructions, 26 Macros and 26 Registers (variables). This relieves you from the tedium of Z80 assembly language, and presents the user with a very compact, human readable, interactive, extendable bytecode language.

## Examples

Spaces are shown for clarity, but only necessary to separate consecutive number strings. Most other operators can be concatenated without spaces.

```
1234 5678 + . ; ADD 1234 to 5678 and print the result

1234 5678 - . ; Subtract 1234 from 5678 and print the result

1234 a! ; Store 1234 in the variable a

5678 b! ; Store 5678 in the variable b

b@ . ; print the value stored in b

a@ b@ + . ; add the contents of a to b and print the sum

a@ b! ; copy the contents of a into b
```

## Loops

0(this code will not be executed but skipped)
1(this code will be execute once)
10(this code will execute 10 times)

You can use the comparison operators < = and > to compare 2 values and conditionally execute the code between the brackets.

### LIST OF PRIMITIVES

Mint is a bytecode interpreter - this means that all of its instructions are 1 byte long. However, the choice of instruction uses printable ASCII characters, as a human readable alternative to assembly language. The interpreter handles 16-bit integers and addresses which is sufficient for small applications running on an 8-bit cpu.

There are roughly 30 punctuation and arithmetical symbols available in the printable ASCII codes. These are assigned to the primitive functions, from which more complex programs can be built.

### Maths Operators

| Symbol | Description                               | Effect   |
| ------ | ----------------------------------------- | -------- |
| +      | 16-bit integer addition ADD               | a b -- c |
| -      | 16-bit integer subtraction SUB            | a b -- c |
| \*     | 8-bit by 8-bit integer multiplication MUL | a b -- c |
| /      | 16-bit by 8-bit division DIV              | a b -- c |
| <      | 16-bit comparison LT                      | a b -- c |
| =      | 16 bit comparison EQ                      | a b -- c |
| >      | 16-bit comparison GT                      | a b -- c |
| {      | Shift the number to the left (2\*)        | a -- b   |
| }      | Shift the number to the right (2/)        | a -- b   |

### Logical Operators

| Symbol | Description                          | Effect   |
| ------ | ------------------------------------ | -------- |
| ~      | 16-bit bitwise inversion INV         | a -- b   |
| \_     | 16-bit negation (2's complement) NEG | a -- b   |
| &      | 16-bit bitwise AND                   | a b -- c |
| \|     | 16-bit bitwise OR                    | a b -- c |
| ^      | 16-bit bitwise XOR                   | a b -- c |

### Stack Operations

| Symbol | Description                                                                   | Effect       |
| ------ | ----------------------------------------------------------------------------- | ------------ |
| "      | Duplicate the top member of the stack DUP                                     | a -- a a     |
| '      | Drop the top member of the stack DROP                                         | a a -- a     |
| $      | Swap the top 2 members of the stack SWAP                                      | a b -- b a   |
| %      | Over - take the 2nd member of the stack and copy it onto the top of the stack | a b -- a b a |

### Input & Output Operations

| Symbol | Description                                               | Effect      |
| ------ | --------------------------------------------------------- | ----------- |
| #      | The following number is in hexadecimal                    | a --        |
| .      | Print the top member of the stack as a decimal number DOT | a --        |
| ,      | Print the number on the stack as a hexadecimal            | a --        |
| \`     | \`Everything between ticks is printed as a string\`       | --          |
| \\.    | non-destructively prints stack                            | --          |
| \\e    | emits a char to output                                    | val --      |
| \\I    | input from a I/O port                                     | port -- val |
| \\k    | read a char from input                                    | -- val      |
| \\n    | prints a newline to output                                | --          |
| \\O    | output to an I/O port                                     | val port -- |

### User Definitions

| Symbol | Description                   | Effect |
| ------ | ----------------------------- | ------ |
| :      | Define a new word DEF         |
| ;      | End of user definition END    |
| \\:    | TODO: starts defining a macro | --     |

### Loops and conditional execution

| Symbol | Description                                       | Effect |
| ------ | ------------------------------------------------- | ------ |
| (      | BEGIN a loop or conditionally executed code block | --     |
| )      | END a loop or conditionally executed code block   | --     |
| \\i    | returns index variable of current loop            | -- val |
| \\j    | returns index variable of outer loop              | -- val |

### Memory and Variable Operations

| Symbol | Description                                 | Effect        |
| ------ | ------------------------------------------- | ------------- |
| @      | FETCH a value from memory                   |               |
| !      | STORE a value to memory                     |               |
| [      | Begin an array definition                   | --            |
| ]      | End an array definition                     | -- adr nwords |
| \\[    | Begin a byte array definition               | --            |
| \\]    | End a byte array definition                 | -- adr nbytes |
| \\\`   | define a string                             | -- adr nchars |
| \\@    | FETCH a byte from memory                    |               |
| \\!    | STORE a byte to memory                      |               |
| \\+    | increments variable at address by an amount | val addr --   |

### Miscellaneous

| Symbol | Description                                   | Effect    |
| ------ | --------------------------------------------- | --------- |
| \\\\   | comment text, skips reading until end of line | --        |
| \\g    | execute mint code at address                  | addr -- ? |
| \\q    | quits from Mint interpreter                   | --        |
| \\x    | execute machine code at address               | addr -- ? |
| \\0    | start address of data stack                   | -- adr    |
| \\1    | heap pointer variable                         | -- adr    |
| \\2    | text input buffer pointer variable            | -- adr    |
| \\2    | show stack                                    | -- adr    |
| \\4    | is hex variable                               | -- adr    |

### Macros

| Symbol | Description  |
| ------ | ------------ |
| ^U     | toggle stack |
