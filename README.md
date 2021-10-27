# MINT

A Minimal INTerpreter in Z80 assembly language for the RC2014 Micro, TEC-1 and other simple Z80 systems.

## What is MINT ?

MINT is a tiny, stack based language based on Forth. It implements a 16-bit virtual machine and provides integer 16-bit arithmetic operations. It shares many commonalities with Forth, but has a greatly simplified execution model when compared to Forth. 

There is no dictionary structure, no parsing of multi-character strings (apart from numbers) and no compilation mode, where source code would be converted into threaded code. The number of primitive functions is reduced to about 30, and User functions are limited to 26, by the use of uppercase characters.

This means that the size of the interpreter is considerably smaller than a full Forth. On the Z80 it can be implemented in fewer than 1024 bytes of machine code - and it is relatively quick, compared to other interpreted languages such as Tiny BASIC.

It's compact size and modularity means that it is relatively easy to port to other 8-bit microprocessors such as the 1802, 6800, 6809, 6502 etc. On a 16-bit microprocessor, the implementation would be greatly simplified.

As it is an interpreted language, you don't have to wait for your code to compile. What you type in is executed immediately after the return key is pressed. This allows small code snippets to be tested interactively, and eliminates the frustrating edit-compile-flash-test cycle.

Like Forth, it uses reverse Polish notation (RPN) so you have to put the operands before the operator. It's just like the old HP calculators from 50 years ago.

If you want to add two numbers you type:

123 456 + . 

When you hit return the result will be displayed thus

00579

OK

Further examples are shown below.


## How MINT Works.

MINT is an interpreted language that uses printable ascii characters as its "instructions". There are 95 such characters:

26 Uppercase letters - used as User Commands
26 Lowercase letters - used as User Variables
10 Numerals - used for number entry
33 arithmetic and punctuation symbols - used to select the program operation

There are also 32 non-printable ascii codes between $00 and $1F. Two of these are the familliar carriage return and line feed, but the other 30 non-printable codes can be used to generate system calls to hardware specific routines, for example. 

The interpreter scans a text string, held in a text buffer, one character at a time. It then uses a look-up table to broadly categorise the current character into one of the above groups.

For each category of character there is a handling routine, which determines how the character should be processed.

NUMBERS

A number string such as 1234 will be scanned one digit at a time and converted into a 16-bit binary number using a routine called num_ .  The converted binary number is then placed on the top of the data stack which is used as a means of temporary storage, before being used later. Multiple numbers may be entered in a sequence separated by spaces: 1234 5678 3579 When the return key is pressed they will be processed in turn and each placed onto the stack. They may then be used as operands or parameters for a calculation or other function.

VARIABLES

User Variables are assigned to the lowercase alpha characters using a routine called var_  The user variables are stored in an array of 26 pairs of bytes in RAM. The lowercase character is a shorthand way of addressing the pair of bytes that holds the variable. It is not usually necessary to know specifically at what address the variable is held at, as it can always be accessed using its name.

For example, the variable addressed by the lowercase character "a" is held in RAM locations 34816 and 34817. Variable "b" will be held in the next locations  34818 and 34819 and so on up to "z".

When a lowercase character is interpreted the variable handler routine converts it to a 16-bit address, and places that address on the top of the stack.

If you want to see this address, you can use the dot operator to print it out e.g.  a.

You can then access that variable using the fetch @ and store ! operators.   e.g.   a@ b!

The a@ b! sequence will fetch the value stored at a, put it on the stack and then store it into the variable b.

You can initialise a variable by typing a number onto the stack and storing it to the variable e.g. 1234 d!

COMMANDS

User Commands are what gives MINT its power and flexibility. Each uppercase letter is a substitute for an address in RAM, where the users code routines are held. For example you may have a routine which produces a hexadecimal dump of the contents of memory. You choose to use the D command to initiate this routine, D for DUMP. You may also pass parameters to a user routine via the stack. In the case of a hex dump routine it would be common to give it the starting address of the section you want to dump, and this might be written 1234 D. On pressing return, the command will be interpreted and the dump routine will commence printing from location 1234. There are clearly 26 User Commands which is usually enough for most small applications.

PRIMITIVES

A primitive is a built in function, normally stored in ROM and not usually needed to be modified by the User. Primitives will include the familliar mathematical functions such as ADD, SUBtract, MULtiply and DIVide, and also boolean logic operations such as AND, OR, XOR and INVert. 

There are also a small group of primitives that perform operations on the stack, DUP is used to duplicate the top item, DROP will remove the top item, making the second item available. SWAP will exchange the top two intems, effectively placing the second item on top.

In total, MINT contains 33 primitives which are executed when the interpreter finds the relevant symbol. Some of these will be commonly used arithmetic symbols like "+" and "-" Others are allocated to punctuation symbols. The full-stop, or dot character is used to print out the number held on the top of the stack. 

THE USER INTERFACE

MINT is designed to work with a lightweight serial interface to a terminal program. By lightweight, there are only two routines needed, getchar and putchar, sometimes called CIN and COUT for character in and character out.

In its idle state, the interpreter sits in a loop waiting for keyboard input. Characters typed at the keyboard are placed consecutively into a buffer until the return key is pressed. Characters are echoed to the terminal to show the user's input.

Following the newline character, the interpreter fetches each character in turn and makes a 4-way decision on how to handle it.

THE INTERPRETER  

MINT uses a large look-up table to convert each character in turn to a unique operation.

The first 256 bytes form a large look-up table, which is used to decode all of the printable ascii characters (and some non-printable ones) into jump addresses or variable storage addresses.

So if the interpreter comes across a "+" character, the look-up table will convert this into the starting address of the ADD routine. These routines are called the primitives and there are about 30 of them hard coded into the ROM. 

They perform the math and logic functions like ADD, SUB, MUL, DIV, AND, OR, XOR etc to name only a few. They also allow the usual stack operations like DUP, DROP, SWAP and control the fetching and storing of variables. LOOPs and other control structures are also coded into the primitives.

The primitives are tightly packed into 2 pages (512) bytes of ROM and using a neat trick called a trampoline bounce to allow each primitive to be addressed with just an 8-bit address. This saves code and is faster to execute.

Other sections of the look up table convert the uppercase characters into the jump addresses for the User Definitions A-Z, and the lowercase characters into the storage address of the 26 variables a-z.

The final 256 bytes contains the code that runs the actual interpreter, converting numbers to binary and providing print routines and the serial communications routines.

## Using MINT

Once the MINT code image is pasted into RAM you can run it using the Go command "G8100"

On initialisation it will present a user prompt "OK" followed by a CR and LF. It is now ready to accept commands from the keyboard.  MINT currently uses decimal numbers for calculations - a maximum integer of 65535.

MINT has about 30 built in commands known as primitives. They are mostly allocated to arithmetical and punctuation symbols.

There are 26 User Defined Commands that use uppercase alpha characters A-Z

There are 26 User Variables that are assigned to lowercase alpha characters a-z

The variable i is assigned to the loop counter.

MINT turns the Z80 into a 16-bit Virtual Machine with 30 instructions, 26 Macros and 26 Registers (variables). This relieves you from the tedium of Z80 assembly language, and presents the user with a very compact, human readable, interactive, extendable bytecode language.

## Examples

Spaces are shown for clarity, but only necessary to separate consecutive number strings. Most other operators can be concatenated without spaces.

1234 5678 + .                ADD 1234 to 5678 and print the result

1234 5678 - .                Subtract 1234 from 5678 and print the result

1234 a!                      Store 1234 in the variable a

5678 b!                      Store 5678 in the variable b

b@ .                         Print the value stored in b

a@ b@ + .                    Add the contents of a to b and print the sum

a@ b!                        Copy the contents of a into b

##Loops

0(this code will not be executed but skipped)
1(this code will be execute once)
10(this code will execute 10 times)

For example use the loop function to print out numbers 0 to 9999 to the terminal is:

0a!10000(a@.a@1+a!)  

Breaking this down into recognisable chunks, as whitespace is optional:

0 a!      Store 0 in variable a

10000(    Set loop counter to 10000 and begin a loop

a@.       Fetch the value from a and print it out

a@ 1+     Fetch it again and add 1

a!        Store it back in variable a

)         End the Loop


You can use the comparison operators < = and > to compare 2 values and conditionally execute the code between the brackets.

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

}   SAVE the top of stack to the return stack


{   LOAD the top of stack from the return stack

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







