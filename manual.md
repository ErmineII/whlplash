# Whlplash ########

https://chat.stackexchange.com/rooms/123064/whlplash

https://github.com/ErmineII/whlplash

[toc]

## Programs #######

A program is made up of (per source file):

 - Optionally, a unnamed function without a header that will be run at program
   start.
 - Any number of headers of the form ` @fn` (a space, `@`, function name),
   each followed by a function body.
 - A function body is a positive number of lines, which are split into cells
   two characters wide (a trailing single character is ignored). These lines
   must each either start with a non-space character or have a space as a
   second character.
 - Any line may start with a space and a tilde `~`, then implementation-
   specific directives.
 - Any line may start with a space and a hash `#`. These lines will be ignored.

## Commands #######

| p | s | description
|:-:|:-:|:-
|` `|` `| **no-op**
|*n*|*n*| **literal number** where *n*s are digits
|`.`|   | **Movement**
|   |*d*| where *d* is in `><^v`, change direction
|   |*m*| where *m* is in `|\/-`, mirror
|   |`#`| jump forward over single instruction
|   |`@`| return/exit from program with exit code on top of stack or 0
|   |`"`| literal string like `."text" ` or `."text with odd characters!"`
|`+`|   | **Arithmetic**
|   |`+`| add top two numbers
|   |`-`| subtract top two numbers
|   |`*`| multiply top two numbers
|   |`/`| divide
|   |`!`| negate
|`?`|   | **Boolean stuff**
|   |`=`| equal
|   | ? | not equal (`~` maybe)
|   |`!`| not
|   |`<`| less
|   |`>`| greater
|   |`[`| less or equal
|   |`]`| greater or equal
|   |`#`| jump forward if true
|   |`j`| relative jump if true (cond dx dy)
|   |`J`| conditional absolute jump
|   |`*`| top-of-the-stack times repeat next instr
|   |`@`| return if true
|   |*d*| if *d* is in `{}^v` turn `<>^v` respectively if the top of the stack is true
|   |`|`| or
|   |`&`| and
|   |`X`| xor
|`!`|   | **I/O and system commands**
|   |`.`| output as default representation
|   |`,`| output as character
|   | ? | system command
|`~`|   | **Stack manipulation**
|   |`:`| dup `( a -- a a )`
|   |`~`| swap/exch `( a b -- b a )`
|   |`r`| rot `( a b c -- b c a )`
|   |`R`| -rot `( a b c -- c a b )`
|   |`o`| over `( a b -- a b a )`
|   |`t`| tuck `( a b -- b a b )`
|   | ? | [others?](http://wiki.laptop.org/go/Forth_stack_operators)
|`:`|   | **Array commands**
|   |`+`| append 2 arrays
|   |`@`| popping `I` then `A` push `A[I]`
|   |`!`| pops an array, index, and value and sets the value at the index in the array (I'm not sure of the order on the stack)
|   |`]`| pops `N` and collects the top `N` stack elements into an array
|`'`|*c*| Push the literal character *c*

## Questions ######

### Implementation language? ###

### Array datatype? ###

Of course there would be a sequence datatype, but how would it be represented? There could be malloc and free like in C.
It should probably be 2d to fit the theme.

### Popping off an empty stack? ##

- Error, because this is modern :P

### Concurrency? ##

Probably not.

### Wrapping ######

Horizontal wrapping, but errors instead of vertical wrapping.

## Examples #######

Factorial
```
.>~:?!?v?:01+- F.+*.@  the " F"
  .@01.<
```

Fibonacci: `(a b n -- fibonacci(n))` where `a` and `b` are `0` and `1` respectively for the normal sequence.
```
~:00?=?v~:01?=?v~R~t++~r
.@~$~$.<  ~r  .<
```
