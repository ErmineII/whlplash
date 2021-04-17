# Whlplash ########

https://chat.stackexchange.com/rooms/123064/whlplash

[toc]

## Commands #######

| p | s | description
|:-:|:-:|:-
|` `|` `| **no-op**
|*n*|*n*| **literal number** where *n*s are digits
|`.`|   | **Movement**
|   |*d*| where *d* is in `><^v`, change direction
|   |*m*| where *m* is in `|\/-`, mirror
|   |`#`| jump forward over single instruction
|   |`j`| relative jump to (2d offset on stack)
|   |`J`| absolute jump
|   |`@`| return/exit from program with exit code on top of stack or 0
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
|   |*d*| if *d* is in `{}MW` (`{}^v`?) turn `<>^v` respectively
|   |`|`| or
|   |`&`| and
|   |`X`| xor
|`!`|   | **I/O and system commands**
|   |`.`| output as default representation
|   |`,`| output as character
|   | ? | system command
|`~`|   | **Stack manipulation**
|   |`:`| dup
|   |`~`| swap
|   | ? | rot drop over nip tuck [etc.](http://wiki.laptop.org/go/Forth_stack_operators)
|` `|*a*| Define the function *a*. When *a* is called, the ip jumps to this place and continues on until it reaches a return instruction.

## Questions ######

### Implementation language? ###

### Array datatype? ###

Of course there would be a sequence datatype, but how would it be represented? There could be malloc and free like in C.
It should probably be 2d to fit the theme.

### Popping off an empty stack? ##

- Error, because this is modern :P

### Concurrency? ##

Probably not.

## Examples #######

```js
 F.>~:?!?W?:01+- F.+*.@
    .@01.< not that long

```

