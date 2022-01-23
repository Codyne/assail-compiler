# Assail-compiler

This is a compiler for a very lisp-like language based somewhat on scheme. The compiler is written in Racket and when run on a file, will output x86_64 instructions which you then run through NASM to make your executable. I've borrowed [asm printer](http://www.cs.umd.edu/class/spring2020/cmsc430/code/hustle/asm/printer.rkt) and expanded it to add support for new functionality.

Install Racket and NASM

`sudo apt-get install racket nasm`

To compile a program run `racket compile-file.rkt example.srkt`, and then run the output assembly through NASM.

Alternatively, run this command to create your executable:

`racket compile-file.rkt example.srkt > example.s && nasm -f elf64 -g -o example.o example.s && ld example.o -o example-exe`

You will find an `example.srkt` file in this repo containing all the examples below, which when compiled and run will print the respective outputs.

| Example Program | Description | Program Output |
| --------------- | -------------- | -------------- |
| `(print "Hello World!")` | Prints the string and newline | `Hello World!` |
| `(print "Hello") (print "World!")` | Prints both strings with a newline after each | `Hello\nWorld!` |
| `(print (string-ref "Hello World!" 6))` |  Returns the 7th char (0 indexed) in the string | `W` |
| `(print (make-string 7 #\w))` | Creates a string of 7 'w' chars | `wwwwwww` |
| `(print (integer? 6))` | Checks if argument is an integer. This returns true | `2` |
| `(print (integer? #\b))` | Checks if argument is an integer. This returns false |`0` |
| `(print (= (/ 8 4) 2))` | Checks if `8 / 4` is equal to `2`. This returns true | `2` |
| `(print (if (= (/ 8 4) 2) #t #f))` | Checks if the condition (1st argument) is true, and it is so it evaluates the 2nd argument |`2` |
|`(print (if (< (* 8 8) 64) #t #f))` | Checks if the condition (1st argument) is true, and it isn't so it evaluates the 3rd argument| `0` |
| `(print (abs -10))` | Abolsute value of `-10` | `10` |
| `(print (cond [(zero? 1) 2] [(zero? (sub1 1)) 4] [else 3]))` | Evaluates the conditions until one is true or the else is reached | `4` |
| `(print (if (zero? (if (zero? 0) 1 0)) (if (zero? 1) 1 2) 7))` | Example of using expressions inside if statement 2nd and 3rd arguments | `7` |
| `(let ((x 7)) (print x))` | Assigning the value `7` to the variable `x` and printing it | `7` |
| `(print (car (cons 1 2)))` | Creating a list and returning the 1st element | `1` |
| `(print (cdr (cons 1 2)))` | Creating a list and returning the last element | `2` |

supported types, keywords, and operations here:
```
type Token =
| Integer
| Char
| Boolean
| String
| '()
| `(variable ,Variable)
| `(keyword ,Keyword)
| `(prim ,Prim)
| 'lparen    ;(
| 'rparen    ;)
| 'lsquare   ;[
| 'rsquare   ;]
| 'eof       ;end of file

type Variable = Symbol (other than 'let, 'cond, etc.)

type Keyword =
| 'let
| 'let*
| 'cond
| 'else
| 'if
| 'print

type Prim = Prim1 | Prim2 | '-

type Prim1 =
| 'add1
| 'sub1
| 'zero?
| 'abs
| 'integer->char
| 'char->integer
| 'char?
| 'boolean?
| 'integer?
| 'string?
| 'box?
| 'empty?
| 'cons?
| 'box
| 'unbox
| 'car
| 'cdr
| 'string-length

type Prim2 =
| 'cons
| 'make-string
| 'string-ref
| '=
| '<
| '<=
| 'char=?
| 'boolean=?
| '+
| '*
| '/
```
