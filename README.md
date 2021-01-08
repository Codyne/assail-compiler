# Assail-compiler

This is a compiler for a very lisp-like language based somewhat on scheme. The compiler is written in Racket and when run on a file, will output x86_64 instructions which you then run through NASM to make your executable. I've borrowed [asm printer](http://www.cs.umd.edu/class/spring2020/cmsc430/code/hustle/asm/printer.rkt) and expanded it to add support for new functionality.

Install Racket and NASM

`sudo apt-get install racket nasm`

To compile a program run `racket compile-file.rkt example.srkt`, and then run the output assembly through NASM.

Alternatively, run this command to create your executable:

`racket compile-file.rkt example.srkt > example.s && nasm -f elf64 -g -o example.o example.s && ld example.o -o example-exe`

You will find an `example.srkt` file in this repo containing all the examples below, which when compiled and run will print the respective outputs.

Example programs:

`(print "Hello World!")` - prints the "Hello World!" string `Hello World!`

`(print "Hello") (print "World!")` - prints the "Hello\nWorld!" string `Hello\nWorld!`

`(print (string-ref "Hello World!" 6))` - gets the 7th letter in the string and prints it `W`

`(print (make-string 7 #\w))` - makes a string of 7 w chars and prints it `wwwwwww`

`(print (make-string 7 (string-ref "Hello world!" 6)))` - makes a string of 7 chars of the 7th letter in the string "Hello world!" and then prints the output `wwwwwww`

`(print (integer? 6))` - prints true `2`

`(print (integer? #\b))` - prints false `0`

`(print (= (/ 8 4) 2))` - prints true `2`

`(print (if (= (/ 8 4) 2) #t #f))` - prints true `2`

`(print (if (< (* 8 8) 64) #t #f))` - prints false `0`

`(print (abs -10))` - prints the absolute value of 10, `10`

`(print (cond [(zero? 1) 2] [(zero? (sub1 1)) 4] [else 3]))` - prints `4`

`(print (if (zero? (if (zero? 0) 1 0)) (if (zero? 1) 1 2) 7))` - prints `7`

`(let ((x 7)) (print x))` - stores 7 in variable x, and then prints the contents of x `7`

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
