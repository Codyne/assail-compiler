# smallracket-compiler

it's a VERY simple lisp like language. started in my compilers class and expanded a bit afterwards. i've borrowed the [asm printer](http://www.cs.umd.edu/class/spring2020/cmsc430/code/hustle/asm/printer.rkt) and added to it for additional operations

you will need racket to be able to run this compiler.

`sudo apt-get install racket`

once you have racket, you can run the compiler on `your_file_here` with `racket compile-file.rkt your_file_here` and lastly run the output through nasm

Example programs:

`(print "Hello World!")` - prints the "Hello World!" string `Hello World!`

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
