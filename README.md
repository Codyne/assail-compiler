# smallracket-compiler

it's a VERY simple lisp like language. started in my compilers class and expanded a bit afterwards. i've borrowed the [asm printer](http://www.cs.umd.edu/class/spring2020/cmsc430/code/hustle/asm/printer.rkt) and added to it for additional operations

you will need racket to be able to run this compiler.

`sudo apt-get install racket`

once you have racket, you can run the compiler on `your_file_here` with `racket compile-file.rkt your_file_here` and lastly run the output through nasm

Example programs:

`(print (integer? 6))` true `2`

`(print (integer? #\b))` false `0`

`(print (= (/ 8 4) 2))` true `2`

`(print (if (= (/ 8 4) 2) #t #f))` true `2`

`(print (if (< (* 8 8) 64) #t #f))` false `0`

`(print (abs -10))` for absolute value of 10, `10`

`(print (cond [(zero? 1) 2] [(zero? (sub1 1)) 4] [else 3]))` outputs `4`

`(print (if (zero? (if (zero? 0) 1 0)) (if (zero? 1) 1 2) 7))` outputs `7`

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
