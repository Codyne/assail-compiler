#lang racket
(provide (all-defined-out))

;; An immediate is anything ending in #b0000
;; All other tags in mask #b111 are pointers

(define result-shift     3)
(define result-type-mask (sub1 (arithmetic-shift 1 result-shift)))
(define type-imm         #b000)
(define type-box         #b001)
(define type-pair        #b010)
(define type-string      #b011)

(define imm-shift        (+ 2 result-shift))
(define imm-type-mask    (sub1 (arithmetic-shift 1 imm-shift)))
(define imm-type-int     (arithmetic-shift #b00 result-shift))
(define imm-type-bool    (arithmetic-shift #b01 result-shift))
(define imm-type-char    (arithmetic-shift #b10 result-shift))
(define imm-type-empty   (arithmetic-shift #b11 result-shift))

(define imm-val-false    imm-type-bool)
(define imm-val-true     (bitwise-ior (arithmetic-shift 1 (add1 imm-shift)) imm-type-bool))

;; Allocate in 64-bit (8-byte) increments, so pointers
;; end in #b000 and we tag with #b001 for boxes, etc.

(define (compile e)
  `(_start
    ,@(compile-exprs e '())
    (mov rax 60)
    (mov rdi 0)
    syscall
    err
    (mov rax 60)
    (mov rdi, 1)
    syscall

    _printNewline
    (mov rax 1)
    (mov rdi 1)
    (mov rdx 1)
    (mov rsi 10)
    (push rsi)
    (mov rsi rsp)
    syscall
    (pop rsi)
    ret
    _printString
    (pop r10);get this return address out of my way i swear to god
    (pop rdi)
    (pop rdi);get the string length
    (shr rdi 5);shift to get rid of int tag
    (mov r8 0)
    (mov r9 rdi)
    _printStringLoop
    (cmp r8 r9)
    (je _printStringEnd)
    (mov rax 1)
    (mov rdi 1)
    (mov rdx 1)
    ;(mov rsi rsp)
    (pop rsi)
    (shr rsi 5)
    (push rsi)
    (mov rsi rsp)
    syscall
    (pop rsi)
    (add r8 1)
    (jmp _printStringLoop)
    _printStringEnd
    (pop rsi);get rid of that null terminator
    (push r10);welcome back ;)
    (call _printNewline)
    ret
    ;;adding subroutine to print integers
    _printChar
    (push rdi)    
    (mov rax 1)
    (mov rdi 1)
    (mov rdx 1)
    (mov rsi rsp)
    syscall
    (call _printNewline)
    (pop rax)
    ret
    _printInt
    (push rdi)
    (mov rax rdi)
    (mov rcx 10)
    (mov r9 0)
    (mov r10 0)
    (mov r11 digits)
    _printIntPushLoop
    (add r9 1)
    (mov rdx 0)
    (div rcx)
    (push rdx)
    (cmp rax 0)
    (jne _printIntPushLoop)
    _printIntPopLoop
    (pop rax)
    (mov rcx rax)
    (add rcx 48)
    (mov [r11] cl)
    (inc r11)
    (add r10 1)
    (cmp r9 r10)
    (jne _printIntPopLoop)
    _printIntEnd
    (mov rcx 10)
    (mov [r11] cl)
    (add r10 1)
    (mov rax 1)
    (mov rdi 1)
    (mov rsi digits)
    (mov rdx r10)
    syscall
    (pop rdi)
    ret
    ))

(define (compile-exprs exprs c)
  (match exprs
    [(cons h '()) (append c (compile-e h '()))]
    [(cons h t) (compile-exprs t (append c (compile-e h '())))]
    )
  )

(define (compile-e e c)
  (match e
    [(? imm? i)            (compile-imm i)]
    [(? string? str)       (compile-string str c)]
    [(? symbol? x)         (compile-var x c)]
    [`(print ,e0)          (compile-print e0 c)]
    [`(box ,e0)            (compile-box e0 c)]
    [`(unbox ,e0)          (compile-unbox e0 c)]
    [`(cons ,e0 ,e1)       (compile-cons e0 e1 c)]
    [`(car ,e0)            (compile-car e0 c)]
    [`(cdr ,e0)            (compile-cdr e0 c)]
    [`(add1 ,e0)           (compile-add1 e0 c)]
    [`(sub1 ,e0)           (compile-sub1 e0 c)]
    [`(zero? ,e0)          (compile-zero? e0 c)]
    [`(if ,e0 ,e1 ,e2)     (compile-if e0 e1 e2 c)]
    [`(+ ,e0 ,e1)          (compile-+ e0 e1 c)]
    [`(- ,e0 ,e1)          (compile-+- e0 e1 c)]
    [`(* ,e0 ,e1)          (compile-* e0 e1 c)]
    [`(/ ,e0 ,e1)          (compile-/ e0 e1 c)]
    [`(abs ,e0)            (compile-abs e0 c)]
    [`(- ,e0)              (compile-- e0 c)]
    [`(integer->char ,e0)  (compile-ich e0 c)]
    [`(char->integer ,e0)  (compile-chi e0 c)]
    [`(integer? ,i)        (compile-int? i c)]
    [`(boolean? ,b)        (compile-bool? b c)]
    [`(char? ,ch)          (compile-char? ch c)]
    [`(string? ,str)       (compile-string? str c)]
    [`(string-ref ,str ,i) (compile-string-ref str i c)]
    [`(string-length ,str) (compile-string-length str c)]
    [`(make-string ,i ,ch) (compile-make-string i ch c)]
    [`(box? ,bx)           (compile-box? bx c)]
    [`(empty? ,emp)        (compile-empty? emp c)]
    [`(cons? ,cn)          (compile-cons? cn c)]
    [`(= ,i1 ,i2)          (compile-= i1 i2 c)]
    [`(< ,i1 ,i2)          (compile-< i1 i2 c)]
    [`(<= ,i1 ,i2)         (compile-<= i1 i2 c)]
    [`(char=? ,ch1 ,ch2)   (compile-char=? ch1 ch2 c)]
    [`(boolean=? ,b1 ,b2)  (compile-boolean=? b1 b2 c)]
    [(cons 'cond ls)       (compile-cond ls c)]
    [(cons 'let (cons ls (cons e1 '()))) (let_helper ls e1 c)]
    [(cons 'let* (cons ls (cons e1 '()))) (let_helper ls e1 c)]
    )
  )

(define (imm? x)
  (or (integer? x)
      (boolean? x)
      (char? x)
      (equal? ''() x)))

(define (type-pred? x)
  (memq x '(integer?
            char?
            empty?
            boolean?
            box?
            cons?)))

(define (compile-imm i)
  `((mov rax ,(imm->bits i))))

(define (imm->bits i)
  (match i
    [(? integer? i) (arithmetic-shift i imm-shift)]
    [(? char? c)    (+ (arithmetic-shift (char->integer c) imm-shift) imm-type-char)]
    [(? boolean? b) (if b imm-val-true imm-val-false)]
    [''()           imm-type-empty]))

;;(define (compile-int? i) `((mov rax ,(if (integer? i) imm-val-true imm-val-false))))
(define (compile-int? i c)
  (let ((c0 (compile-e i c))
        (l0 (gensym)))
    `(,@c0
      (and rax ,imm-type-mask)
      (cmp rax ,imm-type-int)
      (mov rax ,imm-val-false)
      (jne ,l0)
      (mov rax ,imm-val-true)
      ,l0)
      )
    )

;;(define (compile-bool? b) `((mov rax ,(if (boolean? b) imm-val-true imm-val-false))))
(define (compile-bool? b c)
  (let ((c0 (compile-e b c))
        (l0 (gensym)))
    `(,@c0
      (and rax ,imm-type-mask)
      (cmp rax ,imm-type-bool)
      (mov rax ,imm-val-false)
      (jne ,l0)
      (mov rax ,imm-val-true)
      ,l0)
      )
    )

;;(define (compile-char? ch) `((mov rax ,(if (char? ch) imm-val-true imm-val-false))))
(define (compile-char? ch c)
  (let ((c0 (compile-e ch c))
        (l0 (gensym)))
    `(,@c0
      (and rax ,imm-type-mask)
      (cmp rax ,imm-type-char)
      (mov rax ,imm-val-false)
      (jne ,l0)
      (mov rax ,imm-val-true)
      ,l0)
      )
    )

;;(define (compile-string? str) `((mov rax ,(if (string? str) imm-val-true imm-val-false))))
(define (compile-string? str c)
  (let ((c0 (compile-e str c))
        (l0 (gensym)))
    `(,@c0
      (and rax ,result-type-mask)
      (cmp rax ,type-string)
      (mov rax ,imm-val-false)
      (jne ,l0)
      (mov rax ,imm-val-true)
      ,l0)
      )
    )

(define (compile-string str c)
  (if (< 0 (string-length str))
      (let ((c0 (compile-e (string-length str) c))
            (c1 (compile-str str (string-length str) (- (string-length str) 1) c)))
        `(,@c1
          ,@c0
          (push rax)
          (mov rax rsp)
          (or rax ,type-string)
          )
        )
      (let ((c0 (compile-e (string-length str) c)))
        `(,@c0
          (push rax)
          (mov rax rsp)
          (or rax ,type-string)
          )
        )
      )
  )

(define (compile-str str len pos c)
  (if (< 0 pos)
      (let ((c0 (compile-e (string-ref str pos) c))
            (c1 (compile-str str len (- pos 1) c)))
        `(,@c0
          (push rax)
          ,@c1)
        )
      (let ((c0 (compile-e (string-ref str pos) c)))
        `(,@c0
          (push rax)
          )
        )
      )
  )

(define (compile-string-ref str i c)
  (if (and (string? str) (integer? i))
      (let ((c0 (compile-e i c))
            (c1 (compile-e str c)))
        `(,@c0
          ,@assert-integer
          ;;(add rax 1)
          ;;(mov rbx rax)
          ,@c1
          ,@assert-string
          (xor rax ,type-string)
          (mov rbx (offset rax 0))
          (or rbx ,imm-type-int)
          (cmp rbx ,(arithmetic-shift i imm-shift))
          (jle err)
          (mov rbx ,(arithmetic-shift -1 imm-shift))
          (cmp rbx ,(arithmetic-shift i imm-shift))
          (jge err)
          (mov rax (offset rax ,(+ i 1)))
          )
        )
      `(
        (jmp err)
        )
      )
  )

(define (compile-string-length str c)
  (let ((c0 (compile-e str c)))
    `(,@c0
      ,@assert-string
      (xor rax ,type-string)
      (mov rax (offset rax 0))
      )
    )
  )

(define (compile-make-string i ch c)
  (if (< 0 i)
      (let ((c0 (compile-e i c))
            (c1 (compile-e ch c))
            (l0 (gensym)))
        `(,@c0
          (mov r10 0)
          (push r10)
          (mov r9 rax)
          ,@c1
          (mov r10 rax)
          (mov r8 0)
          ,l0
          (push r10)
          (add r8 1)
          (cmp r8 r9)
          (jl ,l0)
          (push r9)
          (or rax ,type-string)))
      (`(jmp err))
      )
  )

(define (compile-box? bx c)
  (let ((c0 (compile-e bx c))
        (l0 (gensym)))
    `(,@c0
      (and rax ,result-type-mask)
      (cmp rax ,type-box)
      (mov rax ,imm-val-false)
      (jne ,l0)
      (mov rax ,imm-val-true)
      ,l0)
    )
  )

(define (compile-empty? emp c)
  (let ((c0 (compile-e emp c))
        (l0 (gensym)))
    `(,@c0
      (and rax ,imm-type-mask)
      (cmp rax ,imm-type-empty)
      (mov rax ,imm-val-false)
      (jne ,l0)
      (mov rax ,imm-val-true)
      ,l0)
    )
  )

(define (compile-cons? cn c)
  (let ((c0 (compile-e cn c))
        (l0 (gensym)))
    `(,@c0
      (and rax ,result-type-mask)
      (cmp rax ,type-pair)
      (mov rax ,imm-val-false)
      (jne ,l0)
      (mov rax ,imm-val-true)
      ,l0)
    )
  )

(define (compile-= i1 i2 c)
  (let ((c0 (compile-e i1 c))
        (c1 (compile-e i2 c))
        (l0 (gensym "end")))
    `(,@c1
      ,@assert-integer
      (push rax)
      ,@c0
      ,@assert-integer
      (pop rcx)
      (cmp rax rcx)
      (mov rax ,imm-val-false)
      (jne ,l0)
      (mov rax ,imm-val-true)
      ,l0
      )
    )
  )

(define (compile-< i1 i2 c)
  (let ((c0 (compile-e i1 c))
        (c1 (compile-e i2 c))
        (l0 (gensym "end")))
    `(,@c1
      ,@assert-integer
      (mov rcx rax)
      ,@c0
      ,@assert-integer
      (cmp rax rcx)
      (mov rax ,imm-val-true)
      (jl ,l0)
      (mov rax ,imm-val-false)
      ,l0
      )
    )
  )

(define (compile-<= i1 i2 c)
  (let ((c0 (compile-e i1 c))
        (c1 (compile-e i2 c))
        (l0 (gensym "end")))
    `(,@c1
      ,@assert-integer
      (mov rcx rax)
      ,@c0
      ,@assert-integer
      (cmp rax rcx)
      (mov rax ,imm-val-true)
      (jle ,l0)
      (mov rax ,imm-val-false)
      ,l0
      )
    )
  )

(define (compile-char=? ch1 ch2 c)
  (let ((c0 (compile-e ch1 c))
        (c1 (compile-e ch2 c))
        (l0 (gensym)))
    `(,@c0
      ,@assert-char
      (mov rcx rax)
      ,@c1
      ,@assert-char
      (cmp rax rcx)
      (mov rax ,imm-val-false)
      (jne ,l0)
      (mov rax ,imm-val-true)
      ,l0
      )
    )
  )

(define (compile-boolean=? b1 b2 c)
  (let ((c0 (compile-e b1 c))
        (c1 (compile-e b2 c))
        (l0 (gensym)))
    `(,@c0
      ,@assert-bool
      (mov rcx rax)
      ,@c1
      ,@assert-bool
      (cmp rax rcx)
      (mov rax ,imm-val-false)
      (jne ,l0)
      (mov rax ,imm-val-true)
      ,l0
      )
    )
  )

(define (compile-var x c)
  (let ((i (lookup x c)))
    `((mov rax (offset rsp ,(- (add1 i)))))))

(define (compile-print e0 c)
  (let ((c0 (compile-e e0 c))
        (l0 (gensym))
        (l1 (gensym))
        (l2 (gensym))
        (l3 (gensym)))
    `(,@c0
      (push rax)
      (and rax ,result-type-mask)
      (cmp rax ,type-string)
      (je ,l0)
      (pop rax)
      (push rax)
      (and rax ,imm-type-mask)
      (cmp rax ,imm-type-char)
      (je ,l1)
      (pop rax)
      (push rax)
      (and rax ,imm-type-mask)
      (cmp rax ,imm-type-int)
      (je ,l2)
      (and rax ,imm-type-mask)
      (cmp rax ,imm-type-bool)
      (je ,l2)
      (jmp err)
      ,l0
      (call _printString)
      (jmp ,l3)
      ,l1
      (pop rdi)
      (shr rdi 5)
      (call _printChar)
      (shl rax 5)
      (or rax ,imm-type-char)
      (jmp ,l3)
      ,l2
      (pop rdi)
      (shr rdi 5)
      (call _printInt)
      (shl rax 5)
      ,l3
      )))

(define (compile-box e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      (push rax)
      (or rax ,type-box))))

(define (compile-unbox e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-box
      (xor rax ,type-box)
      (mov rax (offset rax 0)))))

(define (compile-cons e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c))))
    `(,@c0
      ;(mov (offset rsp ,(- (add1 (length c)))) rax)
      (push rax)
      ,@c1
      ;(mov (offset rdi 0) rax)
      (or rax ,type-pair)
      (push rax)
      ;(mov rax (offset rsp ,(- (add1 (length c)))))
      ;(mov (offset rdi 1) rax)
      ;(mov rax rdi)
      ;(add rdi 16))))
      )))

(define (compile-car e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-pair
      ;(mov rax (offset rax 1))
      (xor rax ,type-pair) ; untag
      )))

(define (compile-cdr e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-pair
      (xor rax ,type-pair) ; untag
      (mov rax (offset rax 0)))))

(define (compile-add1 e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer
      (add rax ,(arithmetic-shift 1 imm-shift)))))

(define (compile-sub1 e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer
      (sub rax ,(arithmetic-shift 1 imm-shift)))))

(define (compile-zero? e0 c)
  (let ((c0 (compile-e e0 c))
        (l0 (gensym))
        (l1 (gensym)))
    `(,@c0
      ,@assert-integer
      (cmp rax 0)
      (mov rax ,imm-val-false)
      (jne ,l0)
      (mov rax ,imm-val-true)
      ,l0)))

(define (compile-if e0 e1 e2 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 c))
        (c2 (compile-e e2 c))
        (l0 (gensym))
        (l1 (gensym)))
    `(,@c0
      (cmp rax ,imm-val-false)
      (je ,l0)
      ,@c1
      (jmp ,l1)
      ,l0
      ,@c2
      ,l1)))

(define (compile-+ e0 e1 c)
  (let ((c1 (compile-e e1 c))
        (c0 (compile-e e0 (cons #f c))))
    `(,@c1
      ,@assert-integer
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c0
      ,@assert-integer
      (add rax (offset rsp ,(- (add1 (length c))))))))

(define (compile-+- e0 e1 c)
  (let ((c1 (compile-e e1 c))
        (c0 (compile-e e0 (cons #f c))))
    `(,@c1
      ,@assert-integer
      (neg rax)
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c0
      ,@assert-integer
      (add rax (offset rsp ,(- (add1 (length c))))))))

(define (compile-* e0 e1 c)
  (let ((c1 (compile-e e1 c))
        (c0 (compile-e e0 (cons #f c))))
    `(,@c1
      ,@assert-integer
      (shr rax ,5)
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c0
      ,@assert-integer
      (shr rax, 5)
      (imul rax (offset rsp ,(- (add1 (length c)))))
      (shl rax 5))))

(define (compile-/ e0 e1 c)
  (let ((c1 (compile-e e1 c))
        (c0 (compile-e e0 (cons #f c))))
    `(,@c1
      ,@assert-integer
      (shr rax ,5)
      (mov rcx rax)
      ,@c0
      ,@assert-integer
      (shr rax, 5)
      (div rcx)
      (shl rax 5))))

(define (compile-abs e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer
      (mov rbx rax)
      (neg rax)
      (cmovl rax rbx)
      ))
  )

(define (compile-- e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer
      (neg rax)))
  )

(define (compile-cond ls c)
  (match ls
    [(cons h nls)
     (match h
       [`(else ,nnls) (compile-e nnls c)]
       [`(,x ,y) (let ((c0 (compile-e x c))
                       (c1 (compile-e y c))
                       (c2 (compile-cond nls c))
                       (l0 (gensym "if"))
                       (l1 (gensym "if")))
                   `(,@c0
                     (cmp rax ,imm-val-false)
                     (je ,l0)
                     ,@c1
                     (jmp, l1)
                     ,l0
                     ,@c2
                     ,l1))]
       )]
    )
  )

(define (let_helper ls e c)
  (match ls
    ['() (compile-e e c)]
    [(cons h nls) (match h
                    [`(,x ,e0)
                     (let ((c0 (compile-e e0 c))
                           ;;(c1 (compile-e e (cons x c))))
                           (c1 (let_helper nls e (cons x c))))
                       `(,@c0
                         (mov (offset rsp ,(- (add1 (length c)))) rax)
                         ,@c1))])]
    )
  )

(define (compile-ich i c)
  (let ((c0 (compile-e i c)))
    `(,@c0
      ,@assert-integer
      ,@(assert-int-char-range)
      (add rax ,(+ (arithmetic-shift 0 imm-shift) imm-type-char))
      ,@assert-char)
    )
  )

(define (compile-chi ch c)
  (let ((c0 (compile-e ch c)))
    `(,@c0
      ,@assert-char
      (sub rax ,(+ (arithmetic-shift 0 imm-shift) imm-type-char))
      )
    )
  )


(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y cenv)
     (match (eq? x y)
       [#t (length cenv)]
       [#f (lookup x cenv)])]))

(define (assert-type p)
  `((mov rbx rax)
    (and rbx ,(type-pred->mask p))
    (cmp rbx ,(type-pred->tag p))
    (jne err)))

(define (type-pred->mask p)
  (match p
    [(or 'box? 'cons? 'string? 'pair?) result-type-mask]
    [_ imm-type-mask]))

(define (type-pred->tag p)
  (match p
    ['pair?    type-pair]
    ['box?     type-box]
    ['cons?    type-pair]
    ['string?  type-string]
    ['integer? imm-type-int]
    ['empty?   imm-type-empty]
    ['char?    imm-type-char]
    ['boolean? imm-type-bool]))

(define assert-integer (assert-type 'integer?))
(define assert-box     (assert-type 'box?))
(define assert-pair    (assert-type 'pair?))
(define assert-char    (assert-type 'char?))
(define assert-bool    (assert-type 'boolean?))
(define assert-string  (assert-type 'string?))


(define (assert-int-char-range)
  (let ((end (gensym "end")))
    `((mov rbx rax)
      (and rbx ,imm-type-int)
      (cmp rbx 0)
      (jne err)
      (mov rbx rax)
      (cmp rbx ,(arithmetic-shift #x110000 imm-shift))
      (jge err)
      (mov rbx rax)
      (cmp rbx ,(arithmetic-shift #xE000 imm-shift))
      (jge ,end)
      (mov rbx rax)
      (cmp rbx ,(arithmetic-shift #xD800 imm-shift))
      (jge err)
      (mov rbx rax)
      (cmp rbx ,(arithmetic-shift -1 imm-shift))
      (jle err)
      ,end
      )
    )
  )
