#lang racket
(provide lex-port lex-string)

(define (lex-string s)
  (lex-port (open-input-string s)))

(define (lex-port p)
  (let loop ()
    (cond
      [(regexp-try-match "^$" p) (list 'eof)]
      [(regexp-try-match #px"^[[:space:]]+" p) (loop)]
      [else
       (cons
        (cond
          [(try-quote "'()" p) '()]
          [(regexp-try-match string p)
           => (compose bytes->string/utf-8 second)]
          [(regexp-try-match (keywords kws) p)
           => (compose mk-keyword string->symbol bytes->string/utf-8 first)]
          [(regexp-try-match (keywords prims) p)
           => (compose mk-prim string->symbol bytes->string/utf-8 first)]
          [(regexp-try-match "^[-]?[0-9]+" p)
           => (compose string->number bytes->string/utf-8 first)]
          [(regexp-try-match symbol p)
           => (compose mk-var string->symbol bytes->string/utf-8 first)]
          [(try-quote "(" p) 'lparen]
          [(try-quote ")" p) 'rparen]
          [(try-quote "[" p) 'lsquare]
          [(try-quote "]" p) 'rsquare]
          [(try-quote "#t" p) #t]
          [(try-quote "#f" p) #f]
          [(special-char p) => identity]
          [(octal-char p) => identity]
          [(hex4-char p) => identity]
          [(hex6-char p) => identity]
          [(char p) => identity]
          [else (error "lexing error")])
        (loop))])))

(define (mk-keyword k) `(keyword ,k))
(define (mk-prim k) `(prim ,k))
(define (mk-var k) `(variable ,k))

(define (try-quote s p)
  (regexp-try-match (string-append "^" (regexp-quote s)) p))

(define prims
  '("add1" "sub1" "zero?" "abs" "-" "integer->char" "char->integer" "char?" "integer?" "boolean?"
           "string?" "box?" "empty?" "cons" "cons?" "box" "unbox" "car" "cdr" "print"
           "string-length"
           "make-string" "string-ref" "=" "<" "<="
           "char=?" "boolean=?" "+" "*" "/"))

(define kws '("cond" "if" "let*" "let" "else"))

(define delim
  (string-append "$|"
                 (regexp-quote " ")  "|"
                 (regexp-quote "\n") "|"
                 (regexp-quote "\t") "|"
                 (regexp-quote "(")  "|"
                 (regexp-quote ")")  "|"
                 (regexp-quote "[")  "|"
                 (regexp-quote "]")))

(define string "^\"((\\\\\"|[^\"])*)\"")
(define symbol
  (string-append
   "^([^]# \n\t\\(\\)[][^] \n\t\\(\\)[]*)"))

(define (special-char p)
  (let ((r (regexp-try-match (string-append "^#\\\\("
                                            (apply string-append (add-between special-char-names "|"))
                                            ")")
                             p)))
    (and (or (eof-object? (peek-char p))
             (not (char-alphabetic? (peek-char p))))
         r
         (bytes->char (second r)))))

(define (octal-char p)
  (let ((r (regexp-try-match "^#\\\\([0-7][0-7][0-7])" p)))
    (and (or (eof-object? (peek-char p))
             (not (char-alphabetic? (peek-char p))))
         r
         (integer->char
          (convert 8 (map octal-byte->number (bytes->list (second r))))))))

(define (hex4-char p)
  (define h "[0-9a-fA-F]")
  (let ((r (regexp-try-match (string-append "^#\\\\u(" h h h h "|" h h h "|" h h "|" h ")") p)))
    (and (or (eof-object? (peek-char p))
             (not (char-alphabetic? (peek-char p))))
         r
         (integer->char
          (convert 16 (map hex-byte->number (bytes->list (second r))))))))

(define (hex6-char p)
  (define h "[0-9a-fA-F]")
  (let ((r (regexp-try-match (string-append "^#\\\\U("
                                            "00" h h h h h h "|" ; undocumented in Racket
                                            "0" h h h h h h "|"  ; undocumented in Racket
                                            h h h h h h "|"
                                            h h h h h "|"
                                            h h h h "|"
                                            h h h "|"
                                            h h "|"
                                            h ")") p)))
    (and (or (eof-object? (peek-char p))
             (not (char-alphabetic? (peek-char p))))
         r
         (integer->char
          (convert 16 (map hex-byte->number (bytes->list (second r))))))))

(define (char p)
  (let ((h (regexp-try-match "^#\\\\" p))
        (r (read-char p)))
    (and h
         (or (eof-object? (peek-char p))
             (or (not (char-alphabetic? r))
                 (not (char-alphabetic? (peek-char p)))))
         r)))

(define (convert base os)
  (define (loop os b)
    (match os
      ['() 0]
      [(cons o os)
       (+ (* (expt base b) o)
          (loop os (add1 b)))]))
  (loop (reverse os) 0))

(define (octal-byte->number b)
  (- b 48))

(define (hex-byte->number h)
  (cond
    [(<= 48 h 57) (- h 48)]
    [(<= 97 h 102) (- h 87)]
    [(<= 65 h 70) (- h 55)]))

(define special-char-names
  '("null" "nul" "backspace" "tab" "newline" "linefeed" "vtab" "page" "return" "space" "rubout"))

(define (bytes->char bs)
  (match bs
    [#"nul" #\nul]
    [#"null" #\null]
    [#"backspace" #\backspace]
    [#"tab" #\tab]
    [#"newline" #\newline]
    [#"linefeed" #\linefeed]
    [#"vtab" #\vtab]
    [#"page" #\page]
    [#"return" #\return]
    [#"space" #\space]
    [#"rubout" #\rubout]))



(define (keywords ws)
  (string-append
   "^("
   (apply string-append (add-between (map regexp-quote ws) "|"))
   ")(?=" delim ")"))
