#lang racket
(provide parse)

(define (parse lot)
  (match (parse-expr lot)
    [(cons '(eof) e) (list e)]
    [(cons (cons 'lparen e2) e1) (append (list e1) (parse (cons 'lparen e2)))]
    [_ (error "parse error")]))

(define (prim1? p)
  (match p
    [`(prim ,n)
     (and (memq n '(add1 sub1 zero? abs integer->char char->integer char?
                         boolean? integer? string? box? empty? cons?
                         box unbox car cdr print string-length))
          #t)]

    [_ #f]))

(define (prim2? p)
  (match p
    [`(prim ,n)
     (and (memq n '(make-string string-ref = < <= char=? boolean=? + * / cons))
          #t)]
    [_ #f]))

(define (parse-expr lot)
  (match lot
    [(cons '() lot)
     (cons lot ''())]
    [(cons (? integer? i) lot)
     (cons lot i)]
    [(cons (? char? c) lot)
     (cons lot c)]
    [(cons (? boolean? b) lot)
     (cons lot b)]
    [(cons (? string? s) lot)
     (cons lot s)]
    [(cons `(variable ,x) lot)
     (cons lot x)]
    [(cons 'lparen lot)
     (match (parse-compound lot)
       [(cons (cons 'rparen lot) e)
        (cons lot e)])]
    [(cons 'lsquare lot)
     (match (parse-compound lot)
       [(cons (cons 'rsquare lot) e)
        (cons lot e)])]))

(define (parse-compound lot)
  (match lot
    [(cons (? prim1? p) lot)
     (match (parse-expr lot)
       [(cons lot e)
        (match p
          [`(prim ,p)
           (cons lot (list p e))])])]
    [(cons (? prim2? p) lot)
     (match (parse-expr lot)
       [(cons lot e0)
        (match (parse-expr lot)
          [(cons lot e1)
           (match p
             [`(prim ,p)
              (cons lot (list p e0 e1))])])])]
    [(cons '(prim -) lot)
     (match (parse-expr lot)
       [(cons lot e0)
        (match (parse-maybe-expr lot)
          [(cons lot #f)
           (cons lot (list '- e0))]
          [(cons lot e1)
           (cons lot (list '- e0 e1))])])]
    [(cons '(keyword if) lot)
     (match (parse-expr lot)
       [(cons lot q)
        (match (parse-expr lot)
          [(cons lot e1)
           (match (parse-expr lot)
             [(cons lot e2)
              (cons lot (list 'if q e1 e2))])])])]
    [(cons '(keyword cond) lot)
     (match (parse-clauses lot)
       [(cons lot cs)
        (match (parse-else lot)
          [(cons lot el)
           (cons lot `(cond ,@cs ,el))])])]
    [(cons '(keyword let) lot)
     (match (parse-bindings lot)
       [(cons lot bs)
        (match (parse-expr lot)
          [(cons lot e)
           (cons lot `(let ,@bs ,e))])])]
    [(cons '(keyword let*) lot)
     (match (parse-bindings lot)
       [(cons lot bs)
        (match (parse-expr lot)
          [(cons lot e)
           (cons lot `(let* ,@bs ,e))])])]))

(define (parse-maybe-expr lot)
  (match lot
    ['() (cons '() #f)]
    [(cons (or 'rparen 'rsquare) _)
     (cons lot #f)]
    [_ (parse-expr lot)]))

(define (parse-bindings lot)
  (match lot
    [(cons 'lparen lot)
     (match (parse-binding-list lot)
       [(cons (cons 'rparen lot) bs) (cons lot (list bs))])]
    [(cons 'lsquare lot)
     (match (parse-binding-list lot)
       [(cons (cons 'rsquare lot) bs) (cons lot (list bs))])]))

(define (parse-binding-list lot)
  (match lot
    [(cons (or 'lparen 'lsquare) _)
     (match (parse-binding lot)
       [(cons lot b)
        (match (parse-binding-list lot)
          [(cons lot bs)
           (cons lot (cons b bs))])])]
    [_ (cons lot '())]))

(define (parse-binding lot)
  (match lot
    [(cons 'lparen (cons (? variable? x) lot))
     (match (parse-expr lot)
       [(cons (cons 'rparen lot) e)
        (match x
          [`(variable ,x)
           (cons lot (list x e))])])]
    [(cons 'lsquare (cons (? variable? x) lot))
     (match (parse-expr lot)
       [(cons (cons 'rsquare lot) e)
        (match x
          [`(variable ,x)
           (cons lot (list x e))])])]))

(define (parse-clauses lot)
  (match lot
    [(cons (or 'lparen 'lsquare) (cons '(keyword else) _))
     (cons lot '())]
    [(cons (or 'lparen 'lsquare) _)
     (match (parse-clause lot)
       [(cons lot c)
        (match (parse-clauses lot)
          [(cons lot cs)
           (cons lot (cons c cs))])])]
    [_
     (cons lot '())]))

(define (parse-clause lot)
  (match lot
    [(cons 'lparen lot)
     (match (parse-expr lot)
       [(cons lot q)
        (match (parse-expr lot)
          [(cons (cons 'rparen lot) e)
           (cons lot (list q e))])])]
    [(cons 'lsquare lot)
     (match (parse-expr lot)
       [(cons lot q)
        (match (parse-expr lot)
          [(cons (cons 'rsquare lot) e)
           (cons lot (list q e))])])]))

(define (parse-else lot)
  (match lot
    [(cons 'lparen (cons '(keyword else) lot))
     (match (parse-expr lot)
       [(cons (cons 'rparen lot) e)
        (cons lot (list 'else e))])]
    [(cons 'lsquare (cons '(keyword else) lot))
     (match (parse-expr lot)
       [(cons (cons 'rsquare lot) e)
        (cons lot (list 'else e))])]))

(define (variable? x)
  (match x
    [`(variable ,_) #t]
    [_ #f]))
