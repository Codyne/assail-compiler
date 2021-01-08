#lang racket
(provide (all-defined-out))

(define (expr? x)
  (match x
    [(cons h '()) (expr?-helper h)]
    [(cons h t) (and (expr?-helper h) (expr? t))]
    [_ #f]
    )
  )

(define (expr?-helper x)
  (match x
    [(? integer? i) #t]
    [(? boolean? b) #t]
    [(? char? c) #t]
    [(? string? s) #t]
    [(? symbol?) (not (memq x '(if let add1 sub1)))]
    [`(if ,x ,y ,z)
     (and (expr?-helper x)
          (expr?-helper y)
          (expr?-helper z))]
    [`(,(? prim1?) ,x) (expr?-helper x)]
    [`(,(? prim2?) ,x ,y) (and (expr?-helper x) (expr?-helper y))]
    [(list 'cond `(,xs ,ys) ... `(else ,z))
     (and (andmap expr?-helper xs)
          (andmap expr?-helper ys)
          (expr?-helper z))]
    [`(string-ref ,x ,y)
     (and (expr?-helper x)
          (expr?-helper y))]
    [`(make-string ,x ,y)
     (and (expr?-helper x)
          (expr?-helper y))]
    [`(let ((,v ,y)) ,z)
     (and (symbol? v)
          (expr?-helper y)
          (expr?-helper z))]
    [_ #f]))

(define (prim1? x)
  (and (symbol? x)
       (memq x '(add1 sub1 abs - integer->char char->integer
                      car cdr print length                     
                      char? integer? boolean? zero?))))

(define (prim2? x)
  (and (symbol? x)
       (memq x '(+
                 *
                 =
                 <
                 <=
                 /
                 cons
                 -))))
