#lang racket
(provide (all-defined-out))

(define (expr? x)
  (match x
    [(? integer? i) #t]
    [(? boolean? b) #t]
    [(? char? c) #t]
    [(? string? s) #t]
    [`(if ,x ,y ,z)
     (and (expr? x)
          (expr? y)
          (expr? z))]
    [`(,(? prim1?) ,x) (expr? x)]
    [`(,(? prim2?) ,x ,y) (and (expr? x) (expr? y))]
    [(list 'cond `(,xs ,ys) ... `(else ,z))
     (and (andmap expr? xs)
          (andmap expr? ys)
          (expr? z))]
    [`(string-ref ,x ,y)
     (and (expr? x)
          (expr? y))]
    [`(make-string ,x ,y)
     (and (expr? x)
          (expr? y))]
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
