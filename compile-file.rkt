#lang racket
(provide (all-defined-out))
(require "compile.rkt" "syntax.rkt" "printer.rkt" "lex.rkt" "parse.rkt")

(define (main fn)
  (with-input-from-file fn
    (λ ()
      (let ((p (read-program)))
        (unless (expr? p)
          (error "syntax error"))          
        (asm-display (compile p))))))

(define (read-program)
  (parse (lex-port (current-input-port))))

(main (vector-ref (current-command-line-arguments) 0))
