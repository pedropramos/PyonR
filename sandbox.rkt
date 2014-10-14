#lang racket
(require "parse.rkt" "compile.rkt" "ast-node.rkt"
         "runtime.rkt" "lib/__builtin__.py" "cpy-importing.rkt"
         racket/generator racket/undefined)


;(define ast1 (read-python-file "C:\\Racket\\collects\\python\\mandelbrot\\mandelbrot_rack.py"))
;(define ast2 (read-python-file "C:\\Racket\\collects\\python\\ackermann\\ackermann_rack.py"))
;(define ast3 (read-python-file "C:\\Racket\\collects\\python\\fib\\fib_rack.py"))
;(define ast4 (read-python-file "C:\\Racket\\collects\\python\\sieve\\sieve_rack.py"))
;(define ast5 (read-python-file "C:\\Racket\\collects\\python\\outros\\foo.pyra"))
;(define ast6 (read-python-file "C:\\Racket\\collects\\python\\outros\\sinal.pyra"))
;(define ast7 (read-python-file "C:\\Users\\Admin\\scissors.py"))

(define (quick-compile [ast (read-python-stdin)])
  (compile-python ast))

(define (quick-compile/sexp [ast (read-python-stdin)])
  (map syntax->datum (compile-python ast)))

(define (quick-parse [ast (read-python-stdin)])
  (make-tree ast))


