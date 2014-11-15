#lang racket
(require "parse.rkt" "compile.rkt" "ast-node.rkt"
         "runtime.rkt" "lib/__builtin__.py" "cpy-importing.rkt"
         racket/generator racket/undefined)

(define (quick-compile [ast (read-python-stdin)])
  (compile-python ast))

(define (quick-compile/sexp [ast (read-python-stdin)])
  (map syntax->datum (compile-python ast)))

(define (quick-parse [ast (read-python-stdin)])
  (make-tree ast))