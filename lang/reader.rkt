#lang s-exp syntax/module-reader
python/lang/python

#:read python-read
#:read-syntax python-read-syntax
#:info python-get-info
#:language-info '#(python/lang/lang-info get-language-info #f)
#:whole-body-readers? #t


(require "../parse.rkt"
         ;; Parses to generate an AST. Identifiers in the AST
         ;; are represented as syntax objects with source location.

         "../compile.rkt"
         ;; Compiles a simplified AST to Scheme.
         
         "../lex+yacc.rkt"

         syntax/strip-context)

(provide python-read
         python-read-syntax
         python-read-syntax-repl
         python-get-info
         ready-to-submit?)


(define (python-read in)
  (map syntax->datum (python-read-syntax #f in)))


(define (python-read-syntax src in)
  (define parsed (read-python-port in src))
  (define compiled (compile-python parsed))
  (define stripped (map strip-context compiled))
  stripped)


(require racket/list)

(define (python-read-syntax-repl src in)
  (let ([code (python-read-syntax src (filter-unready-port in))])
    (if (empty? code)
        eof
        #`(begin #,@code))))


;; TODO
(define (python-get-info key default default-filter)
  (case key
    [(color-lexer) colorizer-lexer]
    [else (default-filter key default)]))