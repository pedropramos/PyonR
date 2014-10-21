#lang racket

(require racket/generator
         racket/undefined
         "../runtime.rkt"
         "../cpy-importing.rkt"
         "../lib/__builtin__.py")

(provide (except-out (all-from-out racket) #%module-begin)
         (rename-out [module-begin #%module-begin])
         (all-from-out racket/generator)
         (all-from-out racket/undefined)
         (all-from-out "../runtime.rkt")
         (all-from-out "../cpy-importing.rkt")
         (all-from-out "../lib/__builtin__.py"))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ stmts ...)
     (with-syntax ([provide-stmt (datum->syntax stx '(provide (all-defined-out)))])
       #'(#%module-begin
          provide-stmt
          (define-namespace-anchor anchor)
          (namespace-for-eval (namespace-anchor->namespace anchor))
          
          (set-name-of-module!)
          stmts ...))]))