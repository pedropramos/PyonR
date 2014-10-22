#lang racket

(require "../../lang/python.rkt")
(provide (except-out (all-from-out "../../lang/python.rkt")
                     define-py-function
                     make-py-lambda
                     py-add)
         (rename-out [define-py-function/debug define-py-function]
                     [make-py-lambda/debug     make-py-lambda]))


(define-syntax (define-py-function/debug stx)
  (syntax-case stx (with-params lambda)
    [(_ name with-params (param ...)
        (lambda (args ...) body))
     
     #'(py-assign! name
                   (function_obj py-function
                                 (lambda (args ...)
                                   (let ([result body])
                                     (void)
                                     result))
                                 (list (quote param) ...)))]))


(define-syntax (make-py-lambda/debug stx)
  (syntax-case stx (with-params lambda)
    [(_ with-params (param ...) 
        (lambda (args ...) body))
     
     #'(function_obj py-function
                     (lambda (args ...)
                                   (let ([result body])
                                     (void)
                                     result))
                     (list (quote param) ...))]))