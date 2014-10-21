#lang racket
(require "reader.rkt"
         "../runtime.rkt"
         "../engine/exceptions.rkt")
         
(provide configure)
 
(define (configure data)
  (error-display-handler python-error-display-handler)
  (current-read-interaction python-read-syntax-repl)
  (current-print (lambda (val)
                   (py-display val)
                   (namespace-set-variable-value! ':_ val))))