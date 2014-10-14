(module racket_values racket
  (provide (all-defined-out))
  (require "engine.rkt"
           "functions.rkt")
  
  
  (define (py-racket-value-get rkt-val obj type)
    (if (procedure? rkt-val)
        (py-function-get rkt-val obj type)
        rkt-val))
  
  
  (define py-racket-value
    (make-type "racket_value"
               (vector py-object)
               (hasheq '__get__ py-racket-value-get
                     '__repr__ ~a)))
  
  )