(module bools racket
  (provide (all-defined-out))
  (require "engine.rkt"
           "numbers.rkt")
  
  (define (py-bool-repr x)
    (if x "True" "False"))
  
  (define py-bool
    (make-type "bool"
               (vector py-int)
               (hasheq '__and__ #f    ; cuidado com as operacoes de ints
                       '__doc__ #f
                       '__new__ #f
                       '__or__ #f
                       '__rand__ #f
                       '__repr__ py-bool-repr
                       '__ror__ #f
                       '__rxor__ #f
                       '__str__ py-bool-repr
                       '__xor__ #f)))
  
  (define-syntax-rule (make-py-bool x) x)
  
  (define-syntax-rule (bool->int b)
    (if b 1 0))
  
  (define-syntax-rule (int->bool i)
    (not (= i 0)))
  
  
  (define py-True #t)
  (define py-False #f)
  
  
  )