(module numbers racket
  (provide (all-defined-out))
  (require "engine.rkt")
  
  
  (define (number-division a b)
    (if (and (exact-integer? a)
             (exact-integer? b))
        (quotient a b)
        (/ a b)))
  
  (define (py-int-hash n)
    n)
  
  (define (py-long-hash l)
    (remainder l 2147483647))
  
  (define (py-float-hash f)
    (if (integer? f)
        (py-long-hash (inexact->exact f))
        (let ([bytes (real->floating-point-bytes f 4)])
          (py-long-hash (+ (* 256 256 256 (bytes-ref bytes 0))
                           (* 256 256 (bytes-ref bytes 1))
                           (* 256 (bytes-ref bytes 2))
                           (bytes-ref bytes 3))))))
  
  (define (py-complex-hash c)
    (py-long-hash (+ (py-float-hash (real-part c))
                     (* 1000003 (py-float-hash (imag-part c))))))
  
  (define py-int
    (make-type "int"
               (vector py-object)
               (hasheq '__abs__ magnitude    ; /!\ must return float
                     '__add__ +
                     '__and__ #f
                     '__cmp__ -
                     '__coerce__ #f
                     '__div__ number-division
                     '__divmod__ #f
                     '__doc__ #f
                     '__float__ #f
                     '__floordiv__ #f
                     '__format__ #f
                     '__getattribute__ #f
                     '__getnewargs__ #f
                     '__hash__ py-int-hash
                     '__hex__ #f
                     '__index__ #f
                     '__int__ #f
                     '__invert__ #f
                     '__long__ #f
                     '__lshift__ #f
                     '__mod__ #f
                     '__mul__ *
                     '__neg__ -
                     '__new__ #f
                     '__nonzero__ #f
                     '__oct__ #f
                     '__or__ #f
                     '__pos__ #f
                     '__pow__ #f
                     '__radd__ #f
                     '__rand__ #f
                     '__rdiv__ #f
                     '__rdivmod__ #f
                     '__repr__ number->string
                     '__rfloordiv__ #f
                     '__rlshift__ #f
                     '__rmod__ #f
                     '__rmul__ #f
                     '__ror__ #f
                     '__rpow__ #f
                     '__rrshift__ #f
                     '__rshift__ #f
                     '__rsub__ #f
                     '__rtruediv__ #f
                     '__rxor__ #f
                     '__str__ number->string
                     '__sub__ -
                     '__truediv__ #f
                     '__trunc__ #f
                     '__xor__ #f
                     'bit_length #f
                     'conjugate #f
                     'denominator #f
                     'imag #f
                     'numerator #f
                     'real #f)))
  
  (define py-long
    (make-type "long"
               (vector py-object)
               (hasheq '__abs__ magnitude
                     '__add__ +
                     '__and__ #f
                     '__cmp__ -
                     '__coerce__ #f
                     '__div__ number-division
                     '__divmod__ #f
                     '__doc__ #f
                     '__float__ #f
                     '__floordiv__ #f
                     '__format__ #f
                     '__getattribute__ #f
                     '__getnewargs__ #f
                     '__hash__ py-long-hash
                     '__hex__ #f
                     '__index__ #f
                     '__int__ #f
                     '__invert__ #f
                     '__long__ #f
                     '__lshift__ #f
                     '__mod__ #f
                     '__mul__ *
                     '__neg__ -
                     '__new__ #f
                     '__nonzero__ #f
                     '__oct__ #f
                     '__or__ #f
                     '__pos__ #f
                     '__pow__ #f
                     '__radd__ #f
                     '__rand__ #f
                     '__rdiv__ #f
                     '__rdivmod__ #f
                     '__repr__ number->string
                     '__rfloordiv__ #f
                     '__rlshift__ #f
                     '__rmod__ #f
                     '__rmul__ #f
                     '__ror__ #f
                     '__rpow__ #f
                     '__rrshift__ #f
                     '__rshift__ #f
                     '__rsub__ #f
                     '__rtruediv__ #f
                     '__rxor__ #f
                     '__sizeof__ #f
                     '__str__ number->string
                     '__sub__ -
                     '__truediv__ #f
                     '__trunc__ #f
                     '__xor__ #f
                     'bit_length #f
                     'conjugate #f
                     'denominator #f
                     'imag #f
                     'numerator #f
                     'real #f)))
  
  (define py-float
    (make-type "float"
               (vector py-object)
               (hasheq '__abs__ magnitude
                     '__add__ +
                     '__coerce__ #f
                     '__div__ number-division
                     '__divmod__ #f
                     '__doc__ #f
                     '__eq__ equal?
                     '__float__ #f
                     '__floordiv__ #f
                     '__format__ #f
                     '__ge__ #f
                     '__getattribute__ #f
                     '__getformat__ #f
                     '__getnewargs__ #f
                     '__gt__ #f
                     '__hash__ py-float-hash
                     '__int__ #f
                     '__le__ #f
                     '__long__ #f
                     '__lt__ #f
                     '__mod__ #f
                     '__mul__ *
                     '__ne__ #f
                     '__neg__ -
                     '__new__ #f
                     '__nonzero__ #f
                     '__pos__ #f
                     '__pow__ #f
                     '__radd__ #f
                     '__rdiv__ #f
                     '__rdivmod__ #f
                     '__repr__ number->string
                     '__rfloordiv__ #f
                     '__rmod__ #f
                     '__rmul__ #f
                     '__rpow__ #f
                     '__rsub__ #f
                     '__rtruediv__ #f
                     '__setformat__ #f
                     '__str__ number->string
                     '__sub__ -
                     '__truediv__ #f
                     '__trunc__ #f
                     'as_integer_ratio #f
                     'conjugate #f
                     'fromhex #f
                     'hex #f
                     'imag #f
                     'is_integer #f
                     'real #f)))
  
  (define py-complex
    (make-type "complex"
               (vector py-object)
               (hasheq '__abs__ magnitude
                     '__add__ +
                     '__coerce__ #f
                     '__div__ number-division
                     '__divmod__ #f
                     '__doc__ #f
                     '__eq__ equal?
                     '__float__ #f
                     '__floordiv__ #f
                     '__format__ #f
                     '__ge__ #f
                     '__getattribute__ #f
                     '__getnewargs__ #f
                     '__gt__ #f
                     '__hash__ py-complex-hash
                     '__int__ #f
                     '__le__ #f
                     '__long__ #f
                     '__lt__ #f
                     '__mod__ #f
                     '__mul__ *
                     '__ne__ #f
                     '__neg__ -
                     '__new__ #f
                     '__nonzero__ #f
                     '__pos__ #f
                     '__pow__ #f
                     '__radd__ #f
                     '__rdiv__ #f
                     '__rdivmod__ #f
                     '__repr__ number->string
                     '__rfloordiv__ #f
                     '__rmod__ #f
                     '__rmul__ #f
                     '__rpow__ #f
                     '__rsub__ #f
                     '__rtruediv__ #f
                     '__str__ number->string
                     '__sub__ -
                     '__truediv__ #f
                     'conjugate #f
                     'imag #f
                     'real #f)))
  
  
  (define-syntax-rule (make-py-int x) x)
  (define-syntax-rule (make-py-long x) x)
  (define-syntax-rule (make-py-float x) x)
  (define-syntax-rule (make-py-complex x) x)
  
  (define (number-type x)
    (match x
      [(? exact-integer? x) (if (and (<= x 2147483647)
                                     (>= x -2147483648))
                                py-int
                                py-long)]
      [(? flonum? x) py-float]
      [(? complex? x) py-complex]
      [_ (error "number-type: what is this number?")]))
  
  )