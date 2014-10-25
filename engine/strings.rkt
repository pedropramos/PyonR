(module strings racket
  (provide (all-defined-out))
  (require "engine.rkt"
           "exceptions.rkt"
           (only-in srfi/13 string-contains))
  
  
  (define (py-string-hash x)
    (let ([bytes (string->bytes/utf-8 x)])
      (remainder (for/fold ([result 0]) ([byte bytes])
                   (+ (* result 256) byte))
                 2147483647)))
  
  (define (py-string-contains outer inner)
    (if (string-contains outer inner) #t #f))
  
  (define (py-string-str x)
    (string-append "'" x "'"))
  
  (define (py-string-new typ . args)
    (if (empty? args)
        ""
        (let ([str (mro-lookup (first args) '__str__)])
          (or (and str (str (first args)))
              (mro-method-call (first args) '__repr__)))))
  
  (define (py-string-replace str old new [count -1])
    (if (< count 0)
        (string-replace str old new)
        (let loop ([i 0]
                   [res str])
          (if (< i count)
              (loop (add1 i) (string-replace res old new #:all? #f))
              res))))
  
  
  
  (define (py-string-mod str args)
    (let loop ([str str]
               [args (if (vector? args)
                         (vector->list args)
                         (list args))])
      (let ([%-location (string-contains str "%")])
        (when (and (not %-location) (not (empty? args)))
          (exn-raise py-TypeError "not all arguments converted during string formatting"))
        (when (and %-location (empty? args))
          (exn-raise py-TypeError "not enough arguments for format string"))
        (cond
          [(and (not %-location) (empty? args)) str]
          [else (let* ([pattern (substring str %-location (+ %-location 2))]
                       [value (first args)]
                       [replacement (case pattern
                                      [("%s") (py-string-new py-string value)]
                                      [("%r") (repr value)]
                                      [("%d" "%i" "%u") (number->string (inexact->exact (floor value)))]
                                      [("%e" "%E" "%f" "%F" "%g" "%G") (number->string (exact->inexact value))]
                                      [("%%") "%"]
                                      [else (exn-raise py-TypeError "'~a' pattern not recoginized or not implemented" pattern)])])
                  (loop (string-replace str pattern replacement #:all? #f)
                        (rest args)))]))))
  
  
  
  (define py-string
    (make-type "str"
               (vector py-object)
               (hasheq '__add__ string-append
                     '__contains__ py-string-contains
                     '__doc__ #f
                     '__eq__ equal?
                     '__format__ #f
                     '__ge__ string>=?
                     '__getattribute__ #f
                     '__getitem__ (compose string string-ref)
                     '__getnewargs__ #f
                     '__getslice__ #f
                     '__gt__ string>?
                     '__hash__ equal-hash-code
                     '__le__ string<=?
                     '__len__ string-length
                     '__lt__ string<?
                     '__mod__ py-string-mod
                     '__mul__ #f
                     '__ne__ (compose not equal?)
                     '__new__ py-string-new
                     '__repr__ py-string-str
                     '__rmod__ #f
                     '__rmul__ #f
                     '__sizeof__ #f
                     '__str__ identity
                     '_formatter_field_name_split #f
                     '_formatter_parser #f
                     'capitalize #f
                     'center #f
                     'count #f
                     'decode #f
                     'encode #f
                     'endswith #f
                     'expandtabs #f
                     'find #f
                     'format #f
                     'index #f
                     'isalnum #f
                     'isalpha #f
                     'isdigit #f
                     'islower #f
                     'isspace #f
                     'istitle #f
                     'isupper #f
                     'join #f
                     'ljust #f
                     'lower string-downcase
                     'lstrip #f
                     'partition #f
                     'replace py-string-replace
                     'rfind #f
                     'rindex #f
                     'rjust #f
                     'rpartition #f
                     'rsplit #f
                     'rstrip #f
                     'split #f
                     'splitlines #f
                     'startswith #f
                     'strip #f
                     'swapcase #f
                     'title #f
                     'translate #f
                     'upper string-upcase
                     'zfill #f)))
  
  (define-syntax-rule (make-py-string x) x)
  
  )