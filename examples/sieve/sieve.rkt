#lang racket
(define (sieve n)
  (let ([primes (make-vector (add1 n) #t)]
        [counter 0])
    (for ([i (in-range 2 n)])
      (when (vector-ref primes i)
        (set! counter (add1 counter))
        (for ([j (in-range (* i i) n i)])
          (vector-set! primes j #f))))
    counter))

(time (sieve 10000000))