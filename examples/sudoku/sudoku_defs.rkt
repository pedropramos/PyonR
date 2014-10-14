#lang racket

(provide (all-defined-out))

(define (sequence->set seq)
  (for/set ([item seq])
    item))

(define (zip . vectors)
  (apply vector-map vector vectors))

(define-syntax-rule (for/mutable-hash ([item sequence] ...) body ...)
  (let ([ht (make-hash)])
    (for ([item sequence] ...)
      (let-values ([(k v) (begin body ...)])
        (hash-set! ht k v)))
    ht))