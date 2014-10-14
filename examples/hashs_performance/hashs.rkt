#lang racket

(define alphabet "abcdefghijklmnopqrstuvwxyz")

(define (random-word n)
  (build-string n (lambda (x) (string-ref alphabet (random 26)))))

(define words (for/list ([k 1000000])
                (random-word 3)))

(define d (make-hash))

(define (run)
  (for ([w words])
    (if (hash-has-key? d w)
        (hash-set! d w (add1 (hash-ref d w)))
        (hash-set! d w 1))))
        
(time (run))