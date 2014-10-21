#lang racket
(define (ackermann m n)
  (cond
    [(= m 0) (+ n 1)]
    [(and (> m 0) (= n 0)) (ackermann (- m 1) 1)]
    [else (ackermann (- m 1) (ackermann m (- n 1)))]))

(time (ackermann 3 9))