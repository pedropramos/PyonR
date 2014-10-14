#lang racket
(require math/matrix)

(define (randint x y limit)
  (build-matrix x y (lambda (x y) (random limit))))

(define (add-arrays n)
  (let ([result (make-matrix 100 100 0)])
    (for ([i n])
      (set! result
            (matrix+ result (randint 100 100 100000))))
    result))

(void (time (add-arrays 100)))