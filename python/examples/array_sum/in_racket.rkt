#lang typed/racket
(require math/matrix)

(: randint (Integer Integer Integer -> (Matrix Integer)))
(define (randint x y limit)
  (build-matrix x y (lambda (x y) (random limit))))

(: add-arrays (Integer -> (Matrix Integer)))
(define (add-arrays n)
  (let: ([result : (Matrix Integer)
                 (make-matrix 100 100 0)])
    (for ([i n])
      (set! result
            (matrix+ result (randint 100 100 100000))))
    result))

(void (time (add-arrays 10000)))