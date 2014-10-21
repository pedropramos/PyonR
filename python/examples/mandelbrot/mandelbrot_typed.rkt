#lang typed/racket

(: mandelbrot (Integer Float-Complex -> Integer))
(define (mandelbrot limit c)
  (let: loop ([i : Integer 0]
              [z : Float-Complex 0.0+0.0i])
    (cond
      [(> i limit) i]
      [(> (magnitude z) 2) i]
      [else (loop (add1 i)
                  (+ (* z z) c))])))

(time (mandelbrot 1000000 .2+.3i))