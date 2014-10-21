#lang racket

(define (mandelbrot limit c)
  (let loop ([i 0] [z 0+0i])
    (cond
      [(> i limit) i]
      [(> (magnitude z) 2) i]
      [else (loop (add1 i)
                  (+ (* z z) c))])))

(time (mandelbrot 1000000 .2+.3i))