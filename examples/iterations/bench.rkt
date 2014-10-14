#lang racket
(require "../../runtime.rkt"
         "../../lib/__builtin__.py"
         racket/undefined)
         

(define-py-function :bench_for with-params (n)
  (lambda (:n)
    (let ((:i undefined)
          (:L undefined))
      (begin
        (py-assign! :L (make-py-list))
        (for ([:i (py-list->vector (py-call :range 1 (py-add :n 1)))])
          (py-call (py-get-attr :L "append")
                   (py-mul 2 :i)))
        (py-call :all :L)))))

(time (void (:bench_for 1000)))
(time (void (:bench_for 10000)))
(time (void (:bench_for 100000)))
(time (void (:bench_for 1000000)))