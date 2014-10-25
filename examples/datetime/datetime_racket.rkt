#lang racket
(require python)

(cpy-from "datetime" import (["date" as python-date]))

(define today (py-get-attr python-date "today"))
(define ilc (today))
(py-print ilc)
(py-print (py-get-attr ilc "year"))
(py-print (py-method-call ilc "isoweekday"))

(define xmas (python-date 2014 12 25))
(define interval (py-sub xmas ilc))
(py-print interval)
(py-print (py-get-attr interval "days"))

(py-print (py-method-call ilc "isocalendar"))
(displayln (tuple-from-cpy
            (unwrap-proxy-object
             (py-method-call ilc "isocalendar"))))
