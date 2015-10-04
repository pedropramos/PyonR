#lang racket
(require math)
(require "../runtime.rkt")
(provide (all-defined-out))

(define :pi pi)
(define :e euler.0)

;;;;;;;;;;;;;;;;;;;;;;,,,

(define (:ceil x)
  (exact->inexact (ceiling x)))

(define (:copysign x y)
  (if (positive? y)
      (abs x)
      (- (abs x))))

(define (:fabs x)
  (abs x))

(define (:factorial x)
  (if (and (integer? x) (not (negative? x)))
      (gamma (add1 x))
      (py-raise py-ValueError)))

(define (:floor x)
  (exact->inexact (floor x)))

(define (:fmod x y)
  (modulo x y))

(define (:frexp x)
  py-NotImplemented)

(define (:fsum iterable)
  py-NotImplemented)

(define (:isinf x)
  (infinite? x))

(define (:isnan x)
  (nan? x))

(define (:ldexp x i)
  (* x (expt 2 i)))

(define (:modf x)
  (py-tuple (- x (floor x)) (floor x)))

(define (:trunc x)
  py-NotImplemented)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (:exp x)
  (exp x))

(define (:expm1 x)
  py-NotImplemented)

(define (:log x [base euler.0])
  (if (= base euler.0)
      (log x)
      (/ (log x) (log base))))

(define (:log1p x)
  py-NotImplemented)

(define (:log10 x)
  (:log x 10))

(define (:pow x y)
  (expt x y))

(define (:sqrt x)
  (sqrt x))

;;;;;;;;;;;;;;;;;;;;;;;

(define (:acos x)
  (acos x))

(define (:asin x)
  (asin x))

(define (:atan x)
  (atan x))

(define (:atan2 y x)
  (atan y x))

(define (:cos x)
  (cos x))

(define (:hypot x y)
  (sqrt (+ (* x x) (* y y))))

(define (:sin x)
  (sin x))

(define (:tan x)
  (tan x))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (:degrees x)
  (* 180 (/ x pi)))

(define (:radians x)
  (* pi (/ x 180)))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (:acosh x)
  (acosh x))

(define (:asinh x)
  (asinh x))

(define (:atanh x)
  (atanh x))

(define (:cosh x)
  (cosh x))

(define (:sinh x)
  (sinh x))

(define (:tanh x)
  (tanh x))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (:erf x)
  (erf x))

(define (:erfc x)
  (erfc x))

(define (:gamma x)
  (gamma x))

(define (:lgamma x)
  (log-gamma x))