#lang racket
(provide (except-out (all-defined-out) paths.rkt))

(require racket/runtime-path)
(define-runtime-path paths.rkt "paths.rkt")
(define python-root (simplify-path (build-path paths.rkt 'up)))

(define path-to-lib (simplify-path (build-path python-root "lib")))
(define path-to-PATH (simplify-path (build-path path-to-lib "PATH")))