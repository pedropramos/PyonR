#lang racket

(provide (all-defined-out))
(require "paths.rkt")

(define (enable-cpyimport!)
  (let* ([file (open-output-file path-to-CPYIMPORT
                                 #:mode 'text
                                 #:exists 'truncate/replace)])
    (display "ON" file)
    (close-output-port file)
    (displayln "The 'cpyimport' statement is now enabled.")))

(define (disable-cpyimport!)
  (let* ([file (open-output-file path-to-CPYIMPORT
                                 #:mode 'text
                                 #:exists 'truncate/replace)])
    (display "OFF" file)
    (close-output-port file)
    (displayln "The 'cpyimport' statement is now disabled.")))

(define (cpyimport-enabled?)
  (let* ([file (open-input-file path-to-CPYIMPORT)]
         [line (read-line file)])
    (string=? line "ON")))