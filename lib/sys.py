#lang racket
(require "../runtime.rkt")
(provide (rename-out [PATH :path]
                     [stdin :stdin]
                     [stdout :stdout]
                     [stderr :stderr]))

(define stdin  (wrap-port (current-input-port)))
(define stdout (wrap-port (current-output-port)))
(define stderr (wrap-port (current-error-port)))