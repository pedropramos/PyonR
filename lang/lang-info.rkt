#lang racket
(require "../lex+yacc.rkt")

(provide get-language-info)
 
(define (get-language-info data)
  (lambda (key default)
    (case key
      [(color-lexer) colorizer-lexer]
      [(configure-runtime) '(#(python/lang/runtime-config configure #f))]
      [(drracket:submit-predicate) ready-to-submit?]
      [else default])))
