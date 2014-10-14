(module parse racket/base
  (require racket/class
           "lex+yacc.rkt"
           "ast-node-stmt.rkt")
  
  (provide read-python-file
           read-python-stdin
           read-python-port)
  
  (define (read-python-file path)
    (init-bindings (build-ast-from-file path)))
  
  (define (read-python-stdin)
    (init-bindings (build-ast-from-port (current-input-port) "stdin")))
  
  (define (read-python-port port name)
    (init-bindings (build-ast-from-port port name)))
  
  (define (init-bindings ast-l)
    (let ([scope (make-object module-scope%)])
      (for-each (lambda (a) (send a set-bindings! scope))
                ast-l)
      (for-each (lambda (a) (send a check-break/cont #f))
                ast-l)
      ast-l))
  
  )