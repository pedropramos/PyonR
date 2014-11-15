(module compile racket/base
  (require racket/class
           "ast-node.rkt")
  
  (provide (all-defined-out))
  
  
  (define (compile-expression expr-ast)
    (send expr-ast to-racket))  
  
  (define (compile-python ast+scope)
    (let ([ast   (car ast+scope)]
          [scope (cdr ast+scope)])
      (append (compile-top-definitions scope)
              (map send-to-racket ast))))
    
  ;; make-tree - processes an AST+Scope, returning a readable list of all its contents
  (define (make-tree ast+scope)
    (let ([ast (car ast+scope)])
      (cond
        [(is-a? ast ast-node%) (map make-tree (send ast to-description))]
        [(list? ast) (map make-tree ast)]
        [else ast])))
  
  )