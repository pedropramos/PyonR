(module compile racket/base
  (require racket/class
           "ast-node.rkt")
  
  (provide (all-defined-out))
  
  (define (compile-python ast-list)
    (map (lambda (ast) (send ast to-racket)) ast-list))
  
  
  ;; make-tree - processes an AST (single or list), returning a list of all its contents
  (define (make-tree x)
    (cond
      [(is-a? x ast-node%) (map make-tree (send x to-description))]
      [(list? x) (map make-tree x)]
      [else x]))
  
  )