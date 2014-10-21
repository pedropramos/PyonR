(module ast-node-target racket/base
  (require racket/class
           "ast-node.rkt")

  (provide (all-defined-out))

  ;; 6.3
  (define target%
    (class ast-node%
      (super-instantiate ())))
  
  (define tidentifier%
    (class target%
      ;; name: symbol?
      (init-field name)
      
      (define scope #f)
      
      (define/public (get-symbol) name)
      
      (define/override (set-bindings! enclosing-scope)
        (when enclosing-scope 
          (unless (send enclosing-scope is-local? this)
            (send enclosing-scope add-binding this))
          (set! scope enclosing-scope)))
      
      
      (define/public (get-sub-targets) (list this)) ; I am my subtargets :P
      
      (define/override (to-description)
        `(tidentifier% ,name))
      
      (define/override (to-racket)
        (->orig-so (string->symbol
                    (string-append ":"
                     (symbol->string name)))))
      
      (inherit ->orig-so ->lex-so)
      
      (super-instantiate ())))
  
  (define (tidentifier=? a b)
    (eq? (send a get-symbol)
         (send b get-symbol)))

  (define ttuple%
    (class target%
      ;; sub-targets: (listof (is-a?/c target%))
      (init-field sub-targets)
      
      (define/override (set-bindings! enclosing-scope)
        (for-each (lambda (x) (send x set-bindings! enclosing-scope)) sub-targets))
      
      (define/override (to-description)
        `(ttuple% ,sub-targets))
      
      
      (define/public (get-sub-targets) sub-targets)
      
      (super-instantiate ())))
  
  (define tlist-display%
    (class target%
      ;; sub-targets: (listof (is-a?/c target%))
      (init-field sub-targets)

      (define/override (set-bindings! enclosing-scope)
        (for-each (lambda (x) (send x set-bindings! enclosing-scope)) sub-targets))

      (define/override (to-description)
        `(tlist-display% ,sub-targets))
      
      
      (define/public (get-sub-targets) sub-targets)
      
      (super-instantiate ())))
  
  (define tattribute-ref%
    (class target%
      ;; expression: (is-a?/c expression%)
      ;; identifier: (is-a?/c identifier%)
      (init-field expression identifier)

      (define/override (set-bindings! enclosing-scope)
        (send expression set-bindings! enclosing-scope))
      
      (define/override (to-description)
        `(tattribute-ref% ,expression ,identifier))
      
      (super-instantiate ())))
  
  (define tsubscription%
    (class target%
      ;; expression: (is-a?/c expression%)
      ;; sub: (is-a?/c expression%)
      (init-field expression sub)

      (define/override (set-bindings! enclosing-scope)
        (send expression set-bindings! enclosing-scope)
        (send sub set-bindings! enclosing-scope))
      
      (define/override (to-description)
        `(tsubscription% ,expression ,sub))
      
      (super-instantiate ())))
  
  (define tsimple-slicing%
    (class target%
      ;; expression: (is-a?/c expression%)
      ;; lower: (or/f false? (is-a?/c expression%))
      ;; upper: (or/f false? (is-a?/c expression%))
      (init-field expression lower upper)

      (define/override (set-bindings! enclosing-scope)
        (when expression (send expression set-bindings! enclosing-scope))
        (when lower (send lower set-bindings! enclosing-scope))
        (when upper (send upper set-bindings! enclosing-scope)))
      
      (define/override (to-description)
        `(tsimple-slicing% ,expression ,lower ,upper))
      
      (super-instantiate ())))
  )
  