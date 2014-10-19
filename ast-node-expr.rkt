(module ast-node-expression racket/base
  (require racket/class
           racket/list
           racket/match
           racket/function
           "ast-node.rkt"
           "ast-node-target.rkt"
           "bindings-mixin.rkt")
  
  (provide (all-defined-out))


  (define parameters%
    (class ast-node%
      
      ;; parm-tup is an identifier or a recurvise list of identifiers
      ;; -----------------------------------------------------------
      ;; (define (parm-tup? x)
      ;;   (or
      ;;    (is-a? x identifier%)
      ;;    (and (list? x) (andmap (lambda (x) (parm-tup? x)) x))))
      
      ;; parm-list: (listof (or/f (list/p (symbols 'dict) (is-a?/c identifier%))
      ;;                          (list/p (symbols 'seq) (is-a?/c identifier%))
      ;;                          (list/p (symbols 'key) parm-tup? (is-a?/c expression%))
      ;;                          (list/p (symbols 'pos) parm-tup?)))
      (init-field parm-list)
      
      ;; pos: (listof parm-tup?)
      (define pos null)
      ;; key: (cons/p parm-tup? (is-a?/c expression%))
      (define key null)
      ;; seq: (or/f false? (is-a?/c identifier%))
      (define seq #f)
      ;; dict: (or/f false? (is-a?/c identifier%))
      (define dict #f)
      
      ;; partition parm-list into pos, key, seq, dict
      (for-each
       (lambda (param)
         (cond
           ((eq? 'pos (car param))
            (cond
              ((null? key) (set! pos (cons (cadr param) pos)))
              (else
               (send (cadr param)
                     stx-err
                     "All parameters following a parameter with a default value must have a default value themselves"))))
           ((eq? 'key (car param))
            (set! key (cons (cons (cadr param) (caddr param)) key)))
           ((eq? 'seq (car param))
            (set! seq (cadr param)))
           (else
            (set! dict (cadr param)))))
       parm-list)
      
      ;; parm-tup-bindings: flattens parm-tup
      (define (parm-tup-bindings x)
        (cond
          ((is-a? x identifier%) (list x))
          (else (apply append (map parm-tup-bindings x)))))
      
      ;; all params in bindings form (flattened list)
      (define parm-bindings
        (append
         (apply append (map parm-tup-bindings pos))
         (apply append (map (lambda (x) (parm-tup-bindings (car x)))
                            key))
         (if seq (list seq) null)
         (if dict (list dict) null)))
      
      ;; checks for duplicate identifiers
      (let ((ht (make-hash)))
        (for-each (lambda (id)
                    (cond
                      ((hash-ref ht (send id get-symbol) (lambda () #f))
                       (send id stx-err "Duplicate identifier in function parameter list."))
                      (else
                       (hash-set! ht (send id get-symbol) (lambda () #f)))))
                  parm-bindings))
      
      
      (define (get-parm-list) parm-bindings)
      
      (define/override (set-bindings! enclosing-scope)
        (for-each (lambda (e) (send (cdr e) set-bindings! enclosing-scope)) key))
      
      (define/public (get-pos) (reverse pos))
      (define/public (get-key) (reverse key))
      (define/public (get-seq) seq)
      (define/public (get-dict) dict)
      
      (define/public (get-names)
        (append (map (lambda (p) (send (first-atom p) get-symbol))
                     (reverse pos))
                (map (lambda (k) (send (first-atom k) get-symbol))
                     (reverse key))))
      
      (define/public (get-default-values)
        (map (lambda (k) (send (cdr k) to-racket))
             (reverse key)))
      
      
      (inherit ->orig-so)
      
      
      (define/override (to-description)
        `(parameters% ,parm-list))
      
      (define/override (to-racket)
        (let ([Ps (map (lambda (p) (send (first-atom p) to-racket))
                       (reverse pos))]
              [Ks (map (lambda (k) `(,(send (car k) to-racket)
                                     ,(send (cdr k) to-racket)))
                       (reverse key))])
          (->orig-so
           (cond
             [(and seq dict) `(,@Ps ,@Ks #:dict ,(send dict to-racket) . ,(send seq to-racket))]
             [seq `(,@Ps ,@Ks . ,(send seq to-racket))]
             [dict `(,@Ps ,@Ks #:dict ,(send dict to-racket))]
             [else `(,@Ps ,@Ks)]))))
      
      (super-instantiate ())))
  
  (define expression%
    (class ast-node%
      (inherit stx-err)
      
      ;; to-target: -> (is-a?/c target%)
      ;; Raises an exception if the expression is not a valid
      ;; assignment target, otherwise returns a target%
      (define/public (to-target)
        (stx-err "Invalid target"))
      
      (define/override (to-description)
        (stx-err "Invalid usage of to-description on an expression% (I'm purely virtual)"))
      
      
      (define/override (to-racket)
        (stx-err "Invalid usage of to-racket on an expression% (I'm purely virtual)"))
      
      (super-instantiate ())))
    
  ;; 5.2.1
  (define identifier%
    (class expression%
      ;; name-string: string?  [or a symbol, sometimes, created by aug-assign%]
      (init name-string)
      ;; name: symbol?
      (define name (if (symbol? name-string)
                       name-string
                       (string->symbol name-string)))
      
      (define/public (get-symbol) name)
      
      (inherit-field start-pos end-pos)
      (define/override (to-target)
        (make-object tidentifier% name start-pos end-pos))
      

      (inherit ->orig-so ->lex-so)
      
      (define/override (to-description)
        `(identifier% ,(get-symbol)))
      
      (define/override (to-racket)
        (->orig-so (string->symbol
                    (string-append ":"
                     (symbol->string name)))))
      
      (define/public (to-racket-as-string)
        (->orig-so (symbol->string name)))
      
      (define/public (to-racket-as-symbol)
        (->orig-so `(quote ,name)))
      
      (super-instantiate ())))
   
  ;; 5.2.2
  (define literal%
    (class expression%
      
      ;; value: (or/f string? number?)
      (init-field type value)
      
      (define/public (get-value) value)
      
;      (define python-value
;        (case type
;          ['int (make-py-int value)]
;          ['long (make-py-long value)]
;          ['float (make-py-float value)]
;          ['complex (make-py-complex value)]
;          ['string (make-py-string value)]))
      
      (inherit ->orig-so stx-err)
      
      (define/override (to-description)
        `(literal% ,type ,value))
      
      (define/override (to-racket)
        (->orig-so value))
      
      (super-instantiate ())))
  
  ;; 5.2.3
  (define tuple%
    (class expression%
      
      ;; expressions: (listof (is-a?/c expression%))
      (init-field expressions)
      
      (inherit-field start-pos end-pos)
      (define/override (to-target)
        (make-object ttuple%
          (map (lambda (e) (send e to-target)) expressions) start-pos end-pos))
      
      (define (set-bindings! enclosing-scope)
        (for-each (lambda (e) (send e set-bindings! enclosing-scope)) expressions))
      
      (define/override (to-description)
        `(tuple% ,expressions))
      
      (inherit ->orig-so)
      (define/override (to-racket)
        (->orig-so `(make-py-tuple ,@(map send-to-racket expressions))))
      
      (super-instantiate ())))
  
  ;; 5.2.4
  (define list-display%
    (class expression%
      ;; expressions: (listof (is-a?/c expression%))
      (init-field expressions)
      
      (inherit-field start-pos end-pos)
      (define/override (to-target)
        (make-object tlist-display%
          (map (lambda (e) (send e to-target)) expressions) start-pos end-pos))
      
      (define/override (set-bindings! enclosing-scope)
        (for-each (lambda (e) (send e set-bindings! enclosing-scope)) expressions))
      
      (define/override (to-description)
        `(list-display% ,expressions))
      
      (inherit ->orig-so)
      ;;; TODO: evaluate at compile-time if it's a literal list
      (define/override (to-racket)
        (->orig-so `(make-py-list ,@(map send-to-racket expressions))))
      
      (super-instantiate ())))
  
  
  ;;;;;; [2*s for s in a]  .... expr = 2*s, for = for s in a .... for.targ = s, for.vals = a, for.iter = #f
  ;;;;;; [x+y for x,y in a] .... expr = x+y, for = for x,y in a .... for.targ = (x,y), for.vals = a, for.iter = #f
  
  (define list-comprehension%
    (class expression%
      ;; expr: (is-a?/c expression%)
      ;; for: (is-a?/c list-for%)
      (init-field expr for)
      
      (define/override (set-bindings! enclosing-scope)
        (send expr set-bindings! enclosing-scope)
        (send for set-bindings! enclosing-scope))
      
      (define/public (get-sub-clauses)
        (cons for (send for get-sub-clauses)))
      
      (define clauses (get-sub-clauses))
      
      (define/override (to-description)
        `(list-comprehension% ,expr ,for))
      
      (inherit ->orig-so)
      (define/override (to-racket)
        (let ([clauses-sexp (apply append (map send-to-racket clauses))])
          (->orig-so `(vector->py-list
                       (for*/vector ,clauses-sexp
                         ,(send expr to-racket))))))
      
      (super-instantiate ())))
  
  (define list-for%
    (class ast-node%
      ;; targ-exp: (is-a?/c expression%)
      (init-field targ-exp)
      ;; vals: (is-a?/c expression%)
      ;; iter: (or/f false? (is-a?/c list-for%) (is-a?/c list-if%))
      (init-field vals iter)
      
      ;; targ: (is-a?/c target%)
      (define targ (send targ-exp to-target))
      
      (define/override (set-bindings! enclosing-scope)
        (send vals set-bindings! enclosing-scope)
        (when iter (send iter set-bindings! enclosing-scope))
        (send targ set-bindings! enclosing-scope))
      

      ;; -> target%
      (define/public (get-targ) targ)
      

      ;; -> expression%
      (define/public (get-vals) vals)
      
      
      (define/public (get-sub-clauses)
        (if iter
            (cons iter (send iter get-sub-clauses))
            empty))
      
      
      (define/override (to-description)
        `(list-for% ,targ ,vals ,iter))
      

      (inherit ->orig-so)
      (define/override (to-racket)  ; returns list of SOs
        (list (->orig-so `[,(send targ-exp to-racket)
                           (->sequence ,(send vals to-racket))])))
              
;        (list (if (is-a? targ tidentifier%)
;                  (->orig-so `[,(send targ to-racket)
;                               (py-seq->sequence ,(send vals to-racket))])
;                  (->orig-so `[,(map send-to-racket (send targ get-sub-targets))
;                               (unpacker ,(send vals to-racket))]))))
      
      
      (define/public (to-racket-gen expr-node)
        (->orig-so `(py-for (,(send targ-exp to-racket) ,(send vals to-racket))
                      ,(if iter
                           (send iter to-racket-gen expr-node)
                           `(yield ,(send expr-node to-racket))))))
      
      
      (super-instantiate ())))
  
  (define list-if%
    (class ast-node%
      ;; test: (is-a?/c expression%)
      ;; iter: (or/f (is-a?/c list-for%) (is-a?/c list-if%))
      (init-field test iter)
      
      (define/override (set-bindings! enclosing-scope)
        (send test set-bindings! enclosing-scope)
        (when iter (send iter set-bindings! enclosing-scope)))

      
      (define/public (get-sub-clauses)
        (if iter
            (cons iter (send iter get-sub-clauses))
            empty))
      
      
      (define/override (to-description)
        `(list-if% test iter))
      

      (inherit ->orig-so)
      (define/override (to-racket)  ; returns list of SOs
        (list (->orig-so '#:when)
              (->orig-so `(py-truth ,(send test to-racket)))))
      
      
      (define/public (to-racket-gen expr-node)
        (->orig-so `(if (py-truth ,(send test to-racket))
                        ,(if iter
                             (send iter to-racket-gen expr-node)
                             `(yield ,(send expr-node to-racket))))))
      
      (super-instantiate ())))
  
  
  (define for-generator%
    (class expression%
      ;; expr: (is-a?/c expression%)
      ;; for: (is-a?/c list-for%)
      (init-field expr for)
      
      (define/override (set-bindings! enclosing-scope)
        (send expr set-bindings! enclosing-scope)
        (send for set-bindings! enclosing-scope))
      
         
      (define/override (to-description)
        `(for-generator% ,expr ,for))
      
      (inherit ->orig-so)
      (define/override (to-racket)
        (->orig-so `(make-py-generator
                     (generator ()
                       ,(send for to-racket-gen expr)))))
      
      (super-instantiate ())))
  
  
  ;; 5.2.5
  (define dictionary-display%
    (class expression%
      ;; key-values: (listof (list/p (is-a?/c expression%) (is-a?/c expression%)))
      (init-field key-values)
      
      (define/override (set-bindings! enclosing-scope)
        (for-each (lambda (x)
                    (send (car x) set-bindings! enclosing-scope)
                    (send (cadr x) set-bindings! enclosing-scope))
                  key-values))
      
      (define/override (to-description)
        `(dictionary-display% ,key-values))
      

      (inherit ->orig-so)
      (define/override (to-racket)
        (->orig-so `(make-py-dict (list ,@(map (lambda (p) `(cons ,(send (first p) to-racket)
                                                                  ,(send (second p) to-racket)))
                                               key-values)))))
      
      (super-instantiate ())))
  
  ;; 5.2.6
  (define string-conversion%
    (class expression%
      ;; expression: (is-a?/c expression%)
      (init-field expression)
      
      (define/override (set-bindings! enclosing-scope)
        (send expression set-bindings! enclosing-scope))
      
      (define/override (to-description)
        `(string-conversion ,expression))
      

      (inherit ->orig-so)
      (define/override (to-racket)
        (->orig-so `(py-string-convert ,(send expression to-racket))))
        ;; TODO implement at runtime module
      
      (super-instantiate ())))
  
  ;; 5.3.1
  (define attribute-ref%
    (class expression%
      ;; expression: (is-a?/c expression%)
      ;; identifier: (is-a?/c identifier%)
      (init-field expression identifier)
      
      (inherit-field start-pos end-pos)
      (define/override (to-target)
        (make-object tattribute-ref% expression identifier start-pos end-pos))
      
      (define/override (set-bindings! enclosing-scope)
        (send expression set-bindings! enclosing-scope))
      
      (define/override (to-description)
        `(attribute-ref% ,expression ,identifier))
      

      (inherit ->orig-so)
      (define/override (to-racket)
        (->orig-so `(py-get-attr ,(send expression to-racket)
                                 ,(send identifier to-racket-as-string))))
      
      (super-instantiate ())))
  
  ;; 5.3.2
  (define subscription%
    (class expression%
      ;; expression: (is-a?/c expression%)
      ;; subs: (is-a?/c expression%)
      (init-field expression sub)
      
      (inherit-field start-pos end-pos)
      (define/override (to-target)
        (make-object tsubscription% expression sub start-pos end-pos))
      
      (define/override (set-bindings! enclosing-scope)
        (send expression set-bindings! enclosing-scope)
        (send sub set-bindings! enclosing-scope))
      
      (define/override (to-description)
        `(subscription% ,expression ,sub))
      

      (inherit ->orig-so)
      (define/override (to-racket)
        (->orig-so `(py-get-index ,(send expression to-racket)
                                  ,(send sub to-racket))))
      
      (super-instantiate ())))
  
  ;; 5.3.3
  (define simple-slicing%
    (class expression%
      ;; expression: (is-a?/c expression%)
      ;; lower: (or/f false? (is-a?/c expression%))
      ;; upper: (or/f false? (is-a?/c expression%))
      (init-field expression lower upper)
      
      (inherit-field start-pos end-pos)
      (define/override (to-target)
        (make-object tsimple-slicing% expression lower upper start-pos end-pos))
      
      (define/override (set-bindings! enclosing-scope)
        (when lower (send lower set-bindings! enclosing-scope))
        (when upper (send upper set-bindings! enclosing-scope))
        (send expression set-bindings! enclosing-scope))
      
      (define/override (to-description)
        `(simple-slicing% ,expression ,lower ,upper))
      

      (inherit ->orig-so)
      (define/override (to-racket)
        (->orig-so `(py-get-slice ,(send expression to-racket)
                                  ,(if lower (send lower to-racket) 0)
                                  ,(if upper (send upper to-racket) +inf.0))))
      
      (super-instantiate ())))
  
  ;; 5.3.4
  (define call%
    (class expression%
      (inherit stx-err)
      
      ;; expression: (is-a?/c expression%)
      (init-field expression)
      
      ;; arg-list: (listof (or/f (list/p (symbols 'dict 'seq 'pos) (is-a?/c expression%))
      ;;                         (list/p (symbols 'key)
      ;;                                 (is-a?/c identifier%)
      ;;                                 (is-a?/c expression%))))
      (init arg-list)
     ; (printf "call arg-list: ~a~n" arg-list)
      ;; pos: (listof (is-a?/c expression%))
      (define pos null)
      ;; key: (listof (cons/p (is-a?/c identifier) (is-a?/c expression%)))
      (define key null)
      ;; seq: (or/f false? (is-a?/c expression%))
      (define seq #f)
      ;; dict: (or/f false? (is-a?/c expression%))
      (define dict #f)
      
      ;;;; TODO  must implement seq and dict arguments on parser level
      
      (for-each (lambda (arg)
                  (cond
                    ((eq? 'pos (car arg))
                     (cond
                       ((null? key)
                        (set! pos (cons (cadr arg) pos)))
                       (else
                        (send (cadr arg) stx-err
                              "positional argument cannot follow keyword argument"))))
                    ((eq? 'key (car arg))
                     (set! key (cons (cons (cadr arg) (caddr arg)) key)))
                    ((eq? 'seq (car arg))
                     (set! seq (cadr arg)))
                    (else
                     (set! dict (cadr arg)))))
                arg-list)
      
      ;; this is dumb but needed
      (define arg-list-copy arg-list)
      
      (define/override (set-bindings! enclosing-scope)
        (send expression set-bindings! enclosing-scope)
        (when seq (send seq set-bindings! enclosing-scope))
        (when dict (send dict set-bindings! enclosing-scope))
        (for-each (lambda (x) (send x set-bindings! enclosing-scope)) pos)
        (for-each (lambda (x) (send (cdr x) set-bindings! enclosing-scope)) key))
      
     
     
      (define/override (to-description)
        `(call% ,expression ,arg-list-copy))
      
      
      (inherit ->orig-so)
      (define/override (to-racket)
        (->orig-so 
         (if (empty? key)
             `(py-call ,(send expression to-racket)
                       ,@(map send-to-racket (reverse pos)))
             `(py-call/keywords ,(send expression to-racket)
                                ,(map send-to-racket (reverse pos))
                                ,(map (lambda (p) (list (send (car p) get-symbol)
                                                        (send (cdr p) to-racket)))
                                      (reverse key))))))
      
      (super-instantiate ())))
  
  
  ;; 5.4, 5.6, 5.7, 5.8, 5.10
  (define binary%
    (class expression%
      ;; lhs: (is-a?/c expression%)
      ;; op: (symbols 'or 'and '\| '^ '& '<< '>> '+ '- '* '/ '% '// '**)
      ;; rhs: (is-a?/c expression%)
      (init-field lhs op rhs)
      
      (define/override (set-bindings! enclosing-scope)
        (send lhs set-bindings! enclosing-scope)
        (send rhs set-bindings! enclosing-scope))
      
      (define python-operator
        (case op
          ['or  'py-or]
          ['and 'py-and]
          ['\|  'py-bwor]
          ['^   'py-bwxor]
          ['&   'py-bwand]
          ['<<  'py-lshift]
          ['>>  'py-rshift]
          ['+   'py-add]
          ['-   'py-sub]
          ['*   'py-mul]
          ['/   'py-div]
          ['%   'py-mod]
          ['//  'py-floordiv]
          ['**  'py-pow]))
      
      (define/override (to-description)
        `(binary% ,lhs ,op ,rhs))
      
      
      (inherit ->orig-so ->lex-so)
      (define/override (to-racket)
        (->orig-so `(,python-operator ,(send lhs to-racket) ,(send rhs to-racket))))
      
      (super-instantiate ())))
  
  ;; 5.5, 5.10
  (define unary%
    (class expression%
      ;; op: (symbols 'not '+ '- '~)
      ;; rhs: (is-a?/c expression%)
      (init-field op rhs)
      
      (define/override (set-bindings! enclosing-scope)
        (send rhs set-bindings! enclosing-scope))
      
      (define python-operator
        (case op
          ['not 'py-not]
          ['+   'py-unary-plus]
          ['-   'py-unary-minus]
          ['~   'py-bwinvert]))
      
      (define/override (to-description)
        `(unary% ,op ,rhs))
      
      
      (inherit ->orig-so)
      (inherit-field start-pos end-pos)
      (define/override (to-racket)
        (->orig-so `(,python-operator ,(send rhs to-racket))))
      
      (super-instantiate ())))
  
  
  ;; 5.9
  (define comparison%
    (class expression%
      
      ;; comps: (listof (or/f (is-a?/c expression) 
      ;;                      (symbols '< '> '== '>= '<= '<> '!= 'in 'notin 'is 'isnot)))
      ;; expression% oper expression% oper ... expression%
      (init-field comps)
      
      (define/override (set-bindings! enclosing-scope)
        (for-each (lambda (x)
                    (unless (symbol? x) (send x set-bindings! enclosing-scope)))
                  comps))

      ;; scheme-op->python-op:  symbol -> syntax-object
      (define (python-operator sym)
        (case sym
          ['<     'py-lt]
          ['>     'py-gt]
          ['==    'py-eq]
          ['>=    'py-ge]
          ['<=    'py-le]
          ['<>    'py-ne]
          ['!=    'py-ne]
          ['in    'py-in]
          ['notin 'py-notin]
          ['is    'py-is]
          ['isnot 'py-isnot]))
      
      
      (define (separate-comparisons comp-list)
        (match comp-list
          [(list expr1 op expr2) (list (list expr1 op expr2))]
          [(list-rest expr1 op expr2 rest) (cons (list expr1 op expr2)
                                                 (separate-comparisons (cons expr2 rest)))]))
        
      
      
      (define/override (to-description)
        `(comparison% ,comps))
      
      
      (inherit ->orig-so)
      (define/override (to-racket)
        (let* ([comparisons (separate-comparisons comps)]
               [comps-so (map (lambda (comp)
                                ;(op expr1 expr2)
                                `(,(python-operator (second comp)) ,(send (first comp) to-racket) ,(send (third comp) to-racket)))
                              comparisons)])
          (if (= (length comps-so) 1)
              (->orig-so (first comps-so))
              (->orig-so `(py-and ,@comps-so)))))
      ;; TODO let on arguments to avoid duplicate evaluation
      
      (super-instantiate ())))
  
  
  (define conditional-expr%
    (class expression%
      
      ;; body: (is-a?/c expression%)
      ;; test: (is-a?/c expression%)
      ;; else: (is-a?/c expression%)
      (init-field body test else)
      
      (define/override (set-bindings! enclosing-scope)
        (send body set-bindings! enclosing-scope)
        (send test set-bindings! enclosing-scope)
        (send else set-bindings! enclosing-scope))
      
      (define/override (to-description)
        `(conditional-expr% ,body ,test, else))
      
      (inherit ->orig-so)
      (define/override (to-racket)
        (->orig-so `(if (py-truth ,(send test to-racket))
                        ,(send body to-racket)
                        ,(send else to-racket))))
        
      (super-instantiate ())))
      
      
      
  
  ;; 5.10
  (define lambda%
    (bindings-mixin
     (binding-scope-base/default-mixin
    (class expression%
      
      ;; parms: (is-a?/c parameters%)
      ;; body: (is-a?/c expression%)
      (init-field parms body)
      
      (define/override (set-bindings! enclosing-scope)
        (send parms set-bindings! enclosing-scope)
        (send body set-bindings! this))
      
      (define/public (get-parms) parms)
      
      (define/override (to-description)
        `(lambda% ,parms
                  (bindings: ,@(send this get-bindings))
                  (globals: ,(send this get-global-table))
                  ,body))
      
      
      (inherit ->orig-so)
      (define/override (to-racket)
         (let* 
             ([bindings-void (map (lambda (binding)
                                    `[,(send binding to-racket) (void)])
                                  (send this get-bindings))]
              
              [body-so (send body to-racket)]
              
              [body+bindings-so (if (not (empty? bindings-void))
                                    `(let ,bindings-void ,body-so)
                                    body-so)])
               
           (->orig-so `(make-py-lambda with-params ,(send parms get-names)
                         (lambda ,(send parms to-racket) ,body+bindings-so)))))
      
      (super-instantiate ())))))
  
  )
