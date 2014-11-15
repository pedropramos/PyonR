(module ast-node-stmt racket/base
  (require racket/class
           racket/list
           racket/function
           racket/match
           mzlib/etc
           "ast-node.rkt"
           "ast-node-expr.rkt"
           "ast-node-target.rkt"
           "bindings-mixin.rkt")

  (provide (all-defined-out))
  
  
  (define using-return-ec? (make-parameter #f))
  (define using-break-ec? (make-parameter #f))
  (define using-continue-ec? (make-parameter #f))
    
  (define statement%
    (class ast-node%
      ;; check-break/cont: (or/f false? (is-a?/c statement%)) ->
      ;; enclosing-loop is the nearest enclosing loop statement to this
      ;; statement, or #f if there is none.  Check that all break
      ;; and continue statements are inside some loop and annotate the
      ;; enclosing-loop that it contains a break or continue.
      (define/public (check-break/cont enclosing-loop) void)

      ;; collect-globals: -> (listof symbol?)
      ;; Return the list of all variables in "global" statements in the 
      ;; current scope.
      (define/public (collect-globals) null)
      
      (define/override (to-description)
        (stx-err (format "Invalid usage of to-description on a statement% (I'm purely virtual): ~a" this)))
      
      
      (inherit stx-err)
      (define/override (to-racket)
        (stx-err (format "Invalid usage of to-racket on a statement% (I'm purely virtual): ~a" this)))

      ;; do nothing for most statements
      (define/public (set-tail-position!)
        void)
      
      ;; needs-escape-continuation?: symbol -> bool
      ;; determine whether this is a non-tail return/break/continue statement
      ;; or a compound statement with a non-tail return/break/continue part
      (define/public (needs-escape-continuation? ec)
        #f)
      
      ;; yields?: -> bool
      ;; determine whether this statement might yield
      (define/public (yields?)
        #f)
      
      ;; return-is-last?: -> bool
      ;; determine whether the last substatement of a compound is a return,
      ;; so that suite% can omit the default py-None return at the end of a function
      (define/public (return-is-last?)
        #f)
      
      (super-instantiate ())))
  
  (define compound-statement% 
    (class statement%
      (init-field substatements)
      
      (define/override (yields?)
        (ormap (lambda (s)
                 (send s yields?))
               substatements))
      
      (define/override (return-is-last?)
        (send (last substatements) return-is-last?))
      
      (super-new)))

  ;; 6.1
  (define expr-stmt%
    (class statement%
      ;; expression: (is-a?/c expression%)
      (init-field expression)
      
      (define/override (set-bindings! enclosing-scope)
        (send expression set-bindings! enclosing-scope))
      
      (define/override (to-description)
        `(expr-stmt% ,expression))
      
      (inherit ->orig-so)
      (define/override (to-racket)
        (->orig-so `(expr-stmt ,(send expression to-racket))))
      
      (super-instantiate ())))
      
  ;; 6.2
  (define assert%
    (class statement%
      ;; expr1: (is-a?/c expression%)
      ;; expr2: (or/f false? (is-a?/c expression%))
      (init-field expr1 expr2)
      
      (define/override (set-bindings! enclosing-scope)
        (send expr1 set-bindings! enclosing-scope)
        (when expr2 (send expr2 set-bindings! enclosing-scope)))
      
      (define/override (to-description)
        `(assert% ,expr1 ,expr2))
      
      
      (inherit ->orig-so)
      (define/override (to-racket)
        (if expr2
            (->orig-so `(py-assert ,(send expr1 to-racket) ,(send expr2 to-racket)))
            (->orig-so `(py-assert ,(send expr1 to-racket)))))
      
      
      (super-instantiate ())))
  
  ;; 6.3
  (define assignment%
    (class statement%
      ;; targ-exps: (listof (is-a?/c expression%))
      (init-field targ-exps)
      ;; expression: (is-a?/c expression%)
      (init-field expression)
      ;; targets: (listof (is-a?/c target%))
      (define targets (map (lambda (e)
                             (send e to-target))
                           targ-exps))
      
      
      (define scope #f)
      (define (top?)
        (or (is-a? scope module-scope%)
            (is-a? scope class-definition%)))
      
      
      (define/public (get-targs) targets)
      
      
      (define/public (get-scope) scope)
      
      (define/override (set-bindings! enclosing-scope)
        (set! scope enclosing-scope)
        (send expression set-bindings! enclosing-scope)
        (for-each (lambda (e) (send e set-bindings! enclosing-scope)) targets))
      
      
      (define (binding-targ? t)
        (or (not (send scope is-bound? t))
            (eq? (send scope binding-tid t) t)))
      
      (define (def-targ? t)
        (and (top?)
             (not (attr-targ? t))
             (binding-targ? t)))
      
      (define (set-targ? t)
        (and (not (attr-targ? t))
             (not (def-targ? t))))
      
      (define (attr-targ? t)
        (is-a? t tattribute-ref%))  
      
      
      (define/override (to-description)
        `(assignment% ,targets ,expression))
      
      
      (inherit ->orig-so)
      (define/override (to-racket)
        (if (= (length targ-exps) 1)
            (->orig-so `(py-assign! ,(send (first targ-exps) to-racket)
                                    ,(send expression to-racket)))
            (->orig-so `(py-multiple-assign! ,(map send-to-racket targ-exps)
                                             ,(send expression to-racket)))))
      
      (super-instantiate ())))
  
  ;;6.3.1
  (define aug-assignment%
    (class statement%
      ;; targ-exp: (is-a?/c expression%)
      (init targ-exp)

      ;; op: (symbols '+= '-= '*= '/= '%= '&= '\|= '^= '<<= '>>= '**= '//=)
      ;; expression: expression%
      (init-field op expression)

      ;(define target (send targ-exp to-target))
      
      (define/override (set-bindings! enclosing-scope)
        (send expression set-bindings! enclosing-scope))
       ; (send target set-bindings! enclosing-scope))

      (define inplace-op (case op
                           [(+=) 'py-iadd]
                           [(-=) 'py-isub]
                           [(*=) 'py-imul]
                           [(/=) 'py-idiv]
                           [(%=) 'py-imod]
                           [(&=) 'py-ibwand]
                           [(\|=) 'py-ibwor]
                           [(^=) 'py-ibwxor]
                           [(<<=) 'py-ilshift]
                           [(>>=) 'py-irshift]
                           [(**=) 'py-ipow]
                           [(//=) 'py-ifloordiv]))
      
      (define/override (to-description)
        `(aug-assignment% ,orig-targ-exp ,op ,expression))
      
      
      (inherit ->orig-so)
      (inherit-field start-pos end-pos)
      (define orig-targ-exp targ-exp)
      
      (define/override (to-racket)
        (let* ([expression-node (make-object already-compiled-expression%
                                  (->orig-so `(,inplace-op
                                               ,(send orig-targ-exp to-racket)
                                               ,(send expression to-racket)))
                                  start-pos end-pos)]
               [assignment-node (make-object assignment%
                                 (list orig-targ-exp) expression-node start-pos end-pos)])
          (send assignment-node to-racket)))
      
      (super-instantiate ())))

  ;; 6.4
  (define pass%
    (class statement%
      
      (define/override (to-description)
        `(pass%))
      
      
      (inherit ->orig-so)
      (define/override (to-racket)
        (->orig-so 'py-None))
      
      (super-instantiate ())))
  
  ;; 6.5
  ;;; TODO: handle local vars, subscripts
  (define del%
    (class statement%
      ;; targ-exp: (is-a?/c expression%)
      (init targ-exp)
      ;; target: (is-a?/c target%)
      (define target (send targ-exp to-target))

      (define the-targ-exp targ-exp)
      (define/override (set-bindings! enclosing-scope)
        (send target set-bindings! enclosing-scope))
      
      (define/override (to-description)
        `(del% ,target))
      
      (inherit ->orig-so stx-err)
      (define/override (to-racket)  ;; TODO
        (printf "del targ-exp: ~v, target: ~v~n" the-targ-exp target)
        (del-target-to-racket target))
        
      (define (del-target-to-racket target)
        (printf "del-target-to-racket: ~v~n" target)
        (->orig-so (cond
                     [(is-a? target tidentifier%) `(namespace-set-variable-value! ',(send target to-racket) 'undefined)]
                     [(is-a? target ttuple%)
                        `(begin ,@(map del-target-to-racket
                                       (send target get-sub-targets)))]                     
                     [else (stx-err "del statement not fully implemented yet.")])))
      
      (super-instantiate ())))

  ;; 6.6
  (define print%
    (class statement%
      ;; to-file?: any?
      ;; expressions: (listof (is-a?/c expression%)
      (init-field to-file? expressions)
      
      ;; if present, file is the first argument in expressions
      
      (define/override (set-bindings! enclosing-scope)
        (for-each (lambda (e) (send e set-bindings! enclosing-scope)) expressions))
      
      
      (define/override (to-description)
        `(print% ,to-file? ,expressions))
      
      
      (inherit ->orig-so ->lex-so)
      (define/override (to-racket)
        (if to-file?
            (->orig-so `(py-print ,@(map send-to-racket (rest expressions))
                                  #:port (unwrap-port ,(send (first expressions) to-racket))))
            (->orig-so `(py-print ,@(map send-to-racket expressions)))))
      
      (super-instantiate ())))
  
  ;; 6.7
  (define return%
    (class statement%
      ;; expression: (or/f false? (is-a?/c expression%)
      (init-field expression)
      
      (inherit stx-err)
      (define scope #f)
      
      (define/override (set-bindings! enclosing-scope)
        (set! scope enclosing-scope)
        (unless (is-a? enclosing-scope function-definition%)
          (stx-err "'return' outside function"))
        (when expression
          (send expression set-bindings! enclosing-scope)))

      (define tail-position? #f)
      
      ;; pleeeease let it be true :)
      (define/override (set-tail-position!)
        (set! tail-position? #t))
      
      (define/override (needs-escape-continuation? ec)
        (and (not tail-position?)
             (eq? ec (send scope get-return-symbol))))
      
      (define/override (return-is-last?) #t)
      
      (define/override (to-description)
        `(return% ,expression))
      
      
      (inherit ->orig-so ->lex-so)
      (define/override (to-racket)
        (let ([expr-so (if expression
                           (send expression to-racket)
                           'py-None)])
          (if (and tail-position?
                   (not (using-return-ec?)))
              (->orig-so expr-so)
              (->orig-so `(,(send scope get-return-symbol) ,expr-so)))))
      
      (super-instantiate ())))

  ;; 6.8
  (define yield%
    (class statement%
      ;; expression: (is-a?/c expression%)
      (init-field expression)

      (define/override (set-bindings! enclosing-scope)
        (send expression set-bindings! enclosing-scope))
      
      (define/override (yields?) #t)
      
      (define/override (to-description)
        `(yield% ,expression))
      
      (inherit ->orig-so)
      (define/override (to-racket)
        (->orig-so `(yield ,(send expression to-racket))))
      
      (super-instantiate ())))
  
  ;; 6.9
  (define raise%
    (class statement%

      ;; type: (or/f (false? is-a?/c expression%))
      ;; parm: (or/f (false? is-a?/c expression%))
      ;; traceback: (or/f (false? is-a?/c expression%))
      (init-field type parm traceback)

      (define/override (set-bindings! enclosing-scope)
        (when type (send type set-bindings! enclosing-scope))
        (when parm (send parm set-bindings! enclosing-scope))
        (when traceback (send traceback set-bindings! enclosing-scope)))
      
      (define/override (to-description)
        `(raise% ,type ,parm ,traceback))
      
      (inherit ->orig-so)
      (define/override (to-racket)
        (cond
          [(and (not type) (not parm))
           (->orig-so `(raise (current-exception)))]
          [(not parm)
           (->orig-so `(py-raise ,(send type to-racket)))]
          [else
           (->orig-so `(py-raise/2 ,(send type to-racket) ,(send parm to-racket)))]))
      
      (super-instantiate ())))
  
  ;; 6.10
  (define break%
    (class statement%
      (inherit stx-err)
      
      (define loop #f)
      
      (define/override (check-break/cont enclosing-loop)
        (set! loop enclosing-loop)
        (if enclosing-loop
            (send enclosing-loop set-can-break?)
            (stx-err "Break statement must be within loop")))
      
      
      (define/override (to-description)
        `(break%))

      (inherit ->orig-so)
      (define/override (to-racket)
        (->orig-so `(,(send loop get-break-symbol) (void))))
      
      (super-instantiate ())))

  ;; 6.11
  (define continue%
    (class statement%
      (inherit stx-err)

      (define loop #f)
      
      (define/override (check-break/cont enclosing-loop)
        (set! loop enclosing-loop)
        (if enclosing-loop
            (send enclosing-loop set-can-cont?)
            (stx-err "Continue statement must be within loop")))
      
      (define/override (to-description)
        `(continue%))
      
      (inherit ->orig-so)
      (define/override (to-racket)
        (->orig-so `(,(send loop get-continue-symbol) )))
      
      (super-instantiate ())))
  
  (define already-compiled-expression%
    (class expression%
      (init-field syntax-object)
      (define/override (to-racket)
        syntax-object)
      
      (super-instantiate ())))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; just some utilities for imports, carry on
  
  (define (identifier-list->dotted-string id-lst [separator "."])
    (foldr (lambda (a b)
             (if (string=? b "")
                 a
                 (string-append a separator b)))
           ""
           (for/list ([id id-lst])
             (syntax->datum (send id to-racket-as-string)))))
               
  
  (define (python-module-spec id-list)
    (identifier-list->dotted-string id-list "/"))
  
  (define (racket-module-spec literal)
    (read (open-input-string (send literal get-value))))
  
  
  ;; 6.12
  (define import-module%
    (class statement%
      ;; modules: (listof (or/f (list/p (listof (is-a?/c identifier%))        <-- python module
      ;;                                (or/f false? (is-a?/c identifier%))
      ;;                                (symbols 'python)))
      ;;                        (list/p (is-a?/c literal%)                    <-- racket module
      ;;                                (is-a?/c identifier%)
      ;;                                (symbols 'racket))))
      (init-field modules)

      (define scope #f)
      (define (top?) (is-a? scope module-scope%))
      (define/public (get-scope) scope)
      
      (define (lang module)
        (third module))
      
      (define/override (set-bindings! enclosing-scope)
        (when enclosing-scope
          (set! scope enclosing-scope)
          (for-each (lambda (module)
                      (let ([id (or (cadr module)
                                    (car (car module)))])
                        (unless (or (send enclosing-scope is-local? id)
                                    (top?))
                          (send enclosing-scope add-binding id))))
                    modules)))
      
      (define/override (to-description)
        `(import-module% ,modules))
      
      (inherit ->orig-so ->lex-so)
      (inherit-field start-pos end-pos)
      
      (define/override (to-racket)
        (->orig-so `(begin
                      ,@(for/list ([module modules])
                          (case (lang module)
                            [(python) (let ([module-spec (python-module-spec (first module))]
                                            [id (or (second module) (last (first module)))])
                                        `(py-import ,module-spec as ,(send id to-racket)))]
                            [(racket) (let ([module-spec (racket-module-spec (first module))]
                                            [id (second module)])
                                        `(py-import-mangle ,module-spec as ,(send id to-racket)))])))))
      
      (super-instantiate ())))
  
  
  (define cpy-import-module%
    (class import-module%
      ;; modules: (listof (list/p (listof (is-a?/c identifier%))
      ;;                          (or/f false? (is-a?/c identifier%))))
      
      (inherit-field modules)
      (define/override (to-description)
        `(cpy-import-module% ,modules))
      
      (inherit ->orig-so)
      (define/override (to-racket)
        (->orig-so `(begin
           ,@(map (lambda (module)
                    (let* ([module-name (identifier-list->dotted-string (first module))]
                           [id (or (second module)
                                   (last (first module)))])
                      (->orig-so `(cpy-import ,module-name as ,(send id to-racket)))))
                  modules))))
      
      (super-instantiate ())))
  
  
  

  (define import-from%
    (class statement%
      ;; module: (listof (is-a?/c identifier%))
      ;; ids: (or/f (symbols '*) (listof (list/p (is-a?/c identifier%)
      ;;                                         (or/f false? (is-a?/c identifier%))))
      ;; lang: 'python
      ;;
      ;; OR
      ;;
      ;; module: (is-a?/c literal%)
      ;; ids: (or/f (symbols '*) (listof (list/p (is-a?/c identifier%)
      ;;                                         (or/f false? (is-a?/c identifier%))))
      ;; lang: 'racket
      (init-field module ids lang)
      
      (define/override (set-bindings! enclosing-scope)
        (unless (symbol? ids) (map (lambda (b) (if (cadr b) (cadr b) (car b))) ids)))
      
      (define/override (to-description)
        `(import-from ,module ,ids))
      
      (inherit ->orig-so)
      (define/override (to-racket)
        (if (eq? ids '*)
            (to-racket/*)
            (to-racket/ids)))
      
      (define (to-racket/ids)
        (case lang
          [(python) (->orig-so `(py-from ,(python-module-spec module) import ,(for/list ([id ids])
                                                                                 (let ([imported-id (send (car id) to-racket)]
                                                                                       [as-id (send (or (cadr id) (car id)) to-racket)])
                                                                                   `(,imported-id as ,as-id)))))]
          [(racket) (->orig-so `(py-from-mangle ,(racket-module-spec module) import ,(for/list ([id ids])
                                                                                 (let ([imported-id (send (car id) get-symbol)]
                                                                                       [as-id (send (or (cadr id) (car id)) to-racket)])
                                                                                   `(,imported-id as ,as-id)))))]))
      
      (define (to-racket/*)
        (case lang
          [(python) (->orig-so `(py-from-import-* ,(python-module-spec module)))]
          [(racket) (->orig-so `(py-from-import-*-mangle ,(racket-module-spec module)))]))
      
      (super-instantiate ())))
  
  
  (define cpy-import-from%
    (class import-from%
      ;; module: (listof (is-a?/c identifier%)
      ;; ids: (or/f (symbols '*) (listof (list/p (is-a?/c identifier%)
      ;;                                         (or/f false? (is-a?/c identifier%)))))
      
      (inherit-field module ids)
      (define/override (to-description)
        `(cpy-import-from ,module ,ids))
      
      (inherit ->orig-so)
      (define/override (to-racket)
        (let ([module-name (identifier-list->dotted-string module)])
          (if (eq? ids '*)
              (->orig-so `(cpy-from-import-* ,module-name))
              (->orig-so `(cpy-from ,module-name import ,(map (lambda (id)
                                                                (let ([imported-name (send (car id) to-racket-as-string)]
                                                                      [as-id (send (or (cadr id) (car id)) to-racket)])
                                                                  `(,imported-name as ,as-id)))
                                                              ids))))))
      
      (super-instantiate ())))
  
  
  
  
  ;; 6.13
  (define global%
    (class statement%
      ;; identifiers: (listof (is-a?/c identifier%))
      (init-field identifiers)

      (define/override (collect-globals) identifiers)
      
      (define/override (to-description)
        `(global% ,identifiers))
      
      ;; doesn't actually _do_ anything at runtime
      (inherit-field start-pos end-pos)
      (define/override (to-racket)
        (send (make-object pass% start-pos end-pos) to-racket))
      
      (super-instantiate ())))

  ;; 6.14
  (define exec%
    (class statement%
      ;; exp1: (or/f false (is-a?/c expression))
      ;; exp2: (or/f false (is-a?/c expression))
      ;; exp3: (or/f false (is-a?/c expression))
      (init-field exp1 exp2 exp3)

      (define/override (set-bindings! enclosing-scope)
        (when exp1 (send exp1 set-bindings! enclosing-scope))
        (when exp2 (send exp2 set-bindings! enclosing-scope))
        (when exp3 (send exp3 set-bindings! enclosing-scope)))
      
      (define/override (to-description)
        `(exec% ,exp1 ,exp2 ,exp3))
      
      (inherit ->orig-so)
      (define/override (to-racket)
        (->orig-so `(py-exec ,(if exp1 (send exp1 to-racket) 'py-None)
                             ,(if exp2 (send exp2 to-racket) 'py-None)
                             ,(if exp3 (send exp3 to-racket) 'py-None))))
      
      (super-instantiate ())))
  
  (define (bindings->los b)
    (cond
      [(list? b) (map bindings->los b)]
      [else (send b get-symbol)]))
  
  ;; 7
  (define suite%
    (class compound-statement%
      
      ;; statements: (listof (is-a?/c statement%))
      (init-field statements)
      (define (sub-stmt-map f)
        (map f statements))
      
      (define scope #f)
      (define/public (get-scope) scope)
      
      (define/public (get-statements) statements)
      
      (define/override (set-bindings! enclosing-scope)
        (set! scope enclosing-scope)
        (for-each (lambda (s) (send s set-bindings! enclosing-scope)) statements))

      (define/override (check-break/cont enclosing-loop)
        (void (sub-stmt-map (lambda (s) (send s check-break/cont enclosing-loop)))))

      (define/override (collect-globals)
        (apply append (sub-stmt-map (lambda (s) (send s collect-globals)))))

      (define last-statement (car (last-pair statements)))
      
      (define/override (set-tail-position!)
        (send last-statement set-tail-position!))
      
      (define/override (to-description)
        `(suite% ,statements))
       

      (define/override (needs-escape-continuation? ec)
        (ormap (lambda (statement)
                 (send statement needs-escape-continuation? ec))
               statements))

      ;; compound-statement% already implements this method,
      ;; but we already have a ref to the last statement,
      ;; so this is faster
      (define/override (return-is-last?)
        (send last-statement return-is-last?))
      
      
      
      (inherit ->orig-so)
      (define/override (to-racket [escape-continuation-symbol #f] [lambda-suite? #f] [def-suite? #f])
        (let*
            ([using-ec? (and escape-continuation-symbol
                             (needs-escape-continuation? escape-continuation-symbol))]
             
             [insert-void-return? (and def-suite?
                                       (not (return-is-last?)))]
             
             [statements-so (parameterize ([using-return-ec? (or (and using-ec? def-suite?)
                                                                 (using-return-ec?))]
                                           ;; TODO: implement these two
                                           [using-break-ec? #f]
                                           [using-continue-ec? #f])
                              (sub-stmt-map send-to-racket))]
             
             [bodies-so (if insert-void-return?
                            (append statements-so (list 'py-None))
                            statements-so)])
          
          (cond
            [using-ec?                 (->orig-so `(let/ec ,escape-continuation-symbol ,@bodies-so))]
            [(= (length bodies-so) 1)  (->orig-so (first bodies-so))]
            [else                      (->orig-so `(begin ,@bodies-so))])))
      
      (super-new (substatements statements))))
  
  
  ;; 7.1
  (define if%
    (class compound-statement%
      ;; test-bodies: (listof (list/p (is-a?/c expression%) (is-a?/c suite%)))
      ;; else: (or/f (is-a?/c suite%) false?)
      (init-field test-bodies else)

      (define (sub-stmt-map f)
        (append
         (map (lambda (x) (f (cadr x))) test-bodies)
         (if else
             (list (f else))
             null)))
      
      (define/override (set-bindings! enclosing-scope)
        (for-each (lambda (x)
                    (send (car x) set-bindings! enclosing-scope)
                    (send (cadr x) set-bindings! enclosing-scope))
                  test-bodies)
        (when else (send else set-bindings! enclosing-scope)))
      
      (define/override (collect-globals)
        (apply append (sub-stmt-map (lambda (s) (send s collect-globals)))))
      
      (define/override (check-break/cont enclosing-loop)
        (void (sub-stmt-map (lambda (s) (send s check-break/cont enclosing-loop)))))

      (define/override (set-tail-position!)
        (for-each (lambda (test&thn)
                    (send (second test&thn) set-tail-position!))
                  test-bodies)
        (when else
          (send else set-tail-position!)))
      
      (define/override (needs-escape-continuation? ec)
        (or (ormap (lambda (test&thn)
                     (send (second test&thn) needs-escape-continuation? ec))
                   test-bodies)
            (and else (send else needs-escape-continuation? ec))))
      
      (define/override (to-description)
        `(if% ,test-bodies ,else))
      
      
      (inherit ->orig-so)
      (define/override (to-racket)
        (let ([ifs-so (map (lambda (clause)
                             `[(py-truth ,(send (first clause) to-racket)) ,(send (second clause) to-racket)])
                           test-bodies)]
              [else-so (if else
                           `[else ,(send else to-racket)]
                           `[else py-None])])
          (->orig-so `(cond ,@ifs-so ,else-so))))
      
      (super-new [substatements (sub-stmt-map identity)])))

  ;; 7.2
  (define while%
    (class compound-statement%

      ;; test: (is-a?/c expression%)
      ;; body: (is-a?/c suite%)
      ;; else: (or/f false? (is-a?/c suite%))
      (init-field test body else)
      (define can-break? #f)
      (define can-cont? #f)
      (define/public (set-can-break?) (set! can-break? #t))
      (define/public (set-can-cont?) (set! can-cont? #t))
      
      (define/override (set-bindings! enclosing-scope)
        (send test set-bindings! enclosing-scope)
        (send body set-bindings! enclosing-scope)
        (when else (send else set-bindings! enclosing-scope)))

      (define/override (collect-globals)
        (append (send body collect-globals)
                (if else (send else collect-globals) null)))
      
      (define/override (check-break/cont enclosing-loop)
        (send body check-break/cont this)
        (when else
          (send else check-break/cont enclosing-loop)))

      (define break-symbol (gensym 'break))
      (define continue-symbol 'continue)
      
      (define/public (get-break-symbol) break-symbol)
      (define/public (get-continue-symbol) continue-symbol)

      (define/override (set-tail-position!)
        (when else
          (send else set-tail-position!)))
      
      (define/override (needs-escape-continuation? ec)
        (or (send body needs-escape-continuation? ec)
            (and else (send else needs-escape-continuation? ec))))
      
      (define/override (to-description)
        `(while% ,test ,body ,else))
      
      (inherit ->orig-so)
      (define/override (to-racket)
        (let* ([stmt `(let ,continue-symbol ()
                        (when (py-truth ,(send test to-racket))
                          (begin
                            ,(send body to-racket continue-symbol)
                            (,continue-symbol))))]
               [stmt (if can-break?
                         `(let/ec ,break-symbol ,stmt)
                         stmt)]
               [stmt (if else
                         `(begin ,stmt ,(send else to-racket))
                         stmt)])
          (->orig-so stmt)))
      
      
      (super-new [substatements (if else
                                    (list body else)
                                    (list body))])))
  
  ;; 7.3
  (define for%
    (class compound-statement%
      
      ;; targ-exp: (is-a?/c expression)
      (init-field targ-exp)

      ;; vals: (is-a?/c expression%)
      ;; body: (is-a?/c suite%)
      ;; else: (or/f false? (is-a?/c suite%))
      (init-field vals body else)

      ;; target: (is-a?/c target%)
      (define target (send targ-exp to-target))
      (define can-break? #f)
      (define can-cont? #f)
      (define/public (set-can-break?) (set! can-break? #t))
      (define/public (set-can-cont?) (set! can-cont? #t))
      
      (define scope #f)
      (define/public (get-scope) scope)
      
      (define/override (set-bindings! enclosing-scope)
        (set! scope enclosing-scope)
        (send target set-bindings! enclosing-scope)
        (send body set-bindings! enclosing-scope)
        (when else (send else set-bindings! enclosing-scope)))
      
      (define/override (collect-globals)
        (append (send body collect-globals)
                (if else (send else collect-globals) null)))
      
      (define/override (check-break/cont enclosing-loop)
        (send body check-break/cont this)
        (when else
          (send else check-break/cont enclosing-loop)))
      
      (define break-symbol (gensym 'break))
      (define continue-symbol 'continue)
      
      (define/public (get-break-symbol) break-symbol)
      (define/public (get-continue-symbol) continue-symbol)

      (define/override (set-tail-position!)
        (when else (send else set-tail-position!)))

      (define/override (needs-escape-continuation? ec)
        (or (send body needs-escape-continuation? ec)
            (and else (send else needs-escape-continuation? ec))))

      (define/override (to-description)
        `(for% ,target ,vals ,body ,else))
      
      
      (inherit ->orig-so)
      (define/override (to-racket)
        (let* ([for-name (if can-cont? 'py-for/continue 'py-for)]
               [for-stmt `(,for-name (,(send targ-exp to-racket) ,(send vals to-racket))
                            ,(send body to-racket continue-symbol))]
               [for-stmt (if can-break?
                             `(let/ec ,break-symbol ,for-stmt)
                             for-stmt)]
               [for-stmt (if else
                             `(begin ,for-stmt ,(send else to-racket))
                             for-stmt)])
          (->orig-so for-stmt)))
      
      (super-new [substatements (if else
                                    (list body else)
                                    (list body))])))
  
  
  ;; 7.4
  (define try-except%
    (class compound-statement%
      (inherit stx-err)
      ;; body: (is-a?/c suite%)
      (init-field body)
      ;; excepts: (listof (list/p (or/f false? (is-a?/c expression%))
      ;;                          (or/f false? (is-a?/c expression%))
      ;;                          (is-a?/c suite%)))
      (init excepts)
      ;; else: (or/f false? (is-a?/c suite%))
      (init-field else)
      ;; finally: (or/f false? (is-a?/c suite%))
      (init-field finally)
      
      (define (sub-stmt-map f)
        (cons (f body)
              (append
               (map (lambda (x) (f (caddr x))) exceptions)
               (if else
                   (list (f else))
                   null)
               (if finally
                   (list (f finally))
                   null))))
      
      ;; exceptions: (listof (list/p (or/f false? (is-a?/c expression%))
      ;;                             (or/f false? (is-a?/c target%))
      ;;                             (is-a?/c suite%)))
      (define exceptions
        (map (lambda (x)
               (list (car x) (if (cadr x) (send (cadr x) to-target) #f) (caddr x)))
             excepts))

      (define/override (set-bindings! enclosing-scope)
        (send body set-bindings! enclosing-scope)
        (for-each (lambda (x)
                    (when (car x) (send (car x) set-bindings! enclosing-scope))
                    (when (cadr x) (send (cadr x) set-bindings! enclosing-scope))
                    (send (caddr x) set-bindings! enclosing-scope))
                  exceptions)
        (when else (send else set-bindings! enclosing-scope))
        (when finally (send finally set-bindings! enclosing-scope)))
        
      (define/override (check-break/cont enclosing-loop)
        (void (sub-stmt-map (lambda (s) (send s check-break/cont enclosing-loop)))))
      
      (define/override (collect-globals)
        (apply append (sub-stmt-map (lambda (s) (send s collect-globals)))))

      (define/override (needs-escape-continuation? ec)
        
        ;;;; erm... sub-stmt-ormap? :P
        (let/ec k
          (sub-stmt-map (lambda (s)
                          (when (send s needs-escape-continuation? ec)
                            (k #t))))
          #f))
      
      (define/override (to-description)
        `(try-except% ,body ,exceptions ,else ,finally))
      
      
      (inherit ->orig-so)
      (define/override (to-racket)
        (let ([try-so
               (list `(try ,(send body to-racket)))]
              
              [except-so 
               (map (lambda (handler)
                      `(except (,(if (first handler)
                                     (send (first handler) to-racket)
                                     'py-object)
                                ,(if (second handler)
                                     (send (second handler) to-racket)
                                     (gensym 'exn)))
                               ,(send (third handler) to-racket)))
                    exceptions)]
              
              [else-so
               (if else
                   (list `(else ,(send else to-racket)))
                   empty)]
              
              [finally-so
               (if finally
                   (list `(finally ,(send finally to-racket)))
                   empty)])
          (->orig-so `(try-except ,@(append try-so except-so else-so finally-so)))))
                          
      
      (super-new [substatements (sub-stmt-map identity)])
      (let loop ((e exceptions))
        (cond
          ((null? e) void)
          ((not (null? (cdr e)))
           (when (not (caar e))
             (stx-err "default except clause must be last"))
           (loop (cdr e)))
          (else (loop (cdr e)))))))
  
  
  ;;  use this as the object type of the initial empty environment for a module
  (define module-scope%
   (class
    (bindings-mixin
     (binding-scope-base/default-mixin
     (class object%
       
       (define/public (get-escape-continuation-symbol) (gensym 'unusable))
       
       (super-new))))
     
     (define/override (is-bound? id) #f)
     
     (super-new)))

  
  ;; 7.5
  (define function-definition%
    (bindings-mixin
     (class* compound-statement% (binding-scope-base<%>)
       (inherit stx-err)
       
       ;; name: (is-a?/c identifier%)
       ;; parms: (is-?/c parameters%)
       ;; body: (is-a?/c suite%)
       (init-field name parms body)
       
       (define/public (get-body-statement) body)
       
       ;; -- moved (send name to-target) here to make set-bindings! work correctly
       (define tname (send name to-target))

       (define return-symbol (gensym 'return))
      
       (define/public (get-return-symbol) return-symbol)
       (define/public (get-escape-continuation-symbol) return-symbol)
       
       (define scope #f)

       (define/override (set-bindings! es)
         (set! scope es)
         (when es
           (unless (send es is-local? tname)
             (send es add-binding tname)))
         (send parms set-bindings! es)
         (send body set-bindings! this))
      
       (define (top?)
         (or (is-a? scope module-scope%)
             (is-a? scope class-definition%)))
       
       (define/override (check-break/cont enclosing-loop)
         (send body check-break/cont #f))
       
       ;; inner functions can't turn outer fns into generators
       (define/override (yields?) #f)
       
       (define/public (get-parms) parms)
       
       (send body set-tail-position!)
       
       (define/override (to-description)
         `(function-definition% ,name
                                ,parms
                                (bindings: ,(send this get-bindings))
                                (globals: ,(send this get-global-table))
                                ,body))
       
       
       (inherit ->orig-so ->lex-so)
       (inherit-field substatements)
       
       (define/override (to-racket)
         (let* 
             ([bindings (map (lambda (binding)
                                    `[,(send binding to-racket) undefined])
                                  (filter (lambda (id)
                                            (not (memq (send id get-symbol) (send parms get-names))))
                                          (send this get-bindings)))]
              
              [body-so (send body to-racket return-symbol #t #t)]
              
              [body+bindings-so (if (not (empty? bindings))
                                    `(let ,bindings ,body-so)
                                    body-so)]
              
              [body+bindings-so (if (send body yields?)
                                    `(make-py-generator (generator () ,body+bindings-so))
                                    body+bindings-so)])
                             
           (->orig-so `(define-py-function ,(send name to-racket) with-params ,(send parms get-names)
                         (lambda ,(send parms to-racket) ,body+bindings-so)))))
       
       (super-new [substatements (list body)]))))
    
  ;; 7.6
  (define class-definition%
    (bindings-mixin
     (class* statement% (binding-scope-base<%>)
       ;; name: (is-a?/c identifier%)
       ;; inherit-expr: (listof (is-a?/c expression%))
       ;; body: (is-a?/c suite%)
       (init-field name inherit-expr body)
       
       ;; -- moved send to-target here
       (define tname (send name to-target))

       (define/public (get-body-statement) body)       
       
       (define/override (set-bindings! enclosing-scope)
         (when (and enclosing-scope
                    (not (send enclosing-scope is-local? tname)))
           (send enclosing-scope add-binding tname))
         (for-each (lambda (x) (send x set-bindings! enclosing-scope)) inherit-expr)
         (send body set-bindings! this))
       
       (define/override (check-break/cont enclosing-loop)
         (send body check-break/cont #f))
       
       (define/override (to-description)
         `(class-definition% ,name
                             ,inherit-expr
                             (bindings: ,@(send this get-bindings))
                             (globals: ,(send this get-global-table))
                             ,body))
       
       
       (inherit ->orig-so ->lex-so)
       (define/override (to-racket)
         (->orig-so `(py-class ,(send name to-racket)
                               ,(map send-to-racket inherit-expr)
                               ,(reverse (map (lambda (x) (send x to-racket))
                                              (send this get-bindings)))
                       ,@(map send-to-racket (send body get-statements)))))
       
;         (let ([class-name (send name to-racket)]
;               [inherit-list (map (lambda (i) (send i to-racket)) inherit-expr)])
;         (->orig-so `(begin (provide ,class-name)
;                            (define ,class-name
;                       (,(py-so 'py-apply) ,(py-so 'cpy-type)
;                           (,(py-so 'make-py-symbol) ',class-name)
;                           (,(py-so 'vector->pytuple) ,(generate-vector (if (empty? inherit-list)
;                                                                          (list (py-so 'cpy-object))
;                                                                          inherit-list)))
;                           (,(py-so 'make-py-dict) (let ()
;                                           ,(foldr (lambda (next acc)
;                                                     (if (or (is-a? next assignment%)
;                                                             (is-a? next function-definition%)
;                                                             (is-a? next class-definition%))
;                                                         `(let ()
;                                                            ,(send next to-racket)
;                                                            ,acc)
;                                                         `(begin ,(send next to-racket) ,acc)))
;                                                    `(list ,@(map (lambda (b)
;                                                        (let ([id (send b to-racket)])
;                                                          `(cons (,(py-so 'make-py-symbol) ',id) ,id)))
;                                                      (send this get-bindings)))
;                                                     (send body get-statements))
;                                                     ))
;                           ))))))
                           
       
       (super-instantiate ()))))
)
