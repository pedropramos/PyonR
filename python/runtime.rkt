(module runtime racket
  (require (for-syntax racket/syntax))
  (require racket/undefined
           "engine/engine.rkt"
           "engine/bools.rkt"
           "engine/collections.rkt"
           "engine/descriptors.rkt"
           "engine/exceptions.rkt"
           "engine/files.rkt"
           "engine/functions.rkt"
           "engine/iterators.rkt"
           "engine/modules.rkt"
           "engine/numbers.rkt"
           "engine/racket_values.rkt"
           "engine/singletons.rkt"
           "engine/strings.rkt")
  (provide (all-defined-out)
           (all-from-out "engine/engine.rkt"
                         "engine/bools.rkt"
                         "engine/collections.rkt"
                         "engine/descriptors.rkt"
                         "engine/exceptions.rkt"
                         "engine/iterators.rkt"
                         "engine/files.rkt"
                         "engine/functions.rkt"
                         "engine/modules.rkt"
                         "engine/numbers.rkt"
                         "engine/racket_values.rkt"
                         "engine/singletons.rkt"
                         "engine/strings.rkt"))
  
  
  ;; parameter used to access current namespace from the eval() function
  (define namespace-for-eval (make-parameter #f))
 
  
  
  (define-syntax (define-py-function stx)
    (syntax-case stx (with-params)
      [(_ name with-params (param ...) lambda-form)
       #'(py-assign! name
           (function_obj py-function
                         lambda-form
                         (list (quote param) ...)))]))
  
  (define-syntax (make-py-lambda stx)
    (syntax-case stx (with-params)
      [(_ with-params (param ...) lambda-form)
       #'(function_obj py-function
                       lambda-form
                       (list (quote param) ...))]))
  
  (require (for-syntax (only-in "name-mangling.rkt" colon-symbol->string)))
  
  (define-syntax (py-class stx)
    (syntax-case stx ()
      [(_ name (base ...) (binding ...) body0 body ...)
       (let* ([name/str (colon-symbol->string (syntax->datum #'name))]
              [bindings/ident (map syntax-e (syntax->list #'(binding ...)))]
              [bindings/sym (map (compose (lambda (x) `(quote ,x))
                                          string->symbol 
                                          colon-symbol->string)
                                 bindings/ident)]
              [bindings/sym+ident (apply append (for/list ([sym (in-list bindings/sym)]
                                                           [ident (in-list bindings/ident)])
                                                  (list sym ident)))])
         (with-syntax ([name-str (datum->syntax stx name/str)]
                       [attrs+bindings (datum->syntax stx bindings/sym+ident)])
           #`(py-assign! name
               (let ([binding 'undefined] ...)
                 body0 body ...
                 (make-class name-str
                             (make-py-tuple base ...)
                             (hasheq #,@#`attrs+bindings))))))]))
  
  
  
  (define-syntax-rule (py-call func ...)
    (func ...))
  
  
  (define (index-of lst ele)
  (let loop ((lst lst)
             (idx 0))
    (cond ((empty? lst) #f)
          ((eq? (first lst) ele) idx)
          (else (loop (rest lst) (add1 idx))))))
  
  (define-syntax-rule (py-call/keywords func (arg ...) ((kw value) ...))
    (apply/keywords func
                    (list arg ...)
                    (list (cons (quote kw) value) ...)))
                       
                       
  (define (apply/keywords func pos-args kw-args)
    ; pos-args - list of values
    ; kw-args - list of pairs of ('kw . value)
    (let* ([arg-number (length (function_obj-parameters func))]
           [args (make-vector arg-number null)])
      ; set positional arguments
      (for ([n (in-range (length pos-args))])
        (vector-set! args n (list-ref pos-args n)))
      ; set keyword arguments
      (for ([arg-pair (in-list kw-args)])
        (vector-set! args
                     (index-of (function_obj-parameters func) (car arg-pair))
                     (cdr arg-pair)))
      ; apply func
      (apply (function_obj-proc func) (filter (compose not null?)
                                              (vector->list args)))))
  
  
  ;;;;;;;;;;;;;;;;;;;;;
   
   
   
  (define (binary-op-method-call x method-name reversed-name y)
    (let ([candidate-result (py-method-call x method-name y)])
      (if (eq? candidate-result py-NotImplemented)
          (py-method-call y reversed-name x)
          candidate-result)))
  
  (define (augmented-op-method-call x method-name incremented-name y)
    (let ([i-meth (mro-lookup x incremented-name)])
      (if i-meth
          (i-meth x y)
          (binary-op-method-call x method-name y))))
  
  
  
  
  (define-for-syntax (prefix-r name)
    (let ([name/str (symbol->string name)])
      (string->symbol
       (string-append (substring name/str 0 2) "r" (substring name/str 2)))))
  
  (define-for-syntax (prefix-i name)
    (let ([name/str (symbol->string name)])
      (string->symbol
       (string-append (substring name/str 0 2) "i" (substring name/str 2)))))
     
  
  
  
  (define-syntax (binary-op stx)
    (syntax-case stx ()
      [(_ method-name)
       (with-syntax ([reversed-name (datum->syntax stx (prefix-r (syntax->datum #'method-name)))])
         #'(lambda (x y)
             (binary-op-method-call x (quote method-name) (quote reversed-name) y)))]))
  
  (define-syntax (binary-op/numbers stx)
    (syntax-case stx ()
      [(_ method-name #:num num-dispatch-op)
       (with-syntax ([reversed-name (datum->syntax stx (prefix-r (syntax->datum #'method-name)))])
         #'(lambda (x y)
             (cond
               [(and (number? x) (number? y)) (num-dispatch-op x y)]
               [else (binary-op-method-call x (quote method-name) (quote reversed-name) y)])))]))
  
  (define-syntax (binary-op/numbers+strings stx)
    (syntax-case stx ()
      [(_ method-name #:num num-dispatch-op
                      #:str str-dispatch-op)
       (with-syntax ([reversed-name (datum->syntax stx (prefix-r (syntax->datum #'method-name)))])
         #'(lambda (x y)
             (cond
               [(and (number? x) (number? y)) (num-dispatch-op x y)]
               [(and (string? x) (string? y)) (str-dispatch-op x y)]
               [else (binary-op-method-call x (quote method-name) (quote reversed-name) y)])))]))
  
  
  
  (define (py-number-modulo p q)
    (- p (* q (truncate (/ p q)))))
  
  (define (py-shift-left n b)
    (arithmetic-shift n b))
  
  (define (py-shift-right n b)
    (arithmetic-shift n (- b)))
  
  (define py-add (binary-op/numbers+strings __add__ #:num + #:str string-append))
  (define py-sub (binary-op/numbers __sub__ #:num -))
  (define py-mul (binary-op/numbers __mul__ #:num *))
  (define py-div (binary-op/numbers __div__ #:num number-division))
  (define py-floordiv (binary-op/numbers __floordiv__ #:num (compose floor /)))
  (define py-mod (binary-op/numbers __mod__ #:num py-number-modulo))
  (define py-pow (binary-op/numbers __pow__ #:num expt))                             ;; /!\ optional argument: modulo
  (define py-lshift (binary-op/numbers __lshift__ #:num py-shift-left))
  (define py-rshift (binary-op/numbers __rshift__ #:num py-shift-right))
  (define py-bwand (binary-op/numbers __and__ #:num bitwise-and))
  (define py-bwor (binary-op/numbers __or__ #:num bitwise-ior))
  (define py-bwxor (binary-op/numbers __xor__ #:num bitwise-xor))
  
  ;;;;;;;;;;;;;;;
  
  (define-syntax (augmented-op stx)
    (syntax-case stx ()
      [(_ method-name)
       (with-syntax ([incremented-name (datum->syntax stx (prefix-i (syntax->datum #'method-name)))])
         #'(lambda (x y)
             (augmented-op-method-call x (quote method-name) (quote incremented-name) y)))]))
  
  (define-syntax (augmented-op/numbers stx)
    (syntax-case stx ()
      [(_ method-name #:num num-dispatch-op)
       (with-syntax ([incremented-name (datum->syntax stx (prefix-i (syntax->datum #'method-name)))])
         #'(lambda (x y)
             (cond
               [(and (number? x) (number? y)) (num-dispatch-op x y)]
               [else (augmented-op-method-call x (quote method-name) (quote incremented-name) y)])))]))
  
  (define-syntax (augmented-op/numbers+strings stx)
    (syntax-case stx ()
      [(_ method-name #:num num-dispatch-op
                      #:str str-dispatch-op)
       (with-syntax ([incremented-name (datum->syntax stx (prefix-i (syntax->datum #'method-name)))])
         #'(lambda (x y)
             (cond
               [(and (number? x) (number? y)) (num-dispatch-op x y)]
               [(and (string? x) (string? y)) (str-dispatch-op x y)]
               [else (augmented-op-method-call x (quote method-name) (quote incremented-name) y)])))]))
  
  
  (define py-iadd (augmented-op/numbers+strings __add__ #:num + #:str string-append))
  (define py-isub (augmented-op/numbers __sub__ #:num -))
  (define py-imul (augmented-op/numbers __mul__ #:num *))
  (define py-idiv (augmented-op/numbers __div__ #:num number-division))
  (define py-ifloordiv (augmented-op/numbers __floordiv__ #:num (compose floor /)))
  (define py-imod (augmented-op/numbers __mod__ #:num py-number-modulo))
  (define py-ipow (augmented-op/numbers __pow__ #:num expt))                             ;; /!\ optional argument: modulo
  (define py-ilshift (augmented-op/numbers __lshift__ #:num py-shift-left))
  (define py-irshift (augmented-op/numbers __rshift__ #:num py-shift-right))
  (define py-ibwand (augmented-op/numbers __and__ #:num bitwise-and))
  (define py-ibwor (augmented-op/numbers __or__ #:num bitwise-ior))
  (define py-ibwxor (augmented-op/numbers __xor__ #:num bitwise-xor))
  
  
  
  (define-syntax (unary-op stx)
    (syntax-case stx ()
      [(_ method-name)
       #'(lambda (x)
           (py-method-call x method-name))]))
  
  (define-syntax (unary-op/numbers stx)
    (syntax-case stx ()
      [(_ method-name #:num num-dispatch-op)
       #'(lambda (x)
           (cond
             [(number? x) (num-dispatch-op x)]
             [else (py-method-call x method-name)]))]))
  
  (define py-unary-plus (unary-op/numbers '__pos__ #:num +))
  (define py-unary-minus (unary-op/numbers '__neg__ #:num -))
  (define py-bwinvert (unary-op '__invert__))
  
  

  
  (define (:abs x)
    (py-method-call x '__abs__))
  
  
  
  
  ;; safe-provide doesn't result in error when used on REPL
  (define-syntax (safe-provide stx)
    (syntax-case stx ()
      [(safe-prov spec other-specs ...)
       (if (syntax-source-module #'safe-prov)
           #'(provide spec other-specs ...)
           #'(void))]))
  
  (define-syntax (expr-stmt stx)
    (syntax-case stx ()
      [(_ expr)
       (if (syntax-source-module stx)
           #'(void expr)
           #'expr)]))
  
  
  (require (for-syntax racket/list))
  
  (define-for-syntax (collect-unbound-ids stx)
    (cond
      [(and (identifier? stx)
            (not (identifier-binding stx 0)))
       (list stx)]
      [(syntax->list stx)
       (remove-duplicates
        (apply append (map collect-unbound-ids (syntax->list stx)))
        free-identifier=?)]
      [else (list)]))
  
  (define-for-syntax (makes-to-sequences stx)
    (cond
      [(and (identifier? stx)
            (or (eq? (syntax->datum stx) 'make-py-tuple)
                (eq? (syntax->datum stx) 'make-py-list)))
       (datum->syntax stx 'sequence)]
      [(syntax->list stx)
       (datum->syntax stx (map makes-to-sequences (syntax->list stx)))]
      [else stx]))
  
  
  (define-syntax (py-assign! stx)
    (syntax-case stx (make-py-tuple make-py-list sequence 
                      py-get-attr py-get-index py-get-slice)
      
      [(_ (make-py-tuple var ...) expr)
       (with-syntax*
           ([ids (collect-unbound-ids #'(make-py-tuple var ...))]
            [defines (for/list ([id (syntax->list #'ids)])
                       #`(define #,id undefined))]
            [seqs (makes-to-sequences #'(make-py-tuple var ...))]
            [body #`(py-assign! seqs expr)])
         #`(begin #,@#'defines body))]
      
      [(_ (make-py-list var ...) expr)
       (with-syntax*
           ([ids (collect-unbound-ids #'(make-py-tuple var ...))]
            [defines (for/list ([id (syntax->list #'ids)])
                       #`(define #,id undefined))]
            [seqs (makes-to-sequences #'(make-py-tuple var ...))]
            [body #`(py-assign! seqs expr)])
         #`(begin #,@#'defines body))]
      
      [(_ (sequence var ...) expr)
       #'(let* ([it (iter expr)]
                [next (mro-lookup it 'next)])
           (py-assign! var (next it))
           ...)]
      
      [(_ (py-get-attr var attr) expr)
        #'(py-set-attr! var attr expr)]
      [(_ (py-get-index var index) expr)
        #'(py-set-index! var index expr)]
      [(_ (py-get-slice var lower upper) expr)
        #'(py-set-slice! var lower upper expr)]
      
      [(_ var expr)
       (if (identifier-binding #'var 0)
           #'(set! var expr)
           #'(define var expr))]))
  
  (define-syntax (py-multiple-assign! stx)
    (syntax-case stx ()
      [(_ (var ...) expr)
       (with-syntax*
           ([ids (collect-unbound-ids #'(var ...))]
            [defines (for/list ([id (syntax->list #'ids)])
                       #`(define #,id undefined))])
         #`(begin
             #,@#'defines
             (let ([rhs expr])
               (py-assign! var rhs)
               ...)))]))
  
  
  
  
  (define (py-get-attr obj attribute)
    (py-method-call obj '__getattribute__ attribute))
  
  (define (py-set-attr! obj attribute item)
    (py-method-call obj '__setattr__ attribute item))

  
  
  
  (define (py-truth x)
    (cond
      [(boolean? x) x]
      [(number? x) (not (zero? x))]
      [(eq? x py-None) #f]
      [else
       (let* ([nonzero (mro-lookup x '__nonzero__)]
              [len (and (not nonzero)
                        (mro-lookup x '__len__))])
         (cond
           [nonzero (let ([res (nonzero x)])
                      (not (or (not res)
                               (= res 0))))]
           [len (let ([res (len x)])
                  (not (or (not res)
                           (= res 0))))]
           [else #t]))]))

  
  (define-syntax-rule (py-or x y)
    (if (py-truth x) x y))
  
  (define-syntax-rule (py-and x y)
    (if (not (py-truth x)) x y))
  
  (define (py-not x)
    (not (py-truth x)))
  
  
  (define (rich-comparison rich-meth-name #:op op)
    (lambda (x y)
      (if (and (number? x) (number? y))
          (op x y)
          (let ([rich-meth (mro-lookup x rich-meth-name)])
            (if rich-meth
                (rich-meth x y)
                (let ([cmp-meth (mro-lookup x '__cmp__)])
                  (if cmp-meth
                      (op (cmp-meth x y) 0)
                      (op (id x) (id y)))))))))
  
  
  (define py-ne (rich-comparison '__ne__ #:op (compose not =)))
  (define py-gt (rich-comparison '__gt__ #:op >))
  (define py-lt (rich-comparison '__lt__ #:op <))
  (define py-ge (rich-comparison '__ge__ #:op >=))
  (define py-le (rich-comparison '__le__ #:op <=))
  
  (define (py-in obj seq)
    (let ([contains (mro-lookup seq '__contains__)])
      (cond
        [contains (contains seq obj)]
        [else (let* ([it (iter seq)]
                     [it-next (mro-find it 'next)])
                (with-handlers ([stop-iteration-predicate (const #f)])
                  (let loop ()
                    (or (py-eq obj (it-next it))
                        (loop)))))])))
  
  (define (py-notin x seq)
    (not (py-in x seq)))
  
  (define (py-is x y)
    (= (id x) (id y)))
  
  (define (py-isnot x y)
    (not (py-is x y)))
  
  
  
  (define-syntax (py-for stx)
    (syntax-case stx ()
      [(_ (target sequence) body ...)
       #'(for ([item (->sequence sequence)])
             (py-assign! target item)
             body ...)]))
         
  (define-syntax (py-for/continue stx)
    (syntax-case stx ()
      [(_ (target sequence) body ...)
       (with-syntax ([continue-name (datum->syntax stx 'continue)])
         #'(let-values ([(more-values? next) (sequence-generate (->sequence sequence))])
             (let continue-name ()
               (if (more-values?)
                   (begin (py-assign! target (next))
                          body ...
                          (continue-name))
                   py-None))))]))
                        
             
         
;         #'(let* ([it (iter sequence)]
;                  [next (mro-lookup it 'next)])
;             (with-handlers ([stop-iteration-predicate void])
;               (let continue-name ()
;                 (py-assign! target (next it))
;                 body ...
;                 (continue-name)))))]))
  
  
 

  
  ;;;;;;;;;;;;;;;
  ;; EXCEPTIONS
  ;;;;;;;;;;;;;;;
  
  (define current-exception
    (make-parameter
     (make-py-exception py-TypeError "no exception in scope")))
  
  (define always-true (lambda (x) #t))
  
  (define-syntax (try-except stx)
    ;; needs fix on returns on finally clause (maybe at compilation level?)
    
    (syntax-case stx (try except else finally)
      
      ;;; try...except...
      [(_ (try try-body ...)
          (except (exception-type exn-id) except-body ...)
          ...)
       #'(with-handlers
             ([(type-predicate exception-type)
               (lambda (exn-id)
                 (parameterize ([current-exception exn-id])
                   except-body ...))]
              ...)
           try-body ...)]
      
      ;;; try...except...else...
      [(_ (try try-body ...)
          (except (exception-type exn-id) except-body ...)
          ...
          (else else-body ...))
       #'(let ([execute-else? #t])
           (with-handlers
               ([(type-predicate exception-type)
                 (lambda (exn-id)
                   (parameterize ([current-exception exn-id])
                     (set! execute-else? #f)
                     except-body ...))]
                ...)
             try-body ...
             (when execute-else?
               else-body ...)))]
      
      ;;; try...except...else...finally...
      [(_ (try try-body ...)
          (except (exception-type exn-id) except-body ...)
          ...
          (else else-body ...)
          (finally finally-body ...))
       #'(let ([execute-else? #t]
               [finally-thunk (lambda () finally-body ...)])
           (with-handlers
               ([(type-predicate exception-type)
                 (lambda (exn-id)
                   (parameterize ([current-exception exn-id])
                     (set! execute-else? #f)
                     except-body ...))]
                ...
                [always-true (lambda (exn)
                               (finally-thunk)
                               (raise exn))])
             try-body ...
             (when execute-else?
               else-body ...))
           (finally-thunk))]
      
      ;;; try...except...finally...
      [(_ (try try-body ...)
          (except (exception-type exn-id) except-body ...)
          ...
          (finally finally-body ...))
       #'(let ([finally-thunk (lambda () finally-body ...)])
           (with-handlers
               ([(type-predicate exception-type)
               (lambda (exn-id)
                 (parameterize ([current-exception exn-id])
                   except-body ...))]
                ...
                [always-true (lambda (exn)
                               (finally-thunk)
                               (raise exn))])
             try-body ...)
           (finally-thunk))]))
  
  
    (define (py-raise obj)
    (if (type_obj? obj)
        (raise (make-py-exception obj))
        (raise obj)))
  
  (define (py-raise/2 typ parm)
    (if (is-subtype? (type parm) typ)
        (raise parm)
        (raise (typ parm))))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-syntax (py-assert stx)
    (syntax-case stx ()
      [(_ expr)
       #'(py-assert expr "")]
      [(_ expr msg)
       #'(when (py-not expr)
           (exn-raise py-AssertionError msg))]))
  
  (define (py-display obj)
    (display (if (void? obj) "" (repr obj))))
  
  (define (py-print #:port [port (current-output-port)] . objs)
    (for ([obj (in-list objs)])
      (let ([msg (print-repr obj)])
        (display msg port)
        (display #\space port)))
    (newline port))
  
  
  ;;;;;;;;;;;;;;;
  ;; IMPORTS
  ;;;;;;;;;;;;;;;
  
  (require racket/require
           racket/provide)
  
  (require "name-mangling.rkt"
           (for-syntax "name-mangling.rkt"))
  (provide (all-from-out "name-mangling.rkt"))
  
  
  (require "paths.rkt")
  (require (for-syntax "paths.rkt"))
  
  (define PATH
    (let* ([file (open-input-file path-to-PATH)])
      (list->py-list
       (append (list "." path-to-lib)
               (map string-trim
                    (port->list read-line file))))))
  
  
  (define (module-name->module-path name)
    (let loop ([dirs (py-list->list PATH)])
      (if (empty? dirs)
          (exn-raise py-ImportError "No module named ~a" name)
          (let* ([dir (first dirs)]
                 [file-path (build-path dir (string-append name ".py"))])
            (if (file-exists? file-path)
                (path->string file-path)
                (loop (rest dirs)))))))
  
  
  ;; similar to above but reads directly from PATH file
  (require (for-syntax racket/port
                       racket/string))
  (define-for-syntax (module-name->module-path/compile-time name)
    (let* ([file (open-input-file path-to-PATH)])
      (let loop ([dirs (append (list "." path-to-lib)
                               (map string-trim (port->list read-line file)))])
        (if (empty? dirs)
          (raise-syntax-error 'ImportError (format "No module named ~a" name))
          (let* ([dir (first dirs)]
                 [file-path (build-path dir (string-append name ".py"))])
            (if (file-exists? file-path)
                (path->string file-path)
                (loop (rest dirs))))))))
  
  
  
  ;; for #lang python modules
  (define-syntax (py-import stx)
    (syntax-case stx (as)
      [(_ module-name as id)
       #'(define id (let ([module-spec (list 'file (module-name->module-path module-name))])
                      (parameterize ([name-of-module-parameter (extract-module-name module-spec)])
                        (py-module-from-module module-spec))))]))
  
  ;; for #lang racket modules
  (define-syntax (py-import-mangle stx)
    (syntax-case stx (as)
      [(_ module-spec as id)
       #'(define id (py-module-from-module (quote module-spec) #:mangled? #t))]))
  
  
  ;; for #lang python modules
  (define-syntax (py-from stx)
    (syntax-case stx (import as)
      [(_ module-name import ((orig-id as bind-id) ...))
       #'(begin
           (define bind-id (let ([module-spec (list 'file (module-name->module-path module-name))])
                             (parameterize ([name-of-module-parameter (extract-module-name module-spec)])
                               (dynamic-require module-spec (quote orig-id)))))
           ...)]))
  
  ;; for #lang racket modules
  (define-syntax (py-from-mangle stx)
    (syntax-case stx (import as)
      [(_ module-spec import ((orig-id as bind-id) ...))
       (with-syntax ([safe-provide (datum->syntax stx 'safe-provide)])
         #'(begin
             (require (only-in (filtered-in mangle-name module-spec)
                               [orig-id bind-id] ...))
             (safe-provide bind-id ...)))]))
  
  
  ;; for #lang python modules
  (define-syntax (py-from-import-* stx)
    (syntax-case stx ()
      [(_ module-name)
       
       (let ([module-spec (list 'file (module-name->module-path/compile-time (syntax->datum #'module-name)))])
         (namespace-require/expansion-time module-spec)
         (let* ([ids (collect-exported module-spec)]
                [ids/stx (datum->syntax stx ids)])
           (syntax-case ids/stx ()
             [(id ...)
              (with-syntax ([module-spec/stx (datum->syntax stx module-spec)])
                #`(begin
                    (parameterize ([name-of-module-parameter (extract-module-name (quote module-spec/stx))])
                      (dynamic-require (quote module-spec/stx) #f))
                    (begin
                      (define id (dynamic-require (quote module-spec/stx) (quote id)))
                      ...)))])))]))
           
  
  ;; for #lang racket modules
  (define-syntax (py-from-import-*-mangle stx)
    (syntax-case stx ()
      [(_ module-spec)
       (with-syntax ([safe-provide (datum->syntax stx 'safe-provide)])
         #'(begin
             (require (prefix-in : (filtered-in mangle-name module-spec)))
             (safe-provide (all-from-out module-spec))))]))
  
  
  ;;;;;;;;;;;;;;;;;
  ;; EVAL + EXEC
  ;;;;;;;;;;;;;;;;;
  
  (require "parse.rkt"
           "compile.rkt"
           "ast-node-stmt.rkt")

  (define (build-namespace glo loc)
    (dynamic-require 'python #f)
    (let ([ns (module->namespace 'python)])
      (for ([ht (in-list (list glo loc))])
        (when (not (void? ht))
          (for ([(name value) (in-dict ht)])
            (namespace-set-variable-value! (string->symbol (string-append ":" name)) value #f ns))))
      ns))
  
  (define (py-eval str [globals py-None] [locals py-None])
    (let* ([ns (if (and (void? globals) (void? locals))
                   (namespace-for-eval)
                   (build-namespace globals locals))]
           [ast (read-python-port (open-input-string str) "eval")]
           [expr-stmt (if (or (not (= (length ast) 1))
                              (not (is-a? (first ast) expr-stmt%)))
                          (exn-raise py-SyntaxError "invalid syntax: not an expression")
                          (first ast))]
           [expr (get-field expression expr-stmt)]
           [compiled (compile-python (list expr))]
           [sexp (syntax->datum (first compiled))])
      (eval sexp ns)))
  
  (define (py-exec str globals locals)
    (let* ([ns (if (and (void? globals) (void? locals))
                   (namespace-for-eval)
                   (build-namespace globals locals))]
           [ast (read-python-port (open-input-string str) "exec")]
           [compiled (compile-python ast)]
           [sexps (map syntax->datum compiled)]
           [stmt `(begin ,@sexps)])
      (eval stmt ns)))
  
  )