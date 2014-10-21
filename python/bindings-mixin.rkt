(module bindings-mixin racket/base
  (require racket/class
           racket/contract
           "ast-node-target.rkt") ;; for tidentifier=?

  (define binding-scope-base<%>
    (interface ()
      get-body-statement))
  
  (define binding-scope<%>
    (interface (binding-scope-base<%>)
      get-global-table
      get-bindings
      add-binding
      is-global?
      is-local?
      is-bound?
      binding-tid))
  
  (define (binding-scope-base-implementation? %)
    (implementation? % binding-scope-base<%>))
  
  (define (binding-scope-implementation? %)
    (implementation? % binding-scope<%>))
  
  (define (binding-scope-base/default-mixin %)
    (class* % (binding-scope-base<%>)
      
      (define fake-body (make-object (class object%
                                       (define/public (collect-globals) null)
                                       (super-instantiate ()))))
      
      (define/public (get-body-statement) fake-body)
      
      (super-new)))

  
  (define (bindings-mixin %)
    ;(unless (implementation? % binding-scope<%>)
    ;  (error 'bindings-mixin
    ;         (format "Expected a binding-scope<%> superclass, got: ~a" %)))
    (class* % (binding-scope<%>)
      (super-new)
      (inherit get-body-statement)
      (define global-table
        (let ((ht (make-hash)))
          (for-each
           (lambda (g) 
             (hash-set! ht (send g get-symbol) g))
           (send (get-body-statement) collect-globals))
          ht))
      
      
      ;;  looks like global-table is a hash-table of key: symbol, value: identifier%
      
      ;; bindings: (listof (is-a?/c tidentifier%))
      (define bindings null)
      
      
      (define/public (get-global-table) global-table)
      (define/public (get-bindings) bindings)
      
      (define/public (add-binding id)
        (unless (hash-ref global-table (send id get-symbol) (lambda () #f))
          (set! bindings (cons id bindings))))
      
;      (define/public (is-global? id)
;        (cond
;          ((hash-table-get global-table (send id get-symbol) (lambda () #f)) #t)
;          (else #f)))
      
      ;;  modified to return the tidentifier% in case it's in the global table
      (define/public (is-global? id)
        (hash-ref global-table (send id get-symbol) (lambda () #f)))
      
      
      (define/public (is-local? id)
        (ormap (lambda (b)
                 (and (tidentifier=? id b) b))
               bindings))
      
      
      (define/public (is-bound? id)
        (or (is-global? id) (is-local? id)))
      
      
      ;; binding-tid: (union identifier% tidentifier%) -> (union identifier% tidentifier% false)
      ;; which tidentifier% or identifier% is the first-seen (binding) instance of this id?
      (define/public (binding-tid id)
        (is-bound? id))
      
      ;(super-new)
      
      ))

  (provide/contract [bindings-mixin (binding-scope-base-implementation? . -> . binding-scope-implementation?)]
                    [binding-scope-base/default-mixin (any/c . -> . binding-scope-base-implementation?)])
  
  (provide binding-scope<%> binding-scope-base<%>)
  
  
  )