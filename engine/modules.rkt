(module modules racket
  (provide (all-defined-out))
  (require "engine.rkt"
           "../name-mangling.rkt")
  
  
  (struct module_obj instance_obj
    (name)
    #:transparent)
  
  (define (py-module-set-entry! mod key value)
    (dict-set! (instance_obj-attributes mod) key value))
  
  (define (py-module-repr mod)
    (format "<module '~a'>" (module_obj-name mod)))
  
  (define py-module
    (make-type "module"
               (vector py-object)
               (hasheq '__delattr__ #f
                     '__dict__ (make-getset instance_obj-attributes)
                     '__doc__ #f
                     '__getattribute__ py-instance-getattribute
                     '__init__ #f
                     '__new__ #f
                     '__repr__ py-module-repr
                     '__setattr__ py-instance-setattribute)))
  
  (define (make-py-module name [contents (make-hash)])
    (module_obj py-module contents name))
  
  
  ;; side effect - initializes module
  (define (py-module-from-module module-spec #:mangled? [mangled? #f])
    (let* ([name (extract-module-name module-spec)]
           [module-obj (make-py-module name)])
      (dynamic-require module-spec #f)
      (for ([id (collect-exported/no-macros module-spec)])
        (let ([name (colon-symbol->string id)])
          (unless (and (eq? (string-ref name 0) #\_)
                       (not mangled?))
            (py-module-set-entry! module-obj
                                  (if mangled? (mangle-name name) name)
                                  (dynamic-require module-spec id)))))
      module-obj))
  
  
  (require racket/path)
  
  (define (extract-module-name module-spec)
    (match module-spec
      [(? symbol? sym) (symbol->string sym)]
      [(list 'file path) (second (regexp-match "^([^.]+)(\\..+)?$"
                                               (path->string (file-name-from-path path))))]
      [(list 'planet (? symbol? sym)) (symbol->string sym)]
      [(list 'planet (? string? str)) str]
      [(list 'lib strs ...) (first strs)]
      [_ "anonymous"]))
            
    
  
  )