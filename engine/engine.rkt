(module engine racket
  (require ffi/unsafe)
  
  ;;; defines a Python object
  (struct python-object (type)
    #:mutable
    #:property prop:procedure
    (lambda (obj . args)
      (let ([call-meth (mro-lookup obj '__call__)])
        (if call-meth
            (apply obj call-meth args)
            (raise (make-py-exception py-TypeError
                    (format "'~a' object is not callable"
                            (type_obj-name (python-object-type obj)))))))))
  
  ;;; and a more specific object which holds instance attributes in a dictionary
  (struct instance_obj python-object
    ([attributes #:mutable]))
  
  
  ;;; defines a Python type object
  (struct type_obj python-object
    (name module bases doc dict mro  ;; general attributes
     getter setter) ;; optimization for getting/setting attributes
    #:mutable
    #:property prop:procedure
    (lambda (typ . args)
      (let* ([obj (apply (mro-type-find typ '__new__) typ args)]
             [obj-init (mro-lookup obj '__init__)])
        (when (and obj-init
                   (is-subtype? (type obj) typ))
          (apply obj-init obj args))
        obj)))
  
  ;;; defines a get/set descriptor
  (struct getset_obj python-object
    ([name #:mutable]
     [objtype #:mutable]
     getter setter doc))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; compute-mro: type_obj, (vector-of type_obj)  ->  (vector-of type_obj)
  (define (compute-mro t bases)
    (list->vector
     (cons t (mro-merge
              (append (map (compose vector->list type_obj-mro)
                           (vector->list bases))
                      (list (vector->list bases)))))))
  
  ;; mro-merge: (listof (listof type_obj)) -> (list-of type_obj)
  (define (mro-merge lists)
    (if (empty? lists)
        empty
        (for/or ([types (in-list lists)])
          (let ([head (first types)])
            (if (for/and ([other-types (in-list lists)])
                  (not (memq head (rest other-types))))
                (cons head (mro-merge (filter (lambda (x) (not (empty? x)))
                                              (map (lambda (x) (remove head x eq?))
                                                   lists))))
                #f)))))
          
  (define (is-subtype? t super)
    (vector-memq super (type_obj-mro t)))
  
  (define (type-predicate t)
    (if (vector? t)
        (type-predicate-vector t)
        (type-predicate-solo t)))
  
  (define (type-predicate-solo t)
    (lambda (x)
      (is-subtype? (type x) t)))
  
  (define (type-predicate-vector t)
    (lambda (x)
      (ormap (lambda (el) (is-subtype? (type x) el))
             (vector->list t))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (mro-find instance attribute)
    (or (mro-lookup instance attribute)
        (raise (make-py-exception py-TypeError
                (format "instance ~a has no attribute ~a"
                        instance attribute)))))
  
  (define (mro-lookup instance attribute)
    (let ([mro (type_obj-mro (type instance))])
      (for/or ([candidate (in-vector mro)])
        (hash-ref (type_obj-dict candidate) attribute #f))))
  
  (define (mro-type-find type attribute)
    (or (mro-type-lookup type attribute)
        (raise (make-py-exception py-TypeError
                (format "type ~a has no attribute ~a"
                        (type_obj-name type) attribute)))))
  
  (define (mro-type-lookup typ attribute)
    (let ([mro (type_obj-mro typ)])
      (or (for/or ([candidate (in-vector mro)])
            (hash-ref (type_obj-dict candidate) attribute #f)))))
  
  (define-syntax-rule (mro-method-call instance method . args)
    ((mro-find instance method) instance . args))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; the name of the module, e.g. __main_
  (define name-of-module #f)
  (define name-of-module-parameter (make-parameter "__main__"))
  (define (set-name-of-module!)
    (set! name-of-module (name-of-module-parameter)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (symbol-hash->string-hash symhash)
    (let ([strhash (make-hash)])
      (for ([(k v) (in-hash symhash)])
        (hash-set! strhash (symbol->string k) v))
      strhash))
  
  (define (string-hash->symbol-hash strhash)
    (let ([symhash (make-hasheq)])
      (for ([(k v) (in-dict strhash)])
        (hash-set! symhash (string->symbol k) v))
      symhash))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (empty-type)
    (type_obj #f
              #f #f #f #f #f #f
              #f #f))
  
  (define (fill-type! id name bases dict [module "__builtin__"] [doc (void)] #:mro [explicit-mro #f])
    (set-python-object-type! id py-type)
    (set-type_obj-name! id name)
    (set-type_obj-module! id module)
    (set-type_obj-bases! id bases)
    (set-type_obj-doc! id doc)
    (set-type_obj-dict! id dict)
    (set-type_obj-mro! id (or explicit-mro
                              (compute-mro id bases)))
    (finalize-type! id))
  
  ;;; easy type definition
  (define (make-type name bases dict [module "__builtin__"] [doc (void)])
    (let ([result (empty-type)])
      (fill-type! result
                  name
                  (if (zero? (vector-length bases))
                      (vector py-object)
                      bases)
                  dict
                  module
                  doc)
      result))
  
  ;;; easy class definition
  (define (make-class name bases attributes [module name-of-module] [doc (void)])
    (let ([dict (make-hasheq)])
      (hash-set! dict '__module__ module)
      (hash-set! dict '__doc__ doc)
      (hash-set! dict '__dict__ (make-getset (lambda (self)
                                               (symbol-hash->string-hash (instance_obj-attributes self)))
                                             (lambda (self value)
                                               (set-instance_obj-attributes! self (string-hash->symbol-hash value)))))
      (hash-set! dict '__new__ py-object-new-instance)
      (hash-set! dict '__getattribute__ py-instance-getattribute)
      (hash-set! dict '__setattr__ py-instance-setattribute)
      
      ;; if __slots__ was provided
      (when (hash-has-key? attributes '__slots__)
        (hash-remove! dict '__dict__)
        (hash-set! dict '__new__ py-object-new-instance/no-dict)
        (let* ([slots (hash-ref attributes '__slots__)]
               [slots (if (string? slots)
                          (list slots)
                          (->sequence slots))])
          (for ([slot slots])
            (hash-set! dict slot (make-getset (lambda (obj)
                                                 (hash-ref (slotted-instance_obj-slots obj) slot))
                                               (lambda (obj val)
                                                 (hash-set! (slotted-instance_obj-slots obj) slot val)))))))
      
      (for ([(name attr) (in-dict attributes)])
        (hash-set! dict name attr))
      
      (make-type name bases dict module doc)))
  
  
  (define (finalize-type! t)
    ;; fill out descriptors
    (for ([(name attr) (type_obj-dict t)])
      (when (getset_obj? attr)
        (set-getset_obj-name! attr name)
        (set-getset_obj-objtype! attr t)))
    ;; caches getter and setter if available
    (when (and (not (eq? t py-object))
               (not (eq? t py-type)))
      (set-type_obj-getter! t (mro-type-lookup t '__get__))
      (set-type_obj-setter! t (mro-type-lookup t '__set__))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (make-getset [getter #f] [setter #f] #:doc [doc (void)])
    (getset_obj py-getset_descriptor #f #f getter setter doc))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;; defining type and object
  ; (they make references to eachother)
  
  (define py-type (empty-type))
  (define py-object (empty-type))
  (define py-getset_descriptor (empty-type))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  ;;;; py-type-getattribute
  ; attribute exists in metatype and is a data descriptor? => return type(m_attr).get(m_attr, type, metatype)
  ; attribute exists in type     and is a descriptor?      => return type(attr).get(attr, None, type)
  ; attribute exists in type?                              => return attr
  ; attribute exists in metatype and is a descriptor?      => return type(m_attr).get(m_attr, type, metatype)
  ; attribute exists in metatype?                          => return m_attr
  (define (py-type-getattribute typ attr-name)
    (let* ([metatyp (type typ)]
           [attr-sym (string->symbol attr-name)]
           [m_attr (mro-type-lookup metatyp attr-sym)]
           [m_attr_get (and m_attr (mro-lookup m_attr '__get__))]
           [m_attr_set (and m_attr (mro-lookup m_attr '__set__))])
      (if (and m_attr m_attr_get m_attr_set)
          (m_attr_get m_attr typ metatyp)
          (let* ([attr (mro-type-lookup typ attr-sym)]
                 [attr_get (and attr (mro-lookup attr '__get__))])
            (cond
              [(and attr attr_get) (attr_get attr (void) typ)]
              [attr attr]
              [(and m_attr m_attr_get) (m_attr_get m_attr typ metatyp)]
              [m_attr m_attr]
              [else (raise (make-py-exception py-TypeError
                            (format "type ~a has no attribute ~a"
                                    (type_obj-name typ) attr-name)))])))))
  
  (define (py-type-new typ . args)
    (case (length args)
      [(1) (let ([obj (first args)]) (type obj))]
      [(3) (apply make-class args)]
      [else (raise (make-py-exception py-TypeError
                                      "type() takes 1 or 3 arguments"))]))
  
  (define (py-type-repr x)
    (format "<type '~a'>" (type_obj-name x)))
  
  (fill-type! py-type
    "type"
    (vector py-object)
    (hasheq '__abstractmethods__ #f
          '__base__ #f
          '__bases__ #f
          '__basicsize__ #f
          '__call__ #f
          '__delattr__ #f
          '__dict__ (make-getset (lambda (self)
                                   (make-dictproxy (type_obj-dict self))))
          '__dictoffset__ #f
          '__doc__ #f
          '__eq__ #f
          '__flags__ #f
          '__ge__ #f
          '__getattribute__ py-type-getattribute
          '__gt__ #f
          '__hash__ #f
          '__init__ #f
          '__instancecheck__ #f
          '__itemsize__ #f
          '__le__ #f
          '__lt__ #f
          '__module__ #f
          '__mro__ (make-getset type_obj-mro)
          '__name__ #f
          '__ne__ #f
          '__new__ py-type-new
          '__repr__ py-type-repr
          '__setattr__ #f
          '__subclasscheck__ #f
          '__subclasses__ #f
          '__weakrefoffset__ #f
          'mro #f)
    #:mro (vector py-type py-object))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (py-object-get-class obj)
    (type obj))
  
  (define (py-object-set-class! obj value)   ;; too permissive
    (cond
      [(null? value)
       (raise (make-py-exception py-TypeError
                                 "can't delete __class__ attribute"))]
      [(not (type_obj? value))
       (raise (make-py-exception py-TypeError
               (format "__class__ must be set to class, not '~a' object"
                       (type value))))]
      [(python-object? obj)
       (set-python-object-type! obj value)]
      [else
       (error "can't mutate type of " obj)]))
  
  
  
  ;;;;;;;;;;;
  
  (define (py-object-getattribute obj attr-name)
    (let ([attr (mro-lookup obj (string->symbol attr-name))])
      (if (not attr)
          ;; if attribute is not found, raise AttributeError
          (raise (make-py-exception py-AttributeError
                  (format "'~a' object has no attribute '~a'"
                          (type_obj-name (type obj)) attr-name)))
          (let ([getter (type_obj-getter (type attr))])
            (if getter
                ;; if attribute is a descriptor, use the getter
                (getter attr obj (type obj))
                ;; else return attribute
                attr)))))
      
  
  (define (py-instance-getattribute obj attr-name)
    (let* ([attr-sym (string->symbol attr-name)]
           [attr (mro-lookup obj attr-sym)])
      (if (not attr)
          
          ;; if attributte is not found, look for it on the object's dictionary or raise AttributeError
          (let ([obj-attr (hash-ref (instance_obj-attributes obj) attr-sym #f)])
            (cond
              [obj-attr obj-attr]
              [else (raise (make-py-exception py-AttributeError
                            (format "'~a' object has no attribute '~a'"
                                    (type_obj-name (type obj)) attr-name)))]))
          
          (let* ([attr-typ (type attr)]
                 [getter (type_obj-getter attr-typ)]
                 [setter (type_obj-setter attr-typ)])
            (if setter
                ;; if attribute is a descriptor, use the getter
                (getter attr obj (type obj))
                ;; else, look in object's dictionary, use getter if descriptor, or return attribute
                (let ([obj-attr (hash-ref (instance_obj-attributes obj) attr-sym #f)])
                  (cond
                    [obj-attr obj-attr]
                    [getter (getter attr obj (type obj))]
                    [else attr])))))))
  
  
  
  (define (py-object-setattribute obj attr-name value)
    (let ([attr (mro-lookup obj (string->symbol attr-name))])
      (if (not attr)
          ;; if attr not found, raise Attribute Error
          (raise (make-py-exception py-AttributeError
                  (format "'~a' object has no attribute '~a'"
                          (type_obj-name (type obj)) attr-name)))
          (let ([setter (type_obj-setter (type attr))])
            (if setter
                ;; if attr is a data descriptor, use the setter
                (setter attr obj value)
                ;; else attribute is read-only
                (raise (make-py-exception py-AttributeError
                        (format "'~a' object attribute '~a' is read-only"
                                (type_obj-name (type obj)) attr-name))))))))
  
  
  (define (py-instance-setattribute obj attr-name value)
    (let* ([attr-sym (string->symbol attr-name)]
           [attr (mro-lookup obj attr-sym)])
      (if (not attr)
          ;; if attr not found, we must set it on object dictionary
          (hash-set! (instance_obj-attributes obj) attr-sym value)
          (let ([setter (type_obj-setter (type attr))])
            (if setter
                ;; if attr is a data descriptor, use the setter
                (setter attr obj value)
                ;; else attribute is read-only
                (raise (make-py-exception py-AttributeError
                        (format "'~a' object attribute '~a' is read-only"
                                (type_obj-name (type obj)) attr-name))))))))
  
  
  ;;;;;;;;;;;
  
  (define (py-object-new typ . args)
    (cond
      [(and (> (length args) 0)
            (eq? (mro-type-lookup typ '__init__) py-object-init))
       (raise (make-py-exception py-TypeError "object.__new__() takes no parameters"))]
      [(and (not (eq? typ py-object))
            (mro-lookup typ '__new__))
       (raise (make-py-exception py-TypeError
               (format "object.__new__(~a) is not safe, use ~a.__new__()"
                       (type_obj-name typ) (type_obj-name typ))))]
      [else
       (python-object typ)]))
  
  (define (py-object-init obj . args)
    (when (and (> (length args) 0)
               (eq? (mro-lookup obj '__new__) py-object-new))
      (raise (make-py-exception py-TypeError
                                "object.__init__() takes no parameters"))))
  
      
  (define (py-object-new-instance typ . args)
    (if (and (> (length args) 0)
             (eq? (mro-type-lookup typ '__init__) py-object-init))
        (raise (make-py-exception py-TypeError "object.__new__() takes no parameters"))
        (instance_obj typ (make-hasheq))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; used when defining a class with __slots__
  (struct slotted-instance_obj instance_obj
    (slots))
  
  (define (py-object-new-instance/no-dict typ . args)
    (slotted-instance_obj typ #f (make-hasheq)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (py-object-repr x)
    (format "<~a object at 0x~x>" (type_obj-name (type x)) (id x)))
  
  (fill-type! py-object
    "object"
    (vector)
    (hasheq '__class__ (make-getset py-object-get-class
                                   py-object-set-class!)
          '__delattr__ #f
          '__doc__ #f
          '__format__ #f
          '__getattribute__ py-object-getattribute
          '__hash__ #f
          '__init__ py-object-init
          '__new__ py-object-new
          '__reduce__ #f
          '__reduce_ex__ #f
          '__repr__ py-object-repr
          '__setattr__ py-object-setattribute
          '__sizeof__ #f
          '__str__ #f
          '__subclasshook__ #f)
    #:mro (vector py-object))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  
  (define (py-getset-get descr obj obj-type)
    (let ([getter (getset_obj-getter descr)])
      (if getter
          (getter obj)
          (raise (make-py-exception py-AttributeError
                  (format "attribute '~a' of '~a' objects is not readable"
                          (getset_obj-name descr)
                          (type_obj-name (getset_obj-objtype descr))))))))
    
  (define (py-getset-set descr obj value)
    (let ([setter (getset_obj-setter descr)])
      (if setter
          (setter obj value)
          (raise (make-py-exception py-AttributeError
                  (format "attribute '~a' of '~a' objects is not writable"
                          (getset_obj-name descr)
                          (type_obj-name (getset_obj-objtype descr))))))))
  
  (define (py-getset-delete descr obj value)
    (py-getset-set descr obj null))
  
  (define (py-getset-repr obj)
    (format "<attribute '~a' of '~a' objects>"
            (getset_obj-name obj) (type_obj-name (getset_obj-objtype obj))))
  
  
  (fill-type! py-getset_descriptor
    "getset_descriptor"
    (vector py-object)
    (hasheq '__delete__ py-getset-delete
          '__doc__ (make-getset getset_obj-doc)
          '__get__ py-getset-get
          '__getattribute__ py-object-getattribute
          '__name__ (make-getset getset_obj-name)
          '__objclass__ (make-getset getset_obj-objtype)
          '__repr__ py-getset-repr
          '__set__ py-getset-set))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define _pointerint
    (case (ctype-sizeof _pointer)
      [(4) _int32]
      [(8) _int64]))
  
  (define (id x)
    (cast x _racket _pointerint))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; TYPES
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define custom-types (list))
  
  (define (set-custom-type! pred? typ)
    (set! custom-types (dict-remove custom-types pred?))
    (set! custom-types (cons (cons pred? typ) custom-types))
    (sort-custom-types!))
  
  (define (remove-custom-type! pred?)
    (set! custom-types (dict-remove custom-types pred?)))
  
  
  (define predicate-parents (make-hasheq))
  
  (define (set-pred-subtype! subtype parent)
    (hash-set! predicate-parents subtype parent)
    (sort-custom-types!))
  
  (define (is-pred-subtype? subtype supertype)
    (and (hash-has-key? predicate-parents subtype)
         (or (eq? (hash-ref predicate-parents subtype) supertype)
             (is-pred-subtype? (hash-ref predicate-parents subtype) supertype))))
  
  (define (sort-custom-types!)
    (set! custom-types (sort custom-types
                             (lambda (x y)
                               (is-pred-subtype? (car x) (car y))))))
  
  
  (define (custom-type obj)
    (for/or ([(pred? typ) (in-dict custom-types)])
      (and (pred? obj) typ)))
  
  (define-syntax-rule (type x)
    (match x
      [(? number? x) (number-type x)]
      [(? string? x) py-string]
      [(? python-object? x) (python-object-type x)]
      [(? vector? x) py-tuple]
      [(? hash? x) py-dict]
      [(? python-dict? x) py-dict]
      [(? python-set? x) py-set]
      [(? boolean? x) py-bool]
      [(? exn? x) (racket-exception-type x)]
      [(? void? x) py-NoneType]

      [_ (or (custom-type x)
             py-racket-value)]))
  
  (define (get-type x)
    (type x))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (repr x)
    ((mro-find x '__repr__) x))
  
  (define (print-repr x)
    (let ([meth (or (mro-lookup x '__str__)
                    (mro-lookup x '__repr__))])
      (meth x)))
  
  (define (py-eq x y)
    (cond
      [(and (number? x) (number? y)) (= x y)]
      [(and (string? x) (string? y)) (string=? x y)]
      [else (let* ([eq (mro-lookup x '__eq__)]
                   [cmp (and (not eq)
                             (mro-lookup x '__cmp__))])
              (cond
                [eq (eq x y)]
                [cmp (= (cmp x y) 0)]
                [else (= (id x) (id y))]))]))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;;;;;;;;
  ;; dirty hack - avoid circular module dependency
  ;;;;;;;;;
  (require racket/runtime-path)
  (define-runtime-paths (bools.rkt collections.rkt exceptions.rkt racket_values.rkt numbers.rkt singletons.rkt strings.rkt runtime.rkt)
    (values "bools.rkt" "collections.rkt" "exceptions.rkt" "racket_values.rkt" "numbers.rkt" "singletons.rkt" "strings.rkt" "../runtime.rkt"))
  
  (define number-type           (dynamic-require numbers.rkt       'number-type))
  (define py-string             (dynamic-require strings.rkt       'py-string))
  (define python-dict?          (dynamic-require collections.rkt   'python-dict?))
  (define py-dict               (dynamic-require collections.rkt   'py-dict))
  (define python-set?           (dynamic-require collections.rkt   'python-set?))
  (define py-set                (dynamic-require collections.rkt   'py-set))
  (define py-tuple              (dynamic-require collections.rkt   'py-tuple))
  (define make-dictproxy        (dynamic-require collections.rkt   'make-dictproxy))
  (define py-bool               (dynamic-require bools.rkt         'py-bool))
  (define racket-exception-type (dynamic-require exceptions.rkt    'racket-exception-type))
  (define make-py-exception     (dynamic-require exceptions.rkt    'make-py-exception))
  (define py-TypeError          (dynamic-require exceptions.rkt    'py-TypeError))
  (define py-AttributeError     (dynamic-require exceptions.rkt    'py-AttributeError))
  (define py-NoneType           (dynamic-require singletons.rkt    'py-NoneType))
  (define py-racket-value       (dynamic-require racket_values.rkt 'py-racket-value))
  (define ->sequence            (dynamic-require runtime.rkt       '->sequence))
  
  (provide (except-out (all-defined-out)
                       number-type py-string python-dict? py-dict python-set? py-set
                       py-tuple make-dictproxy py-bool racket-exception-type make-py-exception
                       py-TypeError py-AttributeError py-NoneType py-racket-value
                       ->sequence))
  
  
  )