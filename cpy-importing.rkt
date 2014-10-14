(module cpy-importing racket
  (require "runtime.rkt"
           "libpython.rkt"
           (for-syntax "libpython.rkt")
           ffi/unsafe)
  (provide (all-defined-out))
  
  
  (define (cpy->racket x)
    (let ([type (PyString_AsString
                 (PyObject_GetAttrString
                  (PyObject_Type x) "__name__"))])
      (case type
        [("bool") (bool-from-cpy x)]
        [("int") (int-from-cpy x)]
        [("long") (long-from-cpy x)]
        [("float") (float-from-cpy x)]
        [("complex") (complex-from-cpy x)]
        [("str") (str-from-cpy x)]
        
        ;[("list") (list-from-cpy x)]
        ;[("tuple") (tuple-from-cpy x)]
        ;[("dict") (dict-from-cpy x)]
        ;[("dictproxy") (dictproxy-from-cpy x)]
        
        [("type") (type-from-cpy x)]
        
        [("module") (module-from-cpy x)]
        
        [("NoneType") py-None]
        
        [else ;(printf "cpy->racket: wrapping value ~a of type ~a\n" (PyString_AsString (PyObject_Repr x)) type)
              (proxy-obj-from-cpy x)])))
  
  
  (define (racket->cpy x)
    (cond
      [(boolean? x) (make-cpy-bool x)]
      [(exact-integer? x) (if (and (<= x 2147483647)
                                   (>= x -2147483648))
                              (make-cpy-int x)
                              (make-cpy-long x))]
      [(flonum? x) (make-cpy-float x)]
      [(complex? x) (make-cpy-complex x)]
      [(string? x) (make-cpy-str x)]
      
      [(vector? x) (make-cpy-tuple x)]
      
      [(void? x) cpy-none]
      
      [(proxy-type-object? x) (unwrap-proxy-type-object x)]
      [(proxy-object? x) (unwrap-proxy-object x)]
      
      [else (error "racket->cpy: value not supported:" x)]))
      
  
  
  
  (define-syntax (cpy-import stx)
    (syntax-case stx (as)
      [(_ module-name as id)
       (begin
         (Py_Initialize)
         #'(define id (module-from-cpy
                       (PyImport_Import
                        (PyString_FromString module-name)))))]))
  
  (define-syntax (cpy-from stx)
    (syntax-case stx (import as)
      [(_ module-name import ((orig-name as bind-id) ...))
       (begin
         (Py_Initialize)
         #'(begin
             (define bind-id 'undefined)
             ...
             (let ([cpy-module (PyImport_Import
                                (PyString_FromString module-name))])
               (set! bind-id (cpy->racket
                              (PyObject_GetAttrString cpy-module orig-name)))
               ...)))]))
  
  
  (require (for-syntax (only-in "name-mangling.rkt" string->colon-symbol)))
  (define-syntax (cpy-from-import-* stx)
    (syntax-case stx ()
      [(_ module-name)
       (begin
         (Py_Initialize)
         (let* ([bindings-names (cpy-module-exports (syntax->datum #'module-name))]
                [bindings (map string->colon-symbol bindings-names)])
           #`(begin
               (define cpy-module (PyImport_Import (PyString_FromString module-name)))
               #,@(for/list ([binding/str bindings-names]
                             [binding/sym bindings])
                    (with-syntax ([id (datum->syntax stx binding/sym)]
                                  [id-name (datum->syntax stx binding/str)])
                      #'(define id (cpy->racket
                                    (PyObject_GetAttrString cpy-module id-name))))))))]))
  
  (require (for-syntax "libpython.rkt"))
  (define-for-syntax (cpy-module-exports module-name)
    (let* ([cpy-module (PyImport_Import (PyString_FromString module-name))]
           [cpy-exports (PyDict_Keys (PyModule_GetDict cpy-module))]
           [len (PyList_Size cpy-exports)])
      (filter (lambda (str) (not (eq? (string-ref str 0) #\_)))
              (for/list ([i (in-range len)])
                (PyString_AsString (PyList_GetItem cpy-exports i))))))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (make-cpy-bool b)
    (PyBool_FromLong (if b 1 0)))
      
  (define (make-cpy-int i)
    (PyInt_FromLong i))
  
  (define (make-cpy-long l) 
    (PyLong_FromString (number->string l) 0 10))
  
  (define (make-cpy-float f) 
    (PyFloat_FromDouble (+ f 0.0)))
  
  (define (make-cpy-complex c)
    (PyComplex_FromCComplex (make-Py_complex (+ (real-part c) 0.0)
                                             (+ (imag-part c) 0.0))))
  (define (make-cpy-str s) 
    (PyString_FromString s))
  
  (define (make-cpy-tuple v)
    (let* ([len (vector-length v)]
           [cpy-tuple (PyTuple_New len)])
      (for ([i (in-range len)]
            [item (in-vector v)])
        (PyTuple_SetItem cpy-tuple i (racket->cpy item)))
      cpy-tuple))
  
  (define cpy-none (get_PyNone))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define recursion-cache (make-parameter (hash)))
  
  (define (bool-from-cpy x)
    (= (PyObject_IsTrue x) 1))
  
  (define (int-from-cpy x) 
    (PyInt_AsLong x))
  
  (define (long-from-cpy x) 
    (string->number
     (PyString_AsString
      (PyObject_Str x))))
  
  (define (float-from-cpy x) 
    (PyFloat_AsDouble x))
  
  (define (complex-from-cpy x)
    (let ([ccomplex (PyComplex_AsCComplex x)])
      (make-rectangular (Py_complex-real ccomplex)
                        (Py_complex-imag ccomplex))))
  
  (define (str-from-cpy x)
    (with-handlers ([exn:fail?
                     (lambda (e) "ERROR")])
      (PyString_AsString x)))
  
  
  
  (define (list-from-cpy x)
    (let* ([id-x (cpointer-id x)]
           [cached (hash-ref (recursion-cache) id-x #f)])
      (or cached
          (let* ([len (PyList_Size x)]
                 [result (vector->py-list (make-vector len))])
            (parameterize ([recursion-cache (hash-set (recursion-cache) id-x result)])
              (for ([i (in-range len)])
                (py-list-setitem! result i (cpy->racket (PyList_GetItem x i))))
              result)))))
  
  (define (tuple-from-cpy x)
    (let* ([id-x (cpointer-id x)]
           [cached (hash-ref (recursion-cache) id-x #f)])
      (or cached
          (let* ([len (PyTuple_Size x)]
                 [result (make-vector len)])
            (parameterize ([recursion-cache (hash-set (recursion-cache) id-x result)])
              (for ([i (in-range len)])
                (vector-set! result i (cpy->racket (PyTuple_GetItem x i))))
              result)))))
                
  
  
  
;  (define (convert-dict-from-cpy x)
;    (let ([type (PyString_AsString
;                 (PyObject_GetAttrString
;                  (PyObject_Type x) "__name__"))])
;      (case type
  
  ;; used only for holding attribute/method names as keys (implemented as symbols)
  (define (dict-from-cpy x)
    (let* ([id-x (cpointer-id x)]
           [cached (hash-ref (recursion-cache) id-x #f)])
      (or cached
          (let* ([cpy-keys (PyDict_Keys x)]
                 [size (PyList_Size cpy-keys)]
                 [result (make-hasheq)])
            (parameterize ([recursion-cache (hash-set (recursion-cache) id-x result)])
              (for ([i (in-range size)])
                (let ([cpy-key (PyList_GetItem cpy-keys i)])
                  (hash-set! result
                             (string->symbol (cpy->racket cpy-key))
                             (cpy->racket (PyDict_GetItem x cpy-key)))))
              result)))))
      
  
  (define (module-from-cpy x)
    (make-py-module (str-from-cpy (PyObject_GetAttrString x "__name__"))
                    (dict-from-cpy (PyObject_GetAttrString x "__dict__"))))
  
              
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (struct proxy-object python-object
    (ffi-object)
    #:transparent
    #:property prop:procedure
    (cpy-call-generator #:unwrapper unwrap-proxy-object))
  
  (define-syntax-rule (cpy-call-generator #:unwrapper unwrapper)
    (lambda (f . args)
      (let ([ffi_call_result
             (PyObject_CallObject (unwrapper f)
                                  (list->cpy-tuple (map racket->cpy args)))])
        (if ffi_call_result
            (cpy->racket ffi_call_result)
            (let ([cpy-exception (second (PyErr_Fetch))])
              (raise (cpy->racket cpy-exception)))))))
  
  
  (define (proxy-obj-from-cpy x)
    (proxy-object (cpy->racket (PyObject_Type x)) x))
  
  (define (unwrap-proxy-object proxy)
    (proxy-object-ffi-object proxy))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define type-cache (make-custom-hash ptr-equal? cpointer-id))
  
  (struct proxy-type-object type_obj
    (ffi-object)
    #:mutable
    #:property prop:procedure
    (cpy-call-generator #:unwrapper unwrap-proxy-type-object))
  
  (define (empty-proxy-type)
    (proxy-type-object #f
                       #f #f #f #f #f #f 
                       #f #f
                       #f))
  
  (define-syntax-rule (fill-proxy-type! (id ffi-obj))
    (begin
      (set-python-object-type! id py-type)
      (set-type_obj-name!   id (str-from-cpy   (PyObject_GetAttrString ffi-obj "__name__")))
      (set-type_obj-module! id (str-from-cpy   (PyObject_GetAttrString ffi-obj "__module__")))
      (set-type_obj-bases!  id (tuple-from-cpy (PyObject_GetAttrString ffi-obj "__bases__")))
      (set-type_obj-doc!    id (cpy->racket    (PyObject_GetAttrString ffi-obj "__doc__")))
      (set-type_obj-dict!   id (dict-from-cpy  (PyType_Dict ffi-obj)))
      (set-type_obj-mro!    id (tuple-from-cpy (PyObject_GetAttrString ffi-obj "__mro__")))
      
      ;(set-type_obj-getter! id (let ([getter (PyObject_GetAttrString ffi-obj "__get__")])
      ;                           (if getter (cpy->racket getter) getter)))
      ;(set-type_obj-setter! id (let ([setter (PyObject_GetAttrString ffi-obj "__set__")])
      ;                           (if setter (cpy->racket setter) setter)))
      
      (set-proxy-type-object-ffi-object! id ffi-obj)))
  
  (define (type-from-cpy x)
    (let* ([cached (dict-ref type-cache x #f)])
      (or cached
          (let ([result (empty-proxy-type)])
            (dict-set! type-cache x result)
            (fill-proxy-type! (result x))
            result))))
  
  (define (unwrap-proxy-type-object proxy)
    (proxy-type-object-ffi-object proxy))
  
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (list->cpy-tuple lst)
    (let* ([len (length lst)]
           [cpy-tuple (PyTuple_New len)])
      (for ([i (in-range len)]
            [item (in-list lst)])
        (PyTuple_SetItem cpy-tuple i item))
      cpy-tuple))
  
  )