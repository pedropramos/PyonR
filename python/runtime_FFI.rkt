(module runtime racket
  (require "libpython.rkt")
  
  (provide make-py-int
           make-py-long
           make-py-float
           make-py-complex
           make-py-string
           make-py-bool
           
           integer-from-py
           long-from-py
           float-from-py
           complex-from-py
           string-from-py
           boolean-from-py
           
           py-or
           py-and
           py-bwor
           py-bwxor
           py-bwand
           py-lshift
           py-rshift
           py-add
           py-sub
           py-mul
           py-div
           py-mod
           py-floordiv
           py-pow
           
           py-not
           py-unary-plus
           py-unary-minus
           py-bwinvert
           
           py-lt
           py-gt
           py-eq
           py-ge
           py-le
           py-ne
           py-ne
           py-in
           py-notin
           py-is
           py-isnot
           
           py-index
           py-set-index
           
           py-truth
           :True
           :False
           
           list->pylist
           make-py-list
           list->pytuple
           make-pytuple
           
           py-None
           py-import
           py-call
           py-for
           py-print)
  
  
  (define (make-py-int i)
    (PyInt_FromLong i))
  
  (define (make-py-long l) 
    (PyLong_FromLong l))
  
  (define (make-py-float f) 
    (PyFloat_FromDouble (+ f 0.0)))
  
  (define (make-py-complex c)
    (PyComplex_FromCComplex (make-Py_complex (+ (real-part c) 0.0)
                                             (+ (imag-part c) 0.0))))
  
  (define (make-py-string s) 
    (PyString_FromString s))
  
  (define (make-py-bool b)
    (PyBool_FromLong (if b 1 0)))

  ;;;;;;;;;;;;;;;;;;
  
  (define (integer-from-py x) 
    (PyInt_AsLong x))
  
  (define (long-from-py x) 
    (PyLong_AsLong x))
  
  (define (float-from-py x) 
    (PyFloat_AsDouble x))
  
  (define (complex-from-py x)
    (let ([ccomplex (PyComplex_AsCComplex x)])
      (make-rectangular (Py_complex-real ccomplex)
                        (Py_complex-imag ccomplex))))
  
  (define (string-from-py x)
    (PyString_AsString x))
  
  (define (boolean-from-py x)
    (= (PyObject_IsTrue x) 1))
  
  ;;;;;;;;;;;;;;;;;;
  
  (define-syntax-rule (py-or x y)
    (make-py-bool (or (boolean-from-py x)
                      (boolean-from-py y))))
  
  (define-syntax-rule (py-and x y)
    (make-py-bool (and (boolean-from-py x)
                       (boolean-from-py y))))
  
  (define __operator__ (PyImport_Import (PyString_FromString "operator")))
  
  (define py-bwor-obj (PyObject_GetAttrString __operator__ "__or__"))
  (define py-bwxor-obj (PyObject_GetAttrString __operator__ "__xor__"))
  (define py-bwand-obj (PyObject_GetAttrString __operator__ "__and__"))
  (define py-lshift-obj (PyObject_GetAttrString __operator__ "__lshift__"))
  (define py-rshift-obj (PyObject_GetAttrString __operator__ "__rshift__"))
  (define py-add-obj (PyObject_GetAttrString __operator__ "__add__"))
  (define py-sub-obj (PyObject_GetAttrString __operator__ "__sub__"))
  (define py-mul-obj (PyObject_GetAttrString __operator__ "__mul__"))
  (define py-div-obj (PyObject_GetAttrString __operator__ "__div__"))
  (define py-mod-obj (PyObject_GetAttrString __operator__ "__mod__"))
  (define py-floordiv-obj (PyObject_GetAttrString __operator__ "__floordiv__"))
  (define py-pow-obj (PyObject_GetAttrString __operator__ "__pow__"))
  
  (define (py-bwor x y) (py-call/PyObject py-bwor-obj (list x y)))
  (define (py-bwxor x y) (py-call/PyObject py-bwxor-obj (list x y)))
  (define (py-bwand x y) (py-call/PyObject py-bwand-obj (list x y)))
  (define (py-lshift x y) (py-call/PyObject py-lshift-obj (list x y)))
  (define (py-rshift x y) (py-call/PyObject py-rshift-obj (list x y)))
  (define (py-add x y) (py-call/PyObject py-add-obj (list x y)))
  (define (py-sub x y) (py-call/PyObject py-sub-obj (list x y)))
  (define (py-mul x y) (py-call/PyObject py-mul-obj (list x y)))
  (define (py-div x y) (py-call/PyObject py-div-obj (list x y)))
  (define (py-mod x y) (py-call/PyObject py-mod-obj (list x y)))
  (define (py-floordiv x y) (py-call/PyObject py-floordiv-obj (list x y)))
  (define (py-pow x y) (py-call/PyObject py-pow-obj (list x y)))
  
  ;;;;;;;;;;;;;;;;;;
  
  (define (py-not x)
    (PyBool_FromLong (PyObject_Not x)))
  
  (define py-pos-obj (PyObject_GetAttrString __operator__ "__pos__"))
  (define py-neg-obj (PyObject_GetAttrString __operator__ "__neg__"))
  (define py-inv-obj (PyObject_GetAttrString __operator__ "__inv__"))
  
  (define (py-unary-plus x) (py-call/PyObject py-pos-obj (list x)))
  (define (py-unary-minus x) (py-call/PyObject py-neg-obj (list x)))
  (define (py-bwinvert x) (py-call/PyObject py-inv-obj (list x)))
  
  ;;;;;;;;;;;;;;;;;;
  
  (define (py-lt x y)
    (PyObject_RichCompare x y 0))
  
  (define (py-gt x y)
    (PyObject_RichCompare x y 4))
  
  (define (py-eq x y) 
    (PyObject_RichCompare x y 2))
  
  (define (py-ge x y) 
    (PyObject_RichCompare x y 5))
  
  (define (py-le x y) 
    (PyObject_RichCompare x y 1))
  
  (define (py-ne x y) 
    (PyObject_RichCompare x y 3))
  
  (define (py-in x y) 
    (PyBool_FromLong (PySequence_Contains y x)))
  
  (define (py-notin x y) 
    (py-not (py-in x y)))
  
  (define (py-is x y) 
    (make-py-bool (eq? x y)))
  
  (define (py-isnot x y) 
    (py-not (py-is x y)))
  
  ;;;;;;;;;;;;
  
  (define (py-index seq i)
    (PySequence_GetItem seq (integer-from-py i)))
  
  (define (py-set-index seq i item)
    (PySequence_SetItem seq (integer-from-py i) item))
  
  ;;;;;;;;;;;;
  
  ; returns #t or #f
  (define (py-truth x)
    (= (PyObject_IsTrue x) 1))
  
  (define :True (PyBool_FromLong 1))
  (define :False (PyBool_FromLong 0))
  
  ;;;;;;;;;;;;
  
  (define (list->pylist lst)
    (let ([pylist (PyList_New (length lst))])
      (for ([i (range (length lst))]
            [elem lst])
        (PyList_SetItem pylist i elem))
      pylist))
  
  (define (make-py-list . elems)
    (list->pylist elems))
  
  (define (list->pytuple lst)
    (let ([pytuple (PyTuple_New (length lst))])
      (for ([i (range (length lst))]
            [elem lst])
        (PyTuple_SetItem pytuple i elem))
      pytuple))
  
  (define (make-pytuple . elems)
    (list->pytuple elems))
  
  ;;;;;;;;;;;;
  
  ;;; TODO change this to python's none
  (define py-None (void))
  
  ; id: identifer
  ; module: string
  (define-syntax-rule (py-import id module)
    (define id (PyImport_Import (PyString_FromString module))))
  
  
  
  ;;; Tuple utilities
  
  
  
  
  ; py-call
  (define (py-call func . args)
    (if (procedure? func)
        (py-call/procedure func args)
        (py-call/PyObject func args)))
  
  ; func: PyObject*
  ; args: (list PyObject*)
  (define (py-call/PyObject func args)
    (PyObject_CallObject func (list->pytuple args)))
  
  ;(define (py-call/PyObject_LEAK func args)
  ;  (PyObject_CallObject_LEAK func (list->pytuple args)))
  
  ; func: procedure
  ; args: (list PyObject*)
  (define (py-call/procedure func args)
    (apply func args))
  

  
  (define-syntax (py-for stx)
    (syntax-case stx ()
      [(_ continue-sym (target sequence) body ...)
       #'(let* ([index -1]
                [seq sequence]
                [len (PySequence_Length seq)])
           (let continue-sym ()
             (set! index (add1 index))
             (unless (= index len)
               (set! target (PySequence_GetItem seq index))
               body ...
               (continue-sym))))]))
  
  (define (py-print . objs)
    (for-each (lambda (x)
                (display (PyString_AsString (PyObject_Str x)))
                (display #\space))
              objs)
    (newline))
  
  
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  BUILT-INS
  
  (provide :abs :all :any :basestring :bin
           :bool :bytearray :callable :chr
           :classmethod :cmp :compile :complex
           :delattr :dict :dir :divmod
           :enumerate :eval :execfile :file
           :filter :float :format :frozenset
           :getattr :globals :hasattr :hash 
           :help :hex :id :input :int 
           :isinstance :issubclass :iter :len 
           :list :locals :long :map :max 
           :memoryview :min :next :object 
           :oct :open :ord :pow :print 
           :property :range :raw_input :reduce 
           :reload :repr :reversed :round 
           :set :setattr :slice :sorted 
           :staticmethod :str :sum :super 
           :tuple :type :unichr :unicode 
           :vars :xrange :zip :__import__ 
           :apply :buffer :coerce :intern)
  
  (py-import __builtin__ "__builtin__")
  (define :abs (PyObject_GetAttrString __builtin__ "abs"))
  (define :all (PyObject_GetAttrString __builtin__ "all"))
  (define :any (PyObject_GetAttrString __builtin__ "any"))
  (define :basestring (PyObject_GetAttrString __builtin__ "basestring"))
  (define :bin (PyObject_GetAttrString __builtin__ "bin"))
  (define :bool (PyObject_GetAttrString __builtin__ "bool"))
  (define :bytearray (PyObject_GetAttrString __builtin__ "bytearray"))
  (define :callable (PyObject_GetAttrString __builtin__ "callable"))
  (define :chr (PyObject_GetAttrString __builtin__ "chr"))
  (define :classmethod (PyObject_GetAttrString __builtin__ "classmethod"))
  (define :cmp (PyObject_GetAttrString __builtin__ "cmp"))
  (define :compile (PyObject_GetAttrString __builtin__ "compile"))
  (define :complex (PyObject_GetAttrString __builtin__ "complex"))
  (define :delattr (PyObject_GetAttrString __builtin__ "delattr"))
  (define :dict (PyObject_GetAttrString __builtin__ "dict"))
  (define :dir (PyObject_GetAttrString __builtin__ "dir"))
  (define :divmod (PyObject_GetAttrString __builtin__ "divmod"))
  (define :enumerate (PyObject_GetAttrString __builtin__ "enumerate"))
  (define :eval (PyObject_GetAttrString __builtin__ "eval"))
  (define :execfile (PyObject_GetAttrString __builtin__ "execfile"))
  (define :file (PyObject_GetAttrString __builtin__ "file"))
  (define :filter (PyObject_GetAttrString __builtin__ "filter"))
  (define :float (PyObject_GetAttrString __builtin__ "float"))
  (define :format (PyObject_GetAttrString __builtin__ "format"))
  (define :frozenset (PyObject_GetAttrString __builtin__ "frozenset"))
  (define :getattr (PyObject_GetAttrString __builtin__ "getattr"))
  (define :globals (PyObject_GetAttrString __builtin__ "globals"))
  (define :hasattr (PyObject_GetAttrString __builtin__ "hasattr"))
  (define :hash (PyObject_GetAttrString __builtin__ "hash"))
  (define :help (PyObject_GetAttrString __builtin__ "help"))
  (define :hex (PyObject_GetAttrString __builtin__ "hex"))
  (define :id (PyObject_GetAttrString __builtin__ "id"))
  (define :input (PyObject_GetAttrString __builtin__ "input"))
  (define :int (PyObject_GetAttrString __builtin__ "int"))
  (define :isinstance (PyObject_GetAttrString __builtin__ "isinstance"))
  (define :issubclass (PyObject_GetAttrString __builtin__ "issubclass"))
  (define :iter (PyObject_GetAttrString __builtin__ "iter"))
  (define :len (PyObject_GetAttrString __builtin__ "len"))
  (define :list (PyObject_GetAttrString __builtin__ "list"))
  (define :locals (PyObject_GetAttrString __builtin__ "locals"))
  (define :long (PyObject_GetAttrString __builtin__ "long"))
  (define :map (PyObject_GetAttrString __builtin__ "map"))
  (define :max (PyObject_GetAttrString __builtin__ "max"))
  (define :memoryview (PyObject_GetAttrString __builtin__ "memoryview"))
  (define :min (PyObject_GetAttrString __builtin__ "min"))
  (define :next (PyObject_GetAttrString __builtin__ "next"))
  (define :object (PyObject_GetAttrString __builtin__ "object"))
  (define :oct (PyObject_GetAttrString __builtin__ "oct"))
  (define :open (PyObject_GetAttrString __builtin__ "open"))
  (define :ord (PyObject_GetAttrString __builtin__ "ord"))
  (define :pow (PyObject_GetAttrString __builtin__ "pow"))
  (define :print (PyObject_GetAttrString __builtin__ "print"))
  (define :property (PyObject_GetAttrString __builtin__ "property"))
  (define :range (PyObject_GetAttrString __builtin__ "range"))
  (define :raw_input (PyObject_GetAttrString __builtin__ "raw_input"))
  (define :reduce (PyObject_GetAttrString __builtin__ "reduce"))
  (define :reload (PyObject_GetAttrString __builtin__ "reload"))
  (define :repr (PyObject_GetAttrString __builtin__ "repr"))
  (define :reversed (PyObject_GetAttrString __builtin__ "reversed"))
  (define :round (PyObject_GetAttrString __builtin__ "round"))
  (define :set (PyObject_GetAttrString __builtin__ "set"))
  (define :setattr (PyObject_GetAttrString __builtin__ "setattr"))
  (define :slice (PyObject_GetAttrString __builtin__ "slice"))
  (define :sorted (PyObject_GetAttrString __builtin__ "sorted"))
  (define :staticmethod (PyObject_GetAttrString __builtin__ "staticmethod"))
  (define :str (PyObject_GetAttrString __builtin__ "str"))
  (define :sum (PyObject_GetAttrString __builtin__ "sum"))
  (define :super (PyObject_GetAttrString __builtin__ "super"))
  (define :tuple (PyObject_GetAttrString __builtin__ "tuple"))
  (define :type (PyObject_GetAttrString __builtin__ "type"))
  (define :unichr (PyObject_GetAttrString __builtin__ "unichr"))
  (define :unicode (PyObject_GetAttrString __builtin__ "unicode"))
  (define :vars (PyObject_GetAttrString __builtin__ "vars"))
  (define :xrange (PyObject_GetAttrString __builtin__ "xrange"))
  (define :zip (PyObject_GetAttrString __builtin__ "zip"))
  (define :__import__ (PyObject_GetAttrString __builtin__ "__import__"))
  (define :apply (PyObject_GetAttrString __builtin__ "apply"))
  (define :buffer (PyObject_GetAttrString __builtin__ "buffer"))
  (define :coerce (PyObject_GetAttrString __builtin__ "coerce"))
  (define :intern (PyObject_GetAttrString __builtin__ "intern"))
  
  
  )