(module functions racket
  (provide (all-defined-out))
  (require "engine.rkt")
  
  
  (struct function_obj python-object
    (proc parameters)
    #:transparent
    #:property prop:procedure
    (struct-field-index proc))
  
  
  (define (py-function-get func obj type)
    (make-py-method func obj type))
  
  (define py-function
    (make-type "function"
               (vector py-object)
               (hasheq '__call__ #f
                     '__closure__ #f
                     '__code__ #f
                     '__defaults__ #f
                     '__delattr__ #f
                     '__dict__ #f
                     '__doc__ #f
                     '__get__ py-function-get
                     '__getattribute__ #f
                     '__globals__ #f
                     '__module__ #f
                     '__name__ #f
                     '__new__ #f
                     '__repr__ #f
                     '__setattr__ #f
                     'func_closure #f
                     'func_code #f
                     'func_defaults #f
                     'func_dict #f
                     'func_doc #f
                     'func_globals #f
                     'func_name #f)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (struct method_obj python-object
    (func self class  generated-proc)
    #:property prop:procedure (struct-field-index generated-proc))
  
  (define (make-py-method func self class)
    (if (void? self)
        (method_obj py-method func self class  func)
        (method_obj py-method func self class  (lambda args
                                                 (apply func self args)))))
  
  (define py-method
    (make-type "instancemethod"
               (vector py-object)
               (hasheq '__call__ #f
                     '__cmp__ #f
                     '__delattr__ #f
                     '__doc__ #f
                     '__func__ #f
                     '__get__ #f
                     '__getattribute__ #f
                     '__hash__ #f
                     '__new__ #f
                     '__repr__ #f
                     '__self__ #f
                     '__setattr__ #f
                     'im_class (make-getset method_obj-class)
                     'im_func (make-getset method_obj-func)
                     'im_self (make-getset method_obj-self))))
  
  )