(module descriptors racket
  (provide (all-defined-out))
  (require "engine.rkt"
           "singletons.rkt"
           "exceptions.rkt")
  
  
  (struct property_obj python-object
    (getter setter deleter doc) #:mutable)
  
  
  (define (py-property-new typ . args)
    (property_obj py-property py-None py-None py-None py-None))
  
  (define (py-property-init! self
                            [fget py-None]
                            [fset py-None]
                            [fdel py-None]
                            [doc py-None])
    (when (and (void? doc)
               (not (void? fget))
               (mro-lookup fget '__doc__))
      (set! doc (mro-lookup fget '__doc__)))
    (set-property_obj-getter! self fget)
    (set-property_obj-setter! self fset)
    (set-property_obj-deleter! self fdel)
    (set-property_obj-doc! self doc))
  
  
  (define (py-property-get self inst type)
    (cond
      [(void? inst) self]
      [(void? (property_obj-getter self)) (exn-raise py-AttributeError "unreadable attribute")]
      [else ((property_obj-getter self) inst)]))
  
  (define (py-property-set self inst value)
    (cond
      [(void? (property_obj-setter self)) (exn-raise py-AttributeError "can't set attribute")]
      [else ((property_obj-setter self) inst value)]))
  
  (define (py-property-delete self inst)
    (cond
      [(void? (property_obj-deleter self)) (exn-raise py-AttributeError "can't delete attribute")]
      [else ((property_obj-deleter self) inst)]))
  
  
  (define (py-property-getter self fget)
    (property_obj fget (property_obj-setter self) (property_obj-deleter self) (property_obj-doc self)))
  
  (define (py-property-setter self fset)
    (property_obj (property_obj-getter self) fset (property_obj-deleter self) (property_obj-doc self)))
  
  (define (py-property-deleter self fdel)
    (property_obj (property_obj-getter self) (property_obj-setter self) fdel (property_obj-doc self)))
  
  (define py-property
    (make-type "property"
               (vector py-object)
               (hasheq '__delete__ py-property-delete
                     '__doc__ (make-getset property_obj-doc)
                     '__get__ py-property-get
                     '__getattribute__ py-object-getattribute
                     '__init__ py-property-init!
                     '__new__ py-property-new
                     '__set__ py-property-set
                     'deleter py-property-deleter
                     'fdel (make-getset property_obj-deleter)
                     'fget (make-getset property_obj-getter)
                     'fset (make-getset property_obj-setter)
                     'getter py-property-getter
                     'setter py-property-setter)))
  
  )