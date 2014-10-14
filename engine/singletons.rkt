(module singletons racket
  (provide (all-defined-out))
  (require "engine.rkt")
  
  
  ;;; py-none
  (define py-NoneType
    (make-type "NoneType"
               (vector py-object)
               (hasheq '__doc__ #f
                     '__hash__ #f
                     '__repr__ (lambda (x) "None"))))
  
  (define py-None (void))
  
  ;;; py-notimplemented
  (define py-NotImplementedType
    (make-type "NotImplementedType"
               (vector py-object)
               (hasheq '__doc__ #f
                     '__repr__ #f)))
  
  (define py-NotImplemented
    (python-object py-NotImplementedType))
  
  
  ;;; py-ellipsis
  (define py-EllipsisType
    (make-type "ellipsis"
               (vector py-object)
               (hasheq '__doc__ #f
                     '__getattribute__ #f
                     '__repr__ #f)))
  
  (define py-Ellipsis
    (python-object py-EllipsisType))
  
  )