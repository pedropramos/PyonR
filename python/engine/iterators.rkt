(module iterators racket
  (provide (all-defined-out))
  (require "engine.rkt"
           "exceptions.rkt")
  
  
  (define (iter obj)
    (let* ([iter-meth (mro-lookup obj '__iter__)]
           [getitem-meth (and (not iter-meth)
                              (mro-lookup obj '__getitem__))])
      (cond
        [iter-meth (iter-meth obj)]
        [getitem-meth (make-getitem-py-iterator obj)]
        [else (exn-raise py-TypeError
                         "'~a' object is not iterable"
                         (type_obj-name (get-type obj)))])))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (struct iterator_obj python-object
    (seq [index #:mutable] getitem))
  
  (define (make-getitem-py-iterator obj)
    (iterator_obj py-iterator obj 0 (mro-find obj '__getitem__)))
  
  
  (define (py-iterator-next pyiter)
    (with-handlers ([(type-predicate py-IndexError)
                     (lambda (exn)
                       (exn-raise py-StopIteration))])
      (begin0 ((iterator_obj-getitem pyiter) (iterator_obj-seq pyiter)
                                             (iterator_obj-index pyiter))
              (set-iterator_obj-index! pyiter (+ (iterator_obj-index pyiter) 1)))))
  
  (define py-iterator
    (make-type "iterator"
               (vector py-object)
               (hasheq '__doc__ #f
                     '__getattribute__ #f
                     '__iter__ #f
                     '__length_hint__ #f
                     'next py-iterator-next)))
    
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (struct generator_obj python-object
    (gen))
  
  (define (make-py-generator gen)
    (generator_obj py-generator gen))
  
  (define (py-generator-next pygen)
    (let ([result ((generator_obj-gen pygen))])
      (if (void? result)
          (exn-raise py-StopIteration)
          result)))
  
  (define py-generator
    (make-type "generator"
               (vector py-object)
               (hasheq '__doc__ #f
                     '__getattribute__ #f
                     '__iter__ identity
                     '__name__ #f
                     '__repr__ #f
                     'close #f
                     'gi_code #f
                     'gi_frame #f
                     'gi_running #f
                     'next py-generator-next
                     'send #f
                     'throw #f)))
  
  )