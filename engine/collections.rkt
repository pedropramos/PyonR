(module collections racket
  (provide (all-defined-out))
  (require "engine.rkt"
           "exceptions.rkt"
           "iterators.rkt"
           racket/generator)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (struct list_obj python-object
    (vector filled)
    #:mutable
    #:transparent
    #:property prop:sequence
    (lambda (pl)
      (make-do-sequence
       (lambda ()
         (values (lambda (pos) (vector-ref (list_obj-vector pl) pos))
                 add1
                 0
                 (lambda (pos) (< pos (list_obj-filled pl)))
                 #f
                 #f)))))
  
  (define (list_obj-allocated pl)
    (vector-length (list_obj-vector pl)))
  
  
  (define (vector->py-list v)
    (list_obj py-list v (vector-length v)))
  
  (define (py-list->vector pl)
    (if (< (list_obj-filled pl)
           (list_obj-allocated pl))
        (vector-take (list_obj-vector pl)
                     (list_obj-filled pl))
        (list_obj-vector pl)))
  
  (define-syntax-rule (list->py-list l)
    (vector->py-list (list->vector l)))
  
  (define-syntax-rule (py-list->list pl)
    (vector->list (py-list->vector pl)))
  
  (define-syntax-rule (for/py-list args ...)
    (vector->py-list
     (for/vector args ...)))
  
  (define-syntax-rule (for*/py-list args ...)
    (vector->py-list
     (for*/vector args ...)))
  
  
  
  (define (make-py-list . args)
    (list->py-list args))
  
  
  
  
  (define (py-list-new typ . args)
    (cond 
      [(empty? args) (make-py-list)]
      [(not (mro-lookup (first args) '__len__)) (make-py-list)]
      [else (vector->py-list (make-vector (py-len (first args))))]))
  
  (define (py-list-init! pl . args)
    (when (not (empty? args))
      (let ([vec (list_obj-vector pl)])
        (if (zero? (vector-length vec))
            
            (for ([item (->sequence (first args))])
              (py-list-append! pl item))
            
            (for ([item (->sequence (first args))]
                  [i (in-naturals 0)])
              (vector-set! vec i item))))))
  
  
  (define (py-list-resize! pl new-size)
    (let ([old-vector (list_obj-vector pl)]
          [new-vector (make-vector new-size)])
      (for ([i (in-range (list_obj-filled pl))])
        (vector-set! new-vector i (vector-ref old-vector i)))
      (set-list_obj-vector! pl new-vector)))
  
  (define (next-size-for-allocation n)
    (ceiling (/ (* (+ n 3) 16) 10)))
  
  (define (py-list-embiggen! pl)
    (py-list-resize! pl (next-size-for-allocation (list_obj-allocated pl))))
      
  
  
  (define (py-list-len pl)
    (list_obj-filled pl))
  
    (define (py-list-getitem pl index)
    (if (< (abs index) (list_obj-filled pl))
        (vector-ref (list_obj-vector pl)
                    (if (< index 0)
                        (+ (list_obj-filled pl) index)
                        index))
        (exn-raise py-IndexError "list index out of range")))
  
  (define (py-list-setitem! pl index item)
    (if (< (abs index) (list_obj-filled pl))
        (vector-set! (list_obj-vector pl) 
                     (if (< index 0)
                        (+ (list_obj-filled pl) index)
                        index)
                     item)
        (exn-raise py-IndexError "list assignment index out of range")))
  
  (define (py-list-append! pl item)
    (when (= (list_obj-filled pl) (list_obj-allocated pl))
      (py-list-embiggen! pl))
    (vector-set! (list_obj-vector pl) (list_obj-filled pl) item)
    (set-list_obj-filled! pl (add1 (list_obj-filled pl))))
  
  (define (py-list-extend! pl other)
    (let* ([pl-len (list_obj-filled pl)]
           [other-len (list_obj-filled other)]
           [combined-len (+ pl-len other-len)])
      (let loop ()
        (when (> combined-len (list_obj-allocated pl))
          (py-list-embiggen! pl)
          (loop)))
      (let ([pl-vector (list_obj-vector pl)]
            [other-vector (list_obj-vector other)])
        (for ([i (in-range other-len)])
          (vector-set! pl-vector (+ pl-len i) (vector-ref other-vector i)))
        (set-list_obj-filled! pl combined-len))))
             
  
  (define (py-list-getslice pl low high)
    (let ((low (if (< low 0) (+ (list_obj-filled pl) low) low))
          (high (if (< high 0) (+ (list_obj-filled pl) high) high)))
      (let* ([pl-len (list_obj-filled pl)]
             [low (max low 0)]
             [high (max (inexact->exact (min high pl-len)) low)])
        (vector->py-list
         (vector-copy
          (py-list->vector pl) low high))))) 
    
  
  
  (define (py-list-add pl1 pl2)
    (let* ([pl1-vector (list_obj-vector pl1)]
           [pl2-vector (list_obj-vector pl2)]
           [pl1-len (list_obj-filled pl1)]
           [pl2-len (list_obj-filled pl2)]
           [pl1+pl2-vector (make-vector (+ pl1-len pl2-len))])
      (for ([i (in-range pl1-len)])
        (vector-set! pl1+pl2-vector i (vector-ref pl1-vector i)))
      (for ([i (in-range pl2-len)])
        (vector-set! pl1+pl2-vector (+ pl1-len i) (vector-ref pl2-vector i)))
      (vector->py-list pl1+pl2-vector)))
  
  (define (py-list-multiply pl times)
    (let* ([old-vector (list_obj-vector pl)]
           [old-len (list_obj-filled pl)]
           [new-len (* old-len times)]
           [new-vector (make-vector new-len)])
      (for ([i (in-range new-len)])
        (vector-set! new-vector i (vector-ref old-vector (remainder i old-len))))
      (vector->py-list new-vector)))
  
  (define (make-py-listiterator pl)
    (listiterator_obj py-listiterator pl 0))
  
  (define (py-list-eq? pl1 pl2)
    (and (list_obj? pl1) (list_obj? pl2)
         (= (list_obj-filled pl1) (list_obj-filled pl2))
         (for/and ([index (in-range (list_obj-filled pl1))]
                   [item1 (in-vector (list_obj-vector pl1))]
                   [item2 (in-vector (list_obj-vector pl2))])
           (py-eq item1 item2))))
  
  (define (py-list-contains? pl obj)
    (let ([pl-vector (list_obj-vector pl)])
      (for/or ([i (in-range (list_obj-filled pl))])
        (py-eq obj (vector-ref pl-vector i)))))
  
  (define (py-list-repr pl)
    (let ([reprs (map repr (py-list->list pl))])
      (if (empty? reprs)
          "[]"
          (string-append "[" 
                         (foldl (lambda (s1 s2) (string-append s2 ", " s1))
                                (first reprs) (rest reprs))
                         "]"))))
  
  (define py-list
    (make-type "list"
               (vector py-object)
               (hasheq '__add__ py-list-add
                     '__contains__ py-list-contains?
                     '__delitem__ #f
                     '__delslice__ #f
                     '__doc__ #f
                     '__eq__ py-list-eq?
                     '__ge__ #f
                     '__getattribute__ #f
                     '__getitem__ py-list-getitem
                     '__getslice__ py-list-getslice
                     '__gt__ #f
                     '__hash__ #f
                     '__iadd__ #f
                     '__imul__ #f
                     '__init__ py-list-init!
                     '__iter__ make-py-listiterator
                     '__le__ #f
                     '__len__ py-list-len
                     '__lt__ #f
                     '__mul__ py-list-multiply
                     '__ne__ #f
                     '__new__ py-list-new
                     '__repr__ py-list-repr
                     '__reversed__ #f
                     '__rmul__ #f
                     '__setitem__ py-list-setitem!
                     '__setslice__ #f
                     '__sizeof__ #f
                     'append py-list-append!
                     'count #f
                     'extend py-list-extend!
                     'index #f
                     'insert #f
                     'pop #f
                     'remove #f
                     'reverse #f
                     'sort #f)))
  
  
  ;;;;;  ITERATOR  ;;;;;
  
  (struct listiterator_obj python-object
    (list [index #:mutable]))

  (define (py-listiterator-next self)
    (let ([index (listiterator_obj-index self)]
          [lst (listiterator_obj-list self)])
      (if (>= index (py-list-len lst))
          (exn-raise py-StopIteration)
          (begin0 (py-list-getitem lst index)
                  (set-listiterator_obj-index! self (add1 index))))))
  
  (define py-listiterator
    (make-type "listiterator"
               (vector py-object)
               (hasheq '__doc__ #f
                     '__getattribute__ #f
                     '__iter__ identity
                     '__length_hint__ #f
                     'next py-listiterator-next)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (py-tuple-new typ . args)
    (if (empty? args)
        (make-vector 0)
        (let ([tup (make-vector (py-len (first args)))])
          (for ([item (->sequence (first args))]
                [i (in-naturals 0)])
            (vector-set! tup i item))
          tup)))
  
  
  (define (py-tuple-len tup)
    (vector-length tup))
  
  (define (py-tuple-getitem tup index)
    (vector-ref tup index))
  
  (define (py-tuple-setitem! tup index item)
    (vector-set! tup index item))
  
  (define (py-tuple-eq? tup1 tup2)
    (and (vector? tup1) (vector? tup2)
         (= (vector-length tup1) (vector-length tup2))
         (for/and ([item1 (in-vector tup1)]
                   [item2 (in-vector tup2)])
           (py-eq item1 item2))))
  
  (define (py-tuple-repr tup)
    (let ([reprs (map repr (vector->list tup))])
      (case (length reprs)
        [(0) "()"]
        [(1) (format "(~a,)" (first reprs))]
        [else (string-append "(" 
                             (foldl (lambda (s1 s2) (string-append s2 ", " s1))
                                    (first reprs) (rest reprs))
                             ")")])))
        
  (define py-tuple
    (make-type "tuple"
               (vector py-object)
               (hasheq '__add__ #f
                     '__contains__ #f
                     '__doc__ #f
                     '__eq__ py-tuple-eq?
                     '__ge__ #f
                     '__getattribute__ #f
                     '__getitem__ py-tuple-getitem
                     '__getnewargs__ #f
                     '__getslice__ #f
                     '__gt__ #f
                     '__hash__ #f
                     '__iter__ #f
                     '__le__ #f
                     '__len__ py-tuple-len
                     '__lt__ #f
                     '__mul__ #f
                     '__ne__ #f
                     '__new__ py-tuple-new
                     '__repr__ py-tuple-repr
                     '__rmul__ #f
                     '__sizeof__ #f
                     'count #f
                     'index #f)))
  
  (define-syntax-rule (make-py-tuple arg ...)
    (vector arg ...))
  
  (define-syntax-rule (vector->py-tuple x) x)
  (define-syntax-rule (py-tuple->vector x) x)
  
  (define-syntax for/py-tuple #'for/vector)
  (define-syntax for*/py-tuple #'for*/vector)
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (py-hash x)
    (if (string? x)
        (equal-hash-code x)
        (let ([hash-meth (or (mro-lookup x '__hash__)
                             (exn-raise py-TypeError
                                        "unhashable type: '~a'"
                                        (type_obj-name (get-type x))))])
          (hash-meth x))))
  
  (define (equal-for-hashing x y)
    (and (py-eq x y)
         (= (py-hash x) (py-hash y))))
      
  
  (define-custom-hash-types python-dict
    py-eq py-hash)
  
  
  (define (py-dict-new typ . args)
    (make-mutable-python-dict))
  
  (define (py-dict-init! d . args)
    (when (not (empty? args))
      (let ([seq (first args)])
        (if (dict? seq)
            
            (for ([(k v) (in-dict seq)])
              (dict-set! d k v))
            
            (for ([kv (->sequence seq)]
                  [i (in-naturals 0)])
              (if (= (py-len kv) 2)
                  (dict-set! d (py-get-index kv 0)
                               (py-get-index kv 1))
                  (exn-raise py-ValueError
                             "dictionary update sequence element #~a has length ~a; 2 is required"
                             i (py-len kv))))))))
  
  (define (py-dict-keys d)
    (vector->py-list
     (for/vector ([k (in-dict-keys d)]) k)))
  
  (define (py-dict-values d)
    (vector->py-list
     (for/vector ([v (in-dict-values d)]) v)))
  
  (define (py-dict-items d)
    (vector->py-list
     (for/vector ([(k v) (in-dict d)])
       (make-py-tuple k v))))
  
  (define (py-dict-eq? d1 d2)
    (and (dict? d1) (dict? d2)
         (= (dict-count d1) (dict-count d2))
         (for/and ([(k1 v1) (in-dict d1)]
                   [(k2 v2) (in-dict d2)])
           (and (py-eq k1 k2)
                (py-eq v1 v2)))))
  
  (define (py-dict-repr d)
    (let ([reprs (dict-map d (lambda (k v)
                               (format "~a: ~a" (repr k) (repr v))))])
      (case (length reprs)
        [(0) "{}"]
        [else (string-append "{" 
                             (foldl (lambda (s1 s2) (string-append s2 ", " s1))
                                    (first reprs) (rest reprs))
                             "}")])))
  
  (define py-dict
    (make-type "dict"
               (vector py-object)
               (hasheq '__cmp__ #f
                     '__contains__ #f
                     '__delitem__ #f
                     '__doc__ #f
                     '__eq__ py-dict-eq?
                     '__ge__ #f
                     '__getattribute__ #f
                     '__getitem__ dict-ref
                     '__gt__ #f
                     '__hash__ #f
                     '__init__ py-dict-init!
                     '__iter__ #f
                     '__le__ #f
                     '__len__ dict-count
                     '__lt__ #f
                     '__ne__ #f
                     '__new__ py-dict-new
                     '__repr__ py-dict-repr
                     '__setitem__ dict-set!
                     '__sizeof__ #f
                     'clear #f
                     'copy #f
                     'fromkeys #f
                     'get #f
                     'has_key #f
                     'items py-dict-items
                     'iteritems #f
                     'iterkeys #f
                     'itervalues #f
                     'keys py-dict-keys
                     'pop #f
                     'popitem #f
                     'setdefault #f
                     'update #f
                     'values py-dict-values
                     'viewitems #f
                     'viewkeys #f
                     'viewvalues #f)))
  
  
  (define-syntax-rule (make-py-dict list-of-pairs)
    (make-mutable-python-dict list-of-pairs))
  
  ;;;;;;;;;;
  
  (struct dictproxy_obj python-object
    (original))
  
  (define (make-dictproxy dict)
    (dictproxy_obj py-dictproxy dict))
  
  
  (define (py-dictproxy-getitem self key)
    (if (string? key)
        (hash-ref (dictproxy_obj-original self) (string->symbol key))
        (hash-ref (dictproxy_obj-original self) key)))
  
  (define (py-dictproxy-repr self)
    (let ([reprs (dict-map (dictproxy_obj-original self)
                           (lambda (k v)
                             (format "~a: ~a" (repr (symbol->string k)) (repr v))))])
      (case (length reprs)
        [(0) "dict_proxy({})"]
        [else (string-append "dict_proxy({" 
                             (foldl (lambda (s1 s2) (string-append s2 ", " s1))
                                    (first reprs) (rest reprs))
                             "})")])))
  
  (define (py-dictproxy-str self)
    (let ([reprs (dict-map (dictproxy_obj-original self)
                           (lambda (k v)
                             (format "~a: ~a" (repr (symbol->string k)) (repr v))))])
      (case (length reprs)
        [(0) "dict_proxy({})"]
        [else (string-append "{" 
                             (foldl (lambda (s1 s2) (string-append s2 ", " s1))
                                    (first reprs) (rest reprs))
                             "}")])))
    
  (define py-dictproxy
    (make-type "dictproxy"
               (vector py-object)
               (hasheq '__cmp__ #f
                     '__contains__ #f
                     '__doc__ #f
                     '__eq__ #f
                     '__ge__ #f
                     '__getattribute__ py-object-getattribute
                     '__getitem__ py-dictproxy-getitem
                     '__gt__ #f
                     '__iter__ #f
                     '__le__ #f
                     '__len__ #f
                     '__lt__ #f
                     '__ne__ #f
                     '__repr__ py-dictproxy-repr
                     '__str__ py-dictproxy-str
                     'copy #f
                     'get #f
                     'has_key #f
                     'items #f
                     'iteritems #f
                     'iterkeys #f
                     'itervalues #f
                     'keys #f
                     'values #f)))  
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-custom-set-types python-set
                           py-eq py-hash)
  
  (define (py-set-new typ . args)
    (make-mutable-python-set))
  
  (define (py-set-init! st . args)
    (when (not (empty? args))
      (for ([item (->sequence (first args))])
        (set-add! st item))))
  
  
  (define (py-set-eq? st1 st2)
    (and (python-set? st1) (python-set? st2)
         (set=? st1 st2)))
  
  (define (py-set-iter st)
    (setiterator_obj py-setiterator
                     st (set-count st)
                     (sequence->generator (in-set st))))
  
  (define (py-set-repr st)
    (let ([reprs (set-map st repr)])
      (case (length reprs)
        [(0) "set([])"]
        [else (string-append "set([" 
                             (foldl (lambda (s1 s2) (string-append s2 ", " s1))
                                    (first reprs) (rest reprs))
                             "])")])))
  
  
  (define (copy-and proc!)
    (lambda (x y)
      (let ([copy (set-copy x)])
        (proc! copy y)
        copy)))
  
  
  (define py-set
    (make-type "set"
               (vector py-object)
               (hasheq '__and__ #f
                     '__cmp__ #f
                     '__contains__ set-member?
                     '__doc__ #f
                     '__eq__ py-set-eq?
                     '__ge__ #f
                     '__getattribute__ #f
                     '__gt__ #f
                     '__hash__ #f
                     '__iand__ #f
                     '__init__ py-set-init!
                     '__ior__ #f
                     '__isub__ #f
                     '__iter__ py-set-iter
                     '__ixor__ #f
                     '__le__ #f
                     '__len__ set-count
                     '__lt__ #f
                     '__ne__ #f
                     '__new__ py-set-new
                     '__or__ #f
                     '__rand__ #f
                     '__reduce__ #f
                     '__repr__ py-set-repr
                     '__ror__ #f
                     '__rsub__ #f
                     '__rxor__ #f
                     '__sizeof__ #f
                     '__sub__ (copy-and set-subtract!)
                     '__xor__ #f
                     'add set-add!
                     'clear #f
                     'copy #f
                     'difference #f
                     'difference_update #f
                     'discard #f
                     'intersection #f
                     'intersection_update #f
                     'isdisjoint #f
                     'issubset #f
                     'issuperset #f
                     'pop #f
                     'remove #f
                     'symmetric_difference #f
                     'symmetric_difference_update #f
                     'union #f
                     'update #f)))
  
  ;;;;;  ITERATOR  ;;;;;
  
  (struct setiterator_obj python-object
    (set original-len generator))

  (define (py-setiterator-next self)
    (when (not (= (set-count (setiterator_obj-set self))
                  (setiterator_obj-original-len self)))
      (exn-raise py-RuntimeError "Set changed size during iteration"))
    (let ([result ((setiterator_obj-generator self))])
      (if (void? result)
          (exn-raise py-StopIteration)
          result)))
  
  (define py-setiterator
    (make-type "setiterator"
               (vector py-object)
               (hasheq '__doc__ #f
                     '__getattribute__ #f
                     '__iter__ identity
                     '__length_hint__ #f
                     'next py-setiterator-next)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (->sequence obj)
    (match obj
      [(? list_obj?) obj]
      [(? vector?) obj]
      [(? string?) (sequence-map string obj)]
      [(? dict?) (in-dict-keys obj)]  ;; dict can't change size during iteration
      [(? python-set?) obj]  ;; set can't change size during iteration
      [_ (py-obj->sequence obj)]))
  
  (define (py-obj->sequence pyobj)
    (let* ([it (iter pyobj)]
           [next (mro-lookup it 'next)])
      (in-producer
       (lambda ()
         (with-handlers ([stop-iteration-predicate (const 'over)])
           (next it)))
       'over)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (py-len obj)  ;; optimize for lists
    (py-method-call obj '__len__))
  
  (define (py-get-index obj index)
    (cond
      [(list_obj? obj) (py-list-getitem obj index)]
      [else (py-method-call obj '__getitem__ index)]))
  
  (define (py-set-index! obj index item)
    (cond
      [(list_obj? obj) (py-list-setitem! obj index item)]
      [else (py-method-call obj '__setitem__ index item)]))
  
  (define (py-get-slice obj i j)
    (py-method-call obj '__getslice__ i j))
  
  (define (py-set-slice! obj i j seq)
    (py-method-call obj '__setslice__ i j seq))
  
  )