#lang racket
(require "../../runtime.rkt" "../../lib/__builtin__.py")

(define (cross A B)
  (vector->py-list
   (for*/vector ([a (->sequence A)] [b (->sequence B)])
     (py-add a b))))

(define digits "123456789")
(define rows "ABCDEFGHI")
(define cols digits)
(define squares (cross rows cols))
(define unitlist (py-add (py-add (for/py-list ([c (->sequence cols)])
                                   (cross rows c))
                                 (for/py-list ([r (->sequence rows)])
                                   (cross r cols)))
                         (for*/py-list ([rs (->sequence (make-py-tuple "ABC" "DEF" "GHI"))]
                                        [cs (->sequence (make-py-tuple "123" "456" "789"))])
                           (cross rs cs))))
(define units (py-dict (for/py-list ([s (->sequence squares)])
                       (make-py-tuple s (for/py-list ([u (->sequence unitlist)]
                                                      #:when (py-truth (py-in s u)))
                                          u)))))
(define peers (py-dict (for/py-list ([s (->sequence squares)])
                       (make-py-tuple s (py-sub (py-set (:sum (py-get-index units s) (make-py-list)))
                                                (py-set (make-py-list s)))))))


(define (parse_grid grid)
  (let/ec return
    (let ([vals (py-dict (for/py-list ([s (->sequence squares)])
                           (make-py-tuple s digits)))])
      (for ([pair (->sequence ((py-get-attr (grid_values grid) "items")))])
        (let ([s (py-get-index pair 0)]
              [d (py-get-index pair 1)])
          (when (py-and (py-in d digits)
                        (py-not (assign vals s d)))
            (return py-False))))
      (return vals))))

(define (grid_values grid)
  (let ([chars (for/py-list ([c (->sequence grid)]
                             #:when (py-or (py-in c digits)
                                           (py-in c "0.")))
                 c)])
    (py-dict (:zip squares chars))))

(define (assign vals s d)
  (let ([other_values ((py-get-attr (py-get-index vals s) "replace") d "")])
    (if (for/and ([d2 (->sequence other_values)])
          (py-truth (eliminate vals s d2)))
        vals
        #f)))

(define (eliminate vals s d)
  (let/ec return
    (when (py-not (py-in d (py-get-index vals s)))
      (return vals))
    
    (py-set-index! vals s ((py-get-attr (py-get-index vals s) "replace") d ""))
    (when (py-eq (py-len (py-get-index vals s)) 0)
      (return py-False))
    
    (when (py-eq (py-len (py-get-index vals s)) 1)
      (let ([d2 (py-get-index vals s)])
        (when (py-not (for/and ([s2 (->sequence (py-get-index peers s))])
                        (py-truth (eliminate vals s2 d2))))
          (return py-False))))
    
    (for ([u (->sequence (py-get-index units s))])
      (let ([dplaces (for/py-list ([s (->sequence u)]
                                   #:when (py-in d (py-get-index vals s)))
                       s)])
        (when (py-eq (py-len dplaces) 0)
          (return py-False))
        (when (py-eq (py-len dplaces) 1)
          (when (py-not (assign vals (py-get-index dplaces 0) d))
            (return py-False)))))
    (return vals)))


(define grid1 "003020600900305001001806400008102900700000008006708200002609500800203009005010300")
(time (void (parse_grid grid1)))

;(require racket/trace)
;(trace assign eliminate)