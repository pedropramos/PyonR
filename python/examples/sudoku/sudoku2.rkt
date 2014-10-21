#lang racket
(require "sudoku_defs.rkt"
         (only-in srfi/13 string-contains))

(define (cross A B)
  (for*/vector ([a A] [b B])
    (string a b)))

(define digits "123456789")
(define rows "ABCDEFGHI")
(define cols digits)
(define squares (cross rows cols))
(define unitlist (vector-append (for/vector ([c cols])
                                  (cross rows (string c)))
                                (for/vector ([r rows])
                                  (cross (string r) cols))
                                (for*/vector ([rs (vector "ABC" "DEF" "GHI")]
                                              [cs (vector "123" "456" "789")])
                                  (cross rs cs))))
(define units (for/hash ([s squares])
                (values s (for/vector ([u unitlist]
                                       #:when (vector-member s u))
                            u))))
(define peers (for/hash ([s squares])
                (values s (set-subtract (sequence->set (for/fold ([sum (vector)])
                                                                 ([vec (hash-ref units s)])
                                                         (vector-append sum vec)))
                                        (set s)))))


(define (parse_grid grid)
  (let/ec return
    (let ([vals (for/mutable-hash ([s squares])
                  (values s digits))])
      (for ([(s d) (grid_values grid)])
        (when (and (string-contains digits (string d)) (not (assign vals s d)))
          (return #f)))
      (return vals))))

(define (grid_values grid)
  (let ([chars (for/vector ([c grid]
                            #:when (or (string-contains digits (string c))
                                       (string-contains "0." (string c))))
                 c)])
    (for/hash ([pair (zip squares chars)])
      (values (vector-ref pair 0)
              (vector-ref pair 1)))))

(define (assign vals s d)
  (let ([other_values (string-replace (hash-ref vals s) (string d) "")])
    (if (for/and ([d2 other_values])
          (eliminate vals s d2))
        vals
        #f)))

(define (eliminate vals s d)
  (let/ec return
    (when (not (string-contains (hash-ref vals s) (string d)))
      (return vals))
    (hash-set! vals s (string-replace (hash-ref vals s) (string d) ""))
    (when (= (string-length (hash-ref vals s)) 0)
      (return #f))
    (when (= (string-length (hash-ref vals s)) 1)
      (let ([d2 (string-ref (hash-ref vals s) 0)])
        (when (not (for/and ([s2 (hash-ref peers s)])
                     (eliminate vals s2 d2)))
          (return #f))))
    (for ([u (hash-ref units s)])
      (let ([dplaces (for/vector ([s u]
                                  #:when (string-contains (hash-ref vals s)
                                                          (string d)))
                       s)])
        (when (= (vector-length dplaces) 0)
          (return #f))
        (when (= (vector-length dplaces) 1)
          (when (not (assign vals (vector-ref dplaces 0) d))
            (return #f)))))
    (return values)))



(define grid1 "003020600900305001001806400008102900700000008006708200002609500800203009005010300")

(time (void (parse_grid grid1)))

;(require racket/trace)
;(trace assign eliminate)