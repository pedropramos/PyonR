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
  (let ([vals (for/mutable-hash ([s squares])
                  (values s digits))])
    (and (for/and ([(s d) (grid_values grid)])
           (if (and (string-contains digits d) (not (assign vals s d)))
               #f
               #t))
         vals)))

(define (grid_values grid)
  (let ([chars (for/vector ([c grid]
                            #:when (or (string-contains digits (string c))
                                       (string-contains "0." (string c))))
                 (string c))])
    (for/hash ([pair (zip squares chars)])
      (values (vector-ref pair 0)
              (vector-ref pair 1)))))

(define (assign vals s d)
  (let ([other_values (string-replace (hash-ref vals s) d "")])
    (if (for/and ([d2 other_values])
          (eliminate vals s (string d2)))
        vals
        #f)))

(define (eliminate vals s d)
  ;(define sd (string d))
  (if (not (string-contains (hash-ref vals s) d))
      vals
      (begin
        (hash-set! vals s (string-replace (hash-ref vals s) d ""))
        (if (= (string-length (hash-ref vals s)) 0)
            #f
            (if (and (= (string-length (hash-ref vals s)) 1)
                     (let ([d2 (hash-ref vals s)])
                       (not (for/and ([s2 (hash-ref peers s)])
                              (eliminate vals s2 d2)))))
                #f
                (and (for/and ([u (hash-ref units s)])
                       (let ([dplaces (for/vector ([s u]
                                                   #:when (string-contains (hash-ref vals s) d))
                                        s)])
                         (if (= (vector-length dplaces) 0)
                             #f
                             (if (and (= (vector-length dplaces) 1)
                                      (not (assign vals (vector-ref dplaces 0) d)))
                                 #f
                                 #t))))
                     vals))))))



(define grid1 "003020600900305001001806400008102900700000008006708200002609500800203009005010300")
(define grid2 "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......")

;(time (void (parse_grid grid1)))
(time (void (for ([t 1000]) (parse_grid grid2))))

;(require racket/trace)
;(trace assign eliminate)