#lang racket
(require "../runtime.rkt")
(provide (rename-out [py-type :type]
                     [py-object :object]
                     [py-list :list]
                     [py-tuple :tuple]
                     [py-dict :dict]
                     [py-set :set]
                     [py-string :str]
                     
                     [py-property :property]
                     
                     [name-of-module :__name__]
                     
                     [py-all :all]
                     [py-eval :eval]
                     ;[py-exec :exec]
                     [py-chr :chr]
                     [py-isinstance? :isinstance]
                     [py-len :len]
                     [py-map :map]
                     [py-ord :ord]
                     [py-range :range]
                     [py-raw-input :raw_input]
                     [py-sum :sum]
                     [py-xrange :xrange]
                     [py-zip :zip]
                     
                     [py-True :True]
                     [py-False :False]
                     [py-None :None]
                     
                     [py-BaseException :BaseException]
                     [py-Exception :Exception]
                     [py-StandardError :StandardError]
                     [py-IndexError :IndexError]
                     [py-ZeroDivisionError :ZeroDivisionError]
                     [py-NameError :NameError]
                     [py-TypeError :TypeError]
                     [py-ValueError :ValueError]))


(define (py-sum iterable [start 0])
  (for ([item (->sequence iterable)])
    (set! start (py-add start item)))
  start)

(define (py-all iterable)
  (let* ([it (iter iterable)]
         [next (mro-lookup it 'next)])
    (with-handlers ([stop-iteration-predicate (const py-True)])
      (let loop ()
        (let ([item (next it)])
          (if (not (py-truth item))
              py-False
              (loop)))))))

(define (py-range . args)
  (vector->py-list
   (for/vector ([item (apply in-range args)]) item)))

(require racket/generator)
(define (py-xrange . args)
  (make-py-generator (sequence->generator (apply in-range args))))


(define (py-zip . iterables)
  (let* ([tuples-len (length iterables)] 
         [its (map iter iterables)]
         [nexts (map (lambda (it) (mro-lookup it 'next)) its)]
         [result (make-py-list)])
    (with-handlers ([stop-iteration-predicate (const result)])
      (let loop ()
        (py-list-append! result
                         (for/vector #:length tuples-len ([it its] [next nexts])
                           (next it)))
        (loop)))))

(define (py-raw-input [prompt ""])
  (display prompt)
  (let ([s (read-line)])
    (if (eof-object? s)
        (exn-raise py-EOFError "EOF when reading a line")
        s)))


(define (py-isinstance? object classinfo)
  (not (not (is-subtype? (type object) classinfo))))

(define (py-map fun iterable)
  (vector->py-list
   (for/vector ([obj (->sequence iterable)])
     (fun obj))))

(define (py-chr i)
  (string (integer->char i)))

(define (py-ord s)
  (if (= (string-length s) 1)
      (char->integer (string-ref s 0))
      (exn-raise py-TypeError "ord() expected a character, but string of length ~a found" (string-length s))))