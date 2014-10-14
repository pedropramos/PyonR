#lang racket

(require (for-syntax racket/string
                     (only-in srfi/13 string-contains)))
(provide colon-symbol->string
         string->colon-symbol
         mangle-name
         collect-exported
         collect-exported/no-macros
         provide-mangled)


(define (colon-symbol->string sym)
    (let ([str (symbol->string sym)])
      (if (eq? (string-ref str 0) #\:)
          (substring str 1)
          str)))

(define (string->colon-symbol str)
  (string->symbol (string-append ":" str)))

(module auxiliary racket
  
  (define (starts-with? name prefix)
    (string=? (substring name 0 (min (string-length prefix)
                                     (string-length name)))
              prefix))
  
  (define (split-pieces pieces input replacement)
    (flatten
     (for/list ([piece pieces])
       (let* ([splitted (string-split piece input #:trim? #f #:repeat? #f)]
              [interlaced (rest (flatten (map (lambda (piece)
                                                (list replacement piece))
                                              splitted)))]
              [interlaced/no-emptys (filter (lambda (piece)
                                              (> (string-length piece) 0))
                                            interlaced)])
         interlaced/no-emptys))))
  
  (define (mangle-name name)
    (let* ([pieces (list name)]
           [pieces (split-pieces pieces "->" "TO")]
           [pieces (split-pieces pieces "<-" "FROM")]
           [pieces (split-pieces pieces "!" "BANG")]
           [pieces (split-pieces pieces "$" "DOLLAR")]
           [pieces (split-pieces pieces "%" "PERCENT")]
           [pieces (split-pieces pieces "&" "AND")]
           [pieces (split-pieces pieces "*" "STAR")]
           [pieces (split-pieces pieces "+" "PLUS")]
           [pieces (split-pieces pieces "_" "UNDERSCORE")]
           [pieces (split-pieces pieces "." "DOT")]
           [pieces (split-pieces pieces "/" "SLASH")]
           [pieces (split-pieces pieces ":" "COLON")]
           [pieces (split-pieces pieces "<=" "LE")]
           [pieces (split-pieces pieces ">=" "GE")]
           [pieces (split-pieces pieces "<" "LT")]
           [pieces (split-pieces pieces ">" "GT")]
           [pieces (split-pieces pieces "=" "EQUAL")]
           [pieces (split-pieces pieces "?" "QUERY")]
           [pieces (split-pieces pieces "@" "AT")]
           [pieces (split-pieces pieces "^" "CARET")]
           [pieces (split-pieces pieces "~" "TILDE")]
           [pieces (let loop ([minuses empty]
                              [piece (first pieces)]
                              [remaining (rest pieces)])
                     (if (starts-with? piece "-")
                         (loop (cons "MINUS" minuses)
                               (substring piece 1)
                               remaining)
                         (append minuses
                                 (if (= (string-length piece) 0)
                                     empty
                                     (list piece))
                                 remaining)))]
           [pieces (if (char-numeric? (string-ref (first pieces) 0))
                       (cons "" pieces)
                       pieces)])
      (string-replace (string-join pieces "_") "-" "_" #:all? #t)))
  
  (require racket/list)
  
  (define (collect-exported-aux exports)
    (for*/list ([level exports]
                #:when (= (car level) 0)
                [export (cdr level)])
                ;#:when (or (empty? (second export))
                ;           (module-path-index? (first (second export)))))
      (first export)))
    
  (define (collect-exported require-spec)
    (let-values ([(exports-a exports-b) (module->exports require-spec)])
      (remove-duplicates (append (collect-exported-aux exports-a)
                                 (collect-exported-aux exports-b))
                         eq?)))
  
  (define (collect-exported/no-macros require-spec)
    (let-values ([(exports-a exports-b) (module->exports require-spec)])
      (remove-duplicates (collect-exported-aux exports-a) eq?)))
  
  (provide (all-defined-out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'auxiliary)
(require (for-syntax (prefix-in stx: 'auxiliary)))


(define-syntax (provide-mangled stx)
  (syntax-case stx ()
    [(_ require-spec)
     (datum->syntax stx `(provide (rename-out ,@(for/list ([id (stx:collect-exported (syntax->datum #'require-spec))])
                                                  `(,id ,(string->symbol
                                                          (stx:mangle-name 
                                                           (symbol->string id))))))))]))
