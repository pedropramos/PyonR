(module ast-node racket/base
  (require racket/class
           racket/match
           parser-tools/lex
           syntax/readerr)
           
  (provide (except-out (all-defined-out) stx-orig-prop))
  
  
  ;; syntatic properties for making syntax object look like they appeared in the source text)
  (define stx-orig-prop (read-syntax #f (open-input-string "orig")))
  
  ;; parameter for determining the target environment for compilation
  ;;; can be 'module or 'repl
  ;;; used for removing provides from code for REPL
  ;;;;;;;;;;; (define target-env (make-parameter 'module))   - may not be needed after all
  
  
  ;; simplification of (send obj to-racket)
  ;;; used on map
  (define (send-to-racket obj . args)
    (apply dynamic-send (append (list obj) args (list 'to-racket))))
  
  
  ;; used to compile definitions for bindings on top-level
  (define (compile-top-definitions scope)
    (for/list ([id (send scope get-bindings)])
      (let ([id-stx (send id to-racket)])
        (datum->syntax id-stx
                       `(py-assign! ,id-stx undefined)
                       id-stx
                       id-stx))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  (define ast-node%
    (class object%
      (init-field start-pos end-pos)
      
      (define src-loc
        (list (file-path)
              (and start-pos (position-line start-pos))
              (and start-pos (position-col start-pos))
              (and start-pos (position-offset start-pos))
              (and start-pos (- (position-offset end-pos)
                                (position-offset start-pos)))))
      
      ;; stx-err: string? ->
      ;; raises an exception with the source of the expression
      (define/public (stx-err msg)
        (apply raise-read-error (cons msg src-loc)))

      ;; ->orig-so: datum? -> syntax-object?
      ;; converts the datum into a syntax-object using the source
      ;; location of the expression.  Uses the stx-orig-prop
      ;; to make the syntax object look like it appeared in the
      ;; source text.  This way check syntax will highlight it.
      (define/public (->orig-so datum)
        (datum->syntax #'here
                       datum
                       src-loc
                       stx-orig-prop))
      
      ;; ->lex-so: datum context -> syntax-object
      ;; same as ->orig-so, but with lexical information
      (define/public (->lex-so datum context)
        (datum->syntax context datum src-loc stx-orig-prop))

      ;; set-bindings!: (or/f fales? (is-a?/c def%) (is-a?/c class%) (is-a?/c lambda%)) ->
      ;; Compute all the bindings this statement could introduce into
      ;; its surrounding scope, assuming that the statement is contained
      ;; inside of some lexical scope.  Scopes are created by lambda, def
      ;; and class.
      (define/public (set-bindings! enclosing-scope) null)

      ;; ->so: datum? -> syntax-object?
      ;; Like ->orig-so, but doesn't set the property.
      (define/public (->so datum)
        (datum->syntax #f datum src-loc))
      
      
      
      (define/public (get-start-pos) start-pos)
      (define/public (get-end-pos) end-pos)
      
      (define/public (to-description)
        (stx-err (format "Invalid usage of to-description on an ast-node% (I'm purely virtual) ~a" this)))
      
      
      ;; to-racket: -> syntax-object?
      (define/public (to-racket)
        (stx-err (format "Invalid usage of to-racket on an ast-node% (I'm purely virtual) ~a" this)))
      
      (super-instantiate ())))

  ;; utility functions....
  
  ;; first-atom: sxp -> atom
  ;; if sxp is a list, find the first item in it (or its inner lists) that isn't a list
  (define (first-atom s)
    (match s
     [(list-rest item items)
      (first-atom item)]
     [else s]))

  )
