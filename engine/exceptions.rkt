(module exceptions racket
  (provide (all-defined-out))
  (require "engine.rkt")
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; BaseException
  ;;  +-- SystemExit
  ;;  +-- KeyboardInterrupt
  ;;  +-- GeneratorExit
  ;;  +-- Exception
  ;;       +-- StopIteration
  ;;       +-- StandardError
  ;;       |    +-- BufferError
  ;;       |    +-- ArithmeticError
  ;;       |    |    +-- FloatingPointError
  ;;       |    |    +-- OverflowError
  ;;       |    |    +-- ZeroDivisionError
  ;;       |    +-- AssertionError
  ;;       |    +-- AttributeError
  ;;       |    +-- EnvironmentError
  ;;       |    |    +-- IOError
  ;;       |    |    +-- OSError
  ;;       |    |         +-- WindowsError (Windows)
  ;;       |    |         +-- VMSError (VMS)
  ;;       |    +-- EOFError
  ;;       |    +-- ImportError
  ;;       |    +-- LookupError
  ;;       |    |    +-- IndexError
  ;;       |    |    +-- KeyError
  ;;       |    +-- MemoryError
  ;;       |    +-- NameError
  ;;       |    |    +-- UnboundLocalError
  ;;       |    +-- ReferenceError
  ;;       |    +-- RuntimeError
  ;;       |    |    +-- NotImplementedError
  ;;       |    +-- SyntaxError
  ;;       |    |    +-- IndentationError
  ;;       |    |         +-- TabError
  ;;       |    +-- SystemError
  ;;       |    +-- TypeError
  ;;       |    +-- ValueError
  ;;       |         +-- UnicodeError
  ;;       |              +-- UnicodeDecodeError
  ;;       |              +-- UnicodeEncodeError
  ;;       |              +-- UnicodeTranslateError
  ;;       +-- Warning
  ;;            +-- DeprecationWarning
  ;;            +-- PendingDeprecationWarning
  ;;            +-- RuntimeWarning
  ;;            +-- SyntaxWarning
  ;;            +-- UserWarning
  ;;            +-- FutureWarning
  ;; 	     +-- ImportWarning
  ;; 	     +-- UnicodeWarning
  ;; 	     +-- BytesWarning
  ;;  
  ;;
  ;; https://docs.python.org/2/library/exceptions.html#exception-hierarchy
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  ;; 2 representations for exceptions: exn? and exception_obj?
  (struct exception_obj python-object
    (args message continuation-marks)
    #:mutable
    #:transparent)
  
  (define (make-py-exception typ . args)
    (exception_obj typ
                   (list->vector args)
                   (if (= (length args) 1)
                       (first args)
                       "")
                   (current-continuation-marks)))
  
  (define-syntax (exn-raise stx)
    (syntax-case stx ()
      [(_ typ msg params ...)
       #'(raise (make-py-exception typ (format msg params ...)))]
      [(_ typ)
       #'(raise (make-py-exception typ))]))

  
  
  
  (define (py-exception-new typ . args)
    (exception_obj typ (vector) "" (current-continuation-marks)))
  
  (define (py-exception-init exc . args)
    (set-exception_obj-args! exc (list->vector args))
    (when (= (length args) 1)
      (set-exception_obj-message! exc (first args))))
  
  
  (define (py-exception-get-args exc)
    (if (exn? exc)
        (vector (exn-message exc))
        (exception_obj-args exc)))
  
  (define (py-exception-set-args! exc args)
    (if (exn? exc)
        (error "racket exceptions are not mutable")
        (set-exception_obj-args! exc args)))
  
  (define (py-exception-get-message exc)
    (if (exn? exc)
        (exn-message exc)
        (exception_obj-message exc)))
  
  (define (py-exception-set-message! exc message)
    (if (exn? exc)
        (error "racket exceptions are not mutable")
        (set-exception_obj-message! exc message)))
  
  
  (define py-BaseException
    (make-type "BaseException"
               (vector py-object)
               (hasheq '__delattr__ #f
                     '__dict__ #f
                     '__doc__ (void)
                     '__getattribute__ #f
                     '__getitem__ #f
                     '__getslice__ #f
                     '__init__ py-exception-init
                     '__new__ py-exception-new
                     '__reduce__ #f
                     '__repr__ #f
                     '__setattr__ #f
                     '__setstate__ #f
                     '__str__ #f
                     '__unicode__ #f
                     'args (make-getset py-exception-get-args
                                         py-exception-set-args!)
                     'message (make-getset py-exception-get-message
                                            py-exception-set-message!))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
  (define (error-format typ msg)
    (if (zero? (string-length (string-trim msg)))
        (format "~a" typ)
        (format "~a: ~a" typ msg)))
  
  (define python-error-display-handler
    (let ([oedh (error-display-handler)]) ; original-error-display-handler
      (lambda (msg e)
        (cond
          [(exception_obj? e)
           (oedh (error-format
                  (type_obj-name (python-object-type e))
                  (print-repr (exception_obj-message e)))
                 (exn:fail (print-repr (exception_obj-message e))
                           (exception_obj-continuation-marks e)))]
          
          [(exn? e)
           (oedh (error-format
                  (type_obj-name (racket-exception-type e))
                  (exn-message e))
                 (exn:fail (exn-message e)
                           (exn-continuation-marks e)))]
          
          [else
           (oedh msg e)]))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (require (for-syntax racket/syntax))
  
  (define-syntax (define-exception-type stx)
    (syntax-case stx ()
      [(_ typ base)
       (with-syntax ([typ-name (datum->syntax #'ctxt
                                (symbol->string
                                 (syntax->datum #'typ)))]
                     [py-typ (format-id #'typ #:source #'typ
                                        "py-~a" (syntax-e #'typ))]
                     [py-base (format-id #'base #:source #'base
                                         "py-~a" (syntax-e #'base))])
         #'(define py-typ
             (make-type typ-name
                        (vector py-base)
                        (hasheq '__doc__ (void)
                              '__init__ py-exception-init
                              '__new__ py-exception-new))))]))
  
  (define-syntax-rule (define-exn-hierarchy base (subtype ...))
    (begin
      (define-exception-type subtype base)
      ...))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-exn-hierarchy BaseException
    (SystemExit KeyboardInterrupt GeneratorExit Exception))
  (define-exn-hierarchy Exception
    (StopIteration StandardError Warning RacketException))
  (define-exn-hierarchy StandardError
    (BufferError ArithmeticError AssertionError AttributeError EnvironmentError
     EOFError ImportError LookupError MemoryError NameError ReferenceError
     RuntimeError SyntaxError SystemError TypeError ValueError))
  (define-exn-hierarchy ArithmeticError
    (FloatingPointError OverflowError ZeroDivisionError))
  (define-exn-hierarchy EnvironmentError
    (IOError OSError))
  (define-exn-hierarchy OSError
    (WindowsError VMSError))
  (define-exn-hierarchy LookupError
    (IndexError KeyError))
  (define-exn-hierarchy NameError
    (UnboundLocalError))
  (define-exn-hierarchy RuntimeError
    (NotImplementedError))
  (define-exn-hierarchy SyntaxError
    (IndentationError))
  (define-exn-hierarchy IndentationError
    (TabError))
  (define-exn-hierarchy ValueError
    (UnicodeError))
  (define-exn-hierarchy UnicodeError
    (UnicodeDecodeError UnicodeEncodeError UnicodeTranslateError))
  (define-exn-hierarchy Warning
    (DeprecationWarning PendingDeprecationWarning RuntimeWarning SyntaxWarning
     UserWarning FutureWarning ImportWarning UnicodeWarning BytesWarning))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (require (only-in srfi/13 string-contains))
  (define (racket-exception-type x)
    (cond
      [(exn:fail:syntax? x) py-SyntaxError]
      [(exn:fail:contract:divide-by-zero? x) py-ZeroDivisionError]
      [(and (exn:fail:contract? x)
            (string-contains (exn-message x) "index is out of range")) py-IndexError]
      [else py-RacketException]))
  
  ;; used for exception handling
  (define stop-iteration-predicate (type-predicate py-StopIteration))
  
  )