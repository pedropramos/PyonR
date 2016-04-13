(module libpython racket
  
  (require ffi/unsafe
           ffi/unsafe/define
           ffi/unsafe/alloc
           "paths.rkt"
           racket/system
           (for-syntax racket/syntax)
           (only-in racket/port with-output-to-string))
  
  (provide (except-out (all-defined-out)
                       define-function define-others define-c-lang
                       ffi ffi-others ffi-c-lang))
  
  
  (define (find-first-path . paths)
    (for/or ([path paths])
      (and (file-exists? path) path)))
  
  (define path-to-cpython-lib
    (case (system-type 'os)
      [(windows) (find-first-path "C:/Windows/SysWOW64/python27.dll"
                                  "C:/Windows/System32/python27.dll")]
      [(unix) (find-first-path (car (regexp-split #px"\n" (with-output-to-string
                                                           (λ () (system "find /usr/lib/ -name libpython2.7.so")))))) ]
      [(macosx) (find-first-path "/usr/lib/libpython2.7.dylib"
                                 "/usr/local/lib/libpython2.7.dylib")]))
  
  (define path-to-others-lib
    (and path-to-cpython-lib
         (let ([file-suffix (number->string (system-type 'word))]
               [file-ext (bytes->string/locale (system-type 'so-suffix))])
           (simplify-path (build-path python-root "c" (string-append "others" file-suffix file-ext))))))
  
  (define cpyimport-enabled
    (let* ([file (open-input-file path-to-CPYIMPORT)]
           [line (read-line file)])
      (string=? line "ON")))

    
  (define-ffi-definer define-function (ffi-lib (and cpyimport-enabled path-to-cpython-lib))
    #:default-make-fail void-if-not-available)
  (define-ffi-definer define-others (ffi-lib (and cpyimport-enabled path-to-others-lib))
    #:default-make-fail void-if-not-available)
  (define-ffi-definer define-c-lang (ffi-lib #f)
    #:default-make-fail void-if-not-available)
  
  (define (void-if-not-available id)
    (lambda () void))
    
  
  
  (define-for-syntax (underscore stx)
    (format-id stx "_~a" stx))
  
  (define-for-syntax (leak stx)
    (format-id stx "~a_LEAK" stx))
  
  (define-syntax (ffi-generic stx)
    (syntax-case stx ()
      [(_ definer result id (args ...))
       (with-syntax ([_result (underscore #'result)]
                     [(_args ...) (map underscore (syntax->list #'(args ...)))])
         #'(definer id (_fun _args ... -> _result)))]))
  
  (define-syntax-rule (ffi args ...) (ffi-generic define-function args ...))
  (define-syntax-rule (ffi-others args ...) (ffi-generic define-others args ...))
  (define-syntax-rule (ffi-c-lang args ...) (ffi-generic define-c-lang args ...))
  
  (define-syntax (cstruct stx)
    (syntax-case stx ()
      [(_ struct-id ([id type] ...))
       (with-syntax ([_struct-id (underscore #'struct-id)]
                     [(_type ...) (map underscore (syntax->list #'(type ...)))]
                     [_struct-id* (format-id #'struct-id "_~a*" #'struct-id)]
                     [_struct-id-pointer (format-id #'struct-id "_~a-pointer" #'struct-id)])
         #'(begin
             (define-cstruct _struct-id ([id _type] ...))
             (define _struct-id* _struct-id-pointer)))]))
  
  ; with allocator
  (define-syntax (ffi-gc stx)
    (syntax-case stx ()
      [(_ result id (args ...))
       (with-syntax ([id_LEAK (leak #'id)]
                     [_result (underscore #'result)]
                     [(_args ...) (map underscore (syntax->list #'(args ...)))])
         #'(begin 
             (define-function id_LEAK (_fun _args ... -> _result) #:c-id id)
             (define id ((allocator Py_XDECREF) id_LEAK))))]))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (cpointer-id x) (cast x _pointer _Py_ssize_t))
  
  (define _void* (_cpointer 'void))
  (define _FILE* (_cpointer 'FILE))
  (define _int* (_cpointer 'int))
  (define _string* (_cpointer 'string))
  (define _PyObject* (_cpointer/null 'PyObject))
  (define _PyObject** (_cpointer 'PyObject*))
  (define _PyTypeObject* (_cpointer 'PyTypeObject))
  (define _Py_ssize_t (case (system-type 'word)
                        [(32) _int32]
                        [(64) _int64]))
  
  ; FILE* functions
  (ffi-c-lang FILE* fopen (string string))
  (ffi-c-lang int fprintf (FILE* string))
  (ffi-c-lang int fflush (FILE*))
  (ffi-c-lang int fclose (FILE*))
  (ffi-others FILE* get_stdin ())
  (ffi-others FILE* get_stdout ())
  (ffi-others FILE* get_stderr ())
  
  ;; Embedding Python
  (ffi void Py_Initialize ())
  (ffi void Py_Finalize ())
  

  ;; Reference Counting
  (ffi-others void Py_INCREF (PyObject*))
  (ffi-others void Py_XINCREF (PyObject*))
  (ffi-others void Py_DECREF (PyObject*))
  (ffi-others void Py_XDECREF (PyObject*))
  (ffi-others void Py_CLEAR (PyObject*))
  
  
  ;; Numbers
  (ffi PyObject* PyInt_FromLong (long))
  (ffi long PyInt_AsLong (PyObject*))
  
  (ffi PyObject* PyLong_FromLong (long))
  (ffi PyObject* PyLong_FromString (string int int))
  (ffi long PyLong_AsLong (PyObject*))
  
  (ffi PyObject* PyFloat_FromDouble (double))
  (ffi double PyFloat_AsDouble (PyObject*))
  
  (cstruct Py_complex ([real double]
                       [imag double]))
  (ffi PyObject* PyComplex_FromCComplex (Py_complex))
  (ffi Py_complex PyComplex_AsCComplex (PyObject*))
  
  
  ;; Booleans
  (ffi PyObject* PyBool_FromLong (long))
  
  
  ;; Strings
  (ffi PyObject* PyString_FromString (string))
  (ffi string PyString_AsString (PyObject*))
  
  
  ;; Objects
  (ffi int PyObject_Print (PyObject* FILE* int))
  (ffi int PyObject_HasAttr (PyObject* PyObject*))
  (ffi int PyObject_HasAttrString (PyObject*  string))
  (ffi PyObject* PyObject_GetAttr (PyObject* PyObject*))
  (ffi PyObject* PyObject_GetAttrString (PyObject*  string))
  (ffi PyObject* PyObject_GenericGetAttr (PyObject* PyObject*))
  (ffi int PyObject_SetAttr (PyObject* PyObject* PyObject*))
  (ffi int PyObject_SetAttrString (PyObject*  string PyObject*))
  (ffi int PyObject_GenericSetAttr (PyObject* PyObject* PyObject*))
  ;(ffi int PyObject_DelAttr (PyObject* PyObject*))
  ;(ffi int PyObject_DelAttrString (PyObject*  string))
  (ffi PyObject* PyObject_RichCompare (PyObject* PyObject* int))
  (ffi int PyObject_RichCompareBool (PyObject* PyObject* int))
  (ffi int PyObject_Cmp (PyObject* PyObject* int*))
  (ffi int PyObject_Compare (PyObject* PyObject*))
  (ffi PyObject* PyObject_Repr (PyObject*))
  (ffi PyObject* PyObject_Str (PyObject*))
  ;(ffi PyObject* PyObject_Bytes (PyObject*))
  (ffi PyObject* PyObject_Unicode (PyObject*))
  (ffi int PyObject_IsInstance (PyObject* PyObject*))
  (ffi int PyObject_IsSubclass (PyObject* PyObject*))
  (ffi int PyCallable_Check (PyObject*))
  (ffi PyObject* PyObject_Call (PyObject* PyObject* PyObject*))
  (ffi PyObject* PyObject_CallObject (PyObject* PyObject*))
  (ffi long PyObject_Hash (PyObject*))
  (ffi long PyObject_HashNotImplemented (PyObject*))
  (ffi int PyObject_IsTrue (PyObject*))
  (ffi int PyObject_Not (PyObject*))
  (ffi PyObject* PyObject_Type (PyObject*))
  ;(ffi int PyObject_TypeCheck (PyObject* PyTypeObject*))
  (ffi Py_ssize_t PyObject_Length (PyObject*))
  (ffi Py_ssize_t PyObject_Size (PyObject*))
  (ffi PyObject* PyObject_GetItem (PyObject* PyObject*))
  (ffi int PyObject_SetItem (PyObject* PyObject* PyObject*))
  (ffi int PyObject_DelItem (PyObject* PyObject*))
  (ffi int PyObject_AsFileDescriptor (PyObject*))
  (ffi PyObject* PyObject_Dir (PyObject*))
  (ffi PyObject* PyObject_GetIter (PyObject*))
  
  (ffi-others PyObject* get_PyNone ())
  
  ;; Sequences
  (ffi int PySequence_Check (PyObject*))
  (ffi Py_ssize_t PySequence_Size (PyObject*))
  (ffi Py_ssize_t PySequence_Length (PyObject*))
  (ffi PyObject* PySequence_Concat (PyObject* PyObject*))
  (ffi PyObject* PySequence_Repeat (PyObject* Py_ssize_t))
  (ffi PyObject* PySequence_InPlaceConcat (PyObject* PyObject*))
  (ffi PyObject* PySequence_InPlaceRepeat (PyObject* Py_ssize_t))
  (ffi PyObject* PySequence_GetItem (PyObject* Py_ssize_t))
  (ffi PyObject* PySequence_GetSlice (PyObject* Py_ssize_t Py_ssize_t))
  (ffi int PySequence_SetItem (PyObject* Py_ssize_t PyObject*))
  (ffi int PySequence_DelItem (PyObject* Py_ssize_t))
  (ffi int PySequence_SetSlice (PyObject* Py_ssize_t Py_ssize_t PyObject*))
  (ffi int PySequence_DelSlice (PyObject* Py_ssize_t Py_ssize_t))
  (ffi Py_ssize_t PySequence_Count (PyObject* PyObject*))
  (ffi int PySequence_Contains (PyObject* PyObject*))
  (ffi Py_ssize_t PySequence_Index (PyObject* PyObject*))
  (ffi PyObject* PySequence_List (PyObject*))
  (ffi PyObject* PySequence_Tuple (PyObject*))
  (ffi PyObject* PySequence_Fast (PyObject* string))
  ;(ffi PyObject* PySequence_Fast_GET_ITEM (PyObject* Py_ssize_t))
  ;(ffi PyObject** PySequence_Fast_ITEMS (PyObject*))
  ;(ffi PyObject* PySequence_ITEM (PyObject* Py_ssize_t))
  ;(ffi Py_ssize_t PySequence_Fast_GET_SIZE (PyObject*))
  
  
  ;; Tuples
  (ffi PyObject* PyTuple_New (Py_ssize_t))
  (ffi PyObject* PyTuple_GetItem (PyObject* Py_ssize_t))
  (ffi void PyTuple_SetItem (PyObject* Py_ssize_t PyObject*))
  (ffi Py_ssize_t PyTuple_Size (PyObject*))
  
  ;; Lists
  (ffi PyObject* PyList_New (Py_ssize_t))
  (ffi void PyList_SetItem (PyObject* Py_ssize_t PyObject*))
  (ffi PyObject* PyList_GetItem (PyObject* Py_ssize_t))
  (ffi Py_ssize_t PyList_Size (PyObject*))
  
  ;; Dicts
  (ffi PyObject* PyDict_Keys (PyObject*))
  (ffi PyObject* PyDict_GetItem (PyObject* PyObject*))
  
  ;; Modules
  (ffi PyObject* PyModule_GetDict (PyObject*))
  
  ;; Imports
  (ffi PyObject* PyImport_Import (PyObject*))
  
  ;; Types
  (define (PyType_Dict type)
    (ptr-ref (ptr-add type (* 33 (ctype-sizeof _pointer))) _PyObject*)) ;; dirty hack
  
  ;; Exceptions
;  (ffi void PyErr_PrintEx (int))
;  (ffi void PyErr_Print ())
;  (ffi PyObject* PyErr_Occurred ())
;  (ffi int PyErr_ExceptionMatches (PyObject*))
;  (ffi int PyErr_GivenExceptionMatches (PyObject* PyObject*))
;  (ffi void PyErr_NormalizeException (PyObject** PyObject** PyObject**))
;  (ffi void PyErr_Clear ())
;  ;(ffi void PyErr_Fetch (PyObject** PyObject** PyObject**))
;  (ffi void PyErr_Restore (PyObject* PyObject* PyObject*))
;  (ffi void PyErr_SetString (PyObject* string))
;  (ffi void PyErr_SetObject (PyObject* PyObject*))
;  ;(ffi PyObject* PyErr_Format (PyObject* string ...))
;  (ffi void PyErr_SetNone (PyObject*))
;  (ffi int PyErr_BadArgument ())
;  (ffi PyObject* PyErr_NoMemory ())
;  (ffi PyObject* PyErr_SetFromErrno (PyObject*))
;  (ffi PyObject* PyErr_SetFromErrnoWithFilenameObject (PyObject* PyObject*))
;  (ffi PyObject* PyErr_SetFromErrnoWithFilename (PyObject* string))
;  (ffi PyObject* PyErr_SetFromWindowsErr (int))
;  (ffi PyObject* PyErr_SetExcFromWindowsErr (PyObject* int))
;  ;(ffi PyObject* PyErr_SetFromWindowsErrWithFilenameObject (int PyObject*))
;  (ffi PyObject* PyErr_SetFromWindowsErrWithFilename (int string))
;  (ffi PyObject* PyErr_SetExcFromWindowsErrWithFilenameObject (PyObject* int PyObject*))
;  (ffi PyObject* PyErr_SetExcFromWindowsErrWithFilename (PyObject* int string))
;  (ffi void PyErr_BadInternalCall ())
;  (ffi int PyErr_WarnEx (PyObject* string int))
;  (ffi int PyErr_Warn (PyObject* string))
;  (ffi int PyErr_WarnExplicit (PyObject* string string int string PyObject*))
;  ;(ffi int PyErr_WarnPy3k (string int))
;  (ffi int PyErr_CheckSignals ())
;  (ffi void PyErr_SetInterrupt ())
;  ;(ffi int PySignal_SetWakeupFd (int))
;  (ffi PyObject* PyErr_NewException (string PyObject* PyObject*))
;  (ffi PyObject* PyErr_NewExceptionWithDoc (string string PyObject* PyObject*))
;  (ffi void PyErr_WriteUnraisable (PyObject*))
  
  (define-function PyErr_Fetch
    (_fun (a : (_ptr o _PyObject*))
          (b : (_ptr o _PyObject*))
          (c : (_ptr o _PyObject*))
          -> _void -> (list a b c)))
  
  (define-function PyErr_NormalizeException
    (_fun (a : (_ptr io _PyObject*))
          (b : (_ptr io _PyObject*))
          (c : (_ptr io _PyObject*))
          -> _void -> (list a b c)))
  
  
  
  ;;;;;;;;;;;;
  (Py_Finalize)
  (Py_Initialize)
  
  )