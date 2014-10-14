(module files racket
  (provide (all-defined-out))
  (require "engine.rkt")
  
  
  (struct file_obj python-object
    (port))
  
  (define (wrap-port port)
    (file_obj py-file port))
  
  (define (unwrap-port file)
    (file_obj-port file))
  
  
  (define (py-file-write file data)
    (void (write-string data (unwrap-port file))))
  
  (define (py-file-read file [bytes -1])
    (if (negative? bytes)
        (apply string-append
               (port->list (lambda (in)
                             (read-string 4096 in))
                           (unwrap-port file)))
        (read-string bytes (unwrap-port file))))
         
  
  (define py-file
    (make-type "file"
               (vector py-object)
               (hasheq '__delattr__ #f
                     '__doc__ #f
                     '__enter__ #f
                     '__exit__ #f
                     '__getattribute__ #f
                     '__init__ #f
                     '__iter__ #f
                     '__new__ #f
                     '__repr__ #f
                     '__setattr__ #f
                     'close #f
                     'closed #f
                     'encoding #f
                     'errors #f
                     'fileno #f
                     'flush #f
                     'isatty #f
                     'mode #f
                     'name #f
                     'newlines #f
                     'next #f
                     'read py-file-read
                     'readinto #f
                     'readline #f
                     'readlines #f
                     'seek #f
                     'softspace #f
                     'tell #f
                     'truncate #f
                     'write py-file-write
                     'writelines #f
                     'xreadlines #f)))
  
  )