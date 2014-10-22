#lang s-exp syntax/module-reader
python/debug/lang/python

#:read python-read
#:read-syntax python-read-syntax
#:info python-get-info
#:language-info '#(python/lang/lang-info get-language-info #f)
#:whole-body-readers? #t

(require "../../lang/reader.rkt")