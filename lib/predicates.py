#lang racket
(require "../runtime.rkt")
(provide (rename-out [set-custom-type! :set_predicate]
                     [remove-custom-type! :remove_predicate]
                     [set-pred-subtype! :define_subtype]))