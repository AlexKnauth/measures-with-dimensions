#lang typed/racket/base

(provide (all-from-out "chemistry/element-struct.rkt"
                       "chemistry/elements.rkt"
                       "chemistry/compound.rkt"
                       "chemistry/molar-mass.rkt"
                       ))

(require "chemistry/element-struct.rkt"
         "chemistry/elements.rkt"
         "chemistry/compound.rkt"
         "chemistry/molar-mass.rkt"
         "untyped-utils.rkt"
         )

(untyped-module*
 [#:all-from (submod "chemistry/element-struct.rkt" untyped)
             (submod "chemistry/elements.rkt" untyped)
             "chemistry/compound.rkt"
             (submod "chemistry/molar-mass.rkt" untyped)
             ])
