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

(module* untyped racket/base
  (provide (all-from-out
            (submod "chemistry/element-struct.rkt" untyped)
            (submod "chemistry/elements.rkt" untyped)
            "chemistry/compound.rkt"
            (submod "chemistry/molar-mass.rkt" untyped)
            ))
  (require (submod "chemistry/element-struct.rkt" untyped)
           (submod "chemistry/elements.rkt" untyped)
           "chemistry/compound.rkt"
           (submod "chemistry/molar-mass.rkt" untyped)
           )
  )
