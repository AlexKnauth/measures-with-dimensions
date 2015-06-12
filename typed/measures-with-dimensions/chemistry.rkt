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
            (submod typed/measures-with-dimensions/chemistry/element-struct untyped)
            (submod typed/measures-with-dimensions/chemistry/elements untyped)
            typed/measures-with-dimensions/chemistry/compound
            (submod typed/measures-with-dimensions/chemistry/molar-mass untyped)
            ))
  (require (submod typed/measures-with-dimensions/chemistry/element-struct untyped)
           (submod typed/measures-with-dimensions/chemistry/elements untyped)
           typed/measures-with-dimensions/chemistry/compound
           (submod typed/measures-with-dimensions/chemistry/molar-mass untyped)
           )
  )
