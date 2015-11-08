#lang sweet-exp typed/racket/base

require reprovide/reprovide

reprovide "chemistry/element-struct.rkt"
          "chemistry/elements.rkt"
          "chemistry/compound.rkt"
          "chemistry/molar-mass.rkt"

module* untyped racket/base
  require reprovide/reprovide
  reprovide (submod typed/measures-with-dimensions/chemistry/element-struct untyped)
            (submod typed/measures-with-dimensions/chemistry/elements untyped)
            typed/measures-with-dimensions/chemistry/compound
            (submod typed/measures-with-dimensions/chemistry/molar-mass untyped)

