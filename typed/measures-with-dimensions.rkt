#lang typed/racket/base

(provide (all-from-out "measures-with-dimensions/dimension-struct.rkt"
                       "measures-with-dimensions/dimension-operations.rkt"
                       "measures-with-dimensions/dimensions.rkt"
                       "measures-with-dimensions/unit-struct.rkt"
                       "measures-with-dimensions/unit-operations.rkt"
                       "measures-with-dimensions/units.rkt"
                       "measures-with-dimensions/measure-struct.rkt"
                       "measures-with-dimensions/physical-constants.rkt"
                       "measures-with-dimensions/measure-types.rkt"
                       "measures-with-dimensions/typed-operations.rkt"
                       "measures-with-dimensions/temperature.rkt"
                       ))

(require "measures-with-dimensions/dimension-struct.rkt"
         "measures-with-dimensions/dimension-operations.rkt"
         "measures-with-dimensions/dimensions.rkt"
         "measures-with-dimensions/unit-struct.rkt"
         "measures-with-dimensions/unit-operations.rkt"
         "measures-with-dimensions/units.rkt"
         "measures-with-dimensions/measure-struct.rkt"
         "measures-with-dimensions/physical-constants.rkt"
         "measures-with-dimensions/measure-types.rkt"
         "measures-with-dimensions/typed-operations.rkt"
         "measures-with-dimensions/temperature.rkt"
         )

(module* untyped racket/base
  (provide (all-from-out
            (submod "measures-with-dimensions/dimension-struct.rkt" untyped)
            (submod "measures-with-dimensions/dimension-operations.rkt" untyped)
            (submod "measures-with-dimensions/dimensions.rkt" untyped)
            (submod "measures-with-dimensions/unit-struct.rkt" untyped)
            (submod "measures-with-dimensions/unit-operations.rkt" untyped)
            (submod "measures-with-dimensions/units.rkt" untyped)
            (submod "measures-with-dimensions/measure-struct.rkt" untyped)
            (submod "measures-with-dimensions/physical-constants.rkt" untyped)
            (submod "measures-with-dimensions/measure-types.rkt" untyped)
            (submod "measures-with-dimensions/typed-operations.rkt" untyped)
            (submod "measures-with-dimensions/temperature.rkt" untyped)
            "measures-with-dimensions/untyped-operations.rkt"
            ))
  
  (require (submod "measures-with-dimensions/dimension-struct.rkt" untyped)
           (submod "measures-with-dimensions/dimension-operations.rkt" untyped)
           (submod "measures-with-dimensions/dimensions.rkt" untyped)
           (submod "measures-with-dimensions/unit-struct.rkt" untyped)
           (submod "measures-with-dimensions/unit-operations.rkt" untyped)
           (submod "measures-with-dimensions/units.rkt" untyped)
           (submod "measures-with-dimensions/measure-struct.rkt" untyped)
           (submod "measures-with-dimensions/physical-constants.rkt" untyped)
           (submod "measures-with-dimensions/measure-types.rkt" untyped)
           (submod "measures-with-dimensions/typed-operations.rkt" untyped)
           (submod "measures-with-dimensions/temperature.rkt" untyped)
           "measures-with-dimensions/untyped-operations.rkt"
           )
  )
