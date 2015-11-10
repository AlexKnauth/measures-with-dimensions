#lang sweet-exp typed/racket

require reprovide/reprovide "../untyped-utils.rkt"

reprovide "typed-operations-1.rkt"
          (only-in "untyped-operations.rkt" m: m)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(untyped-module*
 [#:all-from
  (submod typed/measures-with-dimensions/measures/typed-operations-1 untyped)
  typed/measures-with-dimensions/measures/untyped-operations
  ])


