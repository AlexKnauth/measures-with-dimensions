#lang racket

(provide (rename-out [m* untyped:m*]))

(require (submod "dimension-struct.rkt" untyped)
         (submod "dimension-operations.rkt" untyped)
         (submod "unit-struct.rkt" untyped)
         (submod "unit-operations.rkt" untyped)
         (submod "measure-struct.rkt" untyped)
         (submod "physical-constants.rkt" untyped)
         (submod "typed-operations-1.rkt" untyped)
         )

;; m* : [Measureish * -> Measure]
(define (m* . args)
  (let ([args (map ->measure args)])
    (define (vector-measure? m)
      (vector? (Measure-number m)))
    (define-values (vectors scalars)
      (partition vector-measure? args))
    (match vectors
      [(list)
       (apply m*/scalar scalars)]
      [(list v)
       (m*/vector (apply m*/scalar scalars) v)]
      [vectors
       (error 'm*
              (string-append
               "can't multiply 2 or more vectors together" "\n"
               "  use mdot or mcross instead" "\n"
               "  given: ~v")
              vectors)])))

