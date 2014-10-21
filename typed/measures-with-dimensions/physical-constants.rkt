#lang typed/racket/base

(provide (all-defined-out))

(require "dimension-struct.rkt"
         "unit-struct.rkt"
         "unit-operations.rkt"
         "units.rkt"
         "measure-struct.rkt"
         "measure-types.rkt"
         "untyped-utils.rkt"
         )

(: 0-measure : (measure 0 Dimensionless-Unit +inf.0))
(define 0-measure
  (measure 0 1-unit +inf.0))

(: 1-measure : (measure 1 Dimensionless-Unit +inf.0))
(define 1-measure
  (measure 1 1-unit +inf.0))

(: g : Acceleration)
(define g  ; the acceleration due to gravity on earth
  (measure 1 (unit-rename gravitational-acceleration-unit 'g) +inf.0))

(: G : Number-Measure)
(define G  ; Newton's universal gravitational constant, 
  #;[]    #; (for use in F = (* G (/ (* m-1 m-2)
                                     (sqr d))))
  (make-Measure (* #i6.7384 (10^ -11))
                (u* newton (u/ (usqr meter)
                               (usqr kilogram)))))

(: c : Speed)
(define c  ; the speed of light in a vacuum
  (measure 1 (unit-rename speed-of-light-unit 'c) +inf.0))

(: elementary-charge : Charge)
(define elementary-charge
  (measure 1 (unit-rename elementary-charge-unit 'e) +inf.0))

(: R : (Measureof Positive-Real (Unitof (dimension 1 2 -2 0 -1))))
(define R
  (measure 1 (unit-rename ideal-gas-constant-unit 'R) +inf.0))

(: k_B : (Measureof Positive-Real (Unitof (dimension 1 2 -2 0 -1))))
(define k_B
  (measure 1 (unit-rename bolzmann-constant-unit 'k_B) +inf.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(untyped-module*
 )

