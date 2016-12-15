#lang sweet-exp typed/racket/base

provide (all-defined-out)

require racket/math
        "../dimensions/dimension-struct.rkt"
        "../units/unit-struct.rkt"
        "../units/unit-operations.rkt"
        "../units/units.rkt"
        "measure-struct.rkt"
        "measure-types.rkt"
        "../untyped-utils.rkt"

;; ----------------------------------------------------------------------------

;; Gravity

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

;; ----------------------------------------------------------------------------

;; The Speed of Light

(: c : Speed)
(define c  ; the speed of light in a vacuum
  (measure 1 (unit-rename speed-of-light-unit 'c) +inf.0))

;; ----------------------------------------------------------------------------

;; Electricity and Magnetism

(: elementary-charge : Charge)
(define elementary-charge
  (measure 1 (unit-rename elementary-charge-unit 'e) +inf.0))

(: electric-permittivity-of-free-space :
   (Measureof Positive-Real (Unitof (dimension -1 -3 2 2 0))))
(define electric-permittivity-of-free-space
  (measure 1
           (unit-rename electric-permittivity-of-free-space-unit 'ϵ_0)
           +inf.0))

(: magnetic-permeability-of-free-space :
   (Measureof Positive-Real (Unitof (dimension 1 1 0 -2 0))))
(define magnetic-permeability-of-free-space
  (measure 1
           (unit-rename magnetic-permeability-of-free-space-unit 'μ_0)
           +inf.0))

;; ----------------------------------------------------------------------------

;; Thermodynamics

(: R : (Measureof Positive-Real (Unitof (dimension 1 2 -2 0 -1))))
(define R
  (measure 1 (unit-rename ideal-gas-constant-unit 'R) +inf.0))

(: k_B : (Measureof Positive-Real (Unitof (dimension 1 2 -2 0 -1))))
(define k_B
  (measure 1 (unit-rename bolzmann-constant-unit 'k_B) +inf.0))

;; ----------------------------------------------------------------------------

;; Quantum

(: planck-constant : (Measureof Positive-Real Angular-Momentum-Unit))
(define planck-constant
  (cast (make-Measure #i6.626070040e-34 (u* joule second))
        (Measureof Positive-Real Angular-Momentum-Unit)))

(: reduced-planck-constant : (Measureof Positive-Real Angular-Momentum-Unit))
(define reduced-planck-constant
  (cast (make-measure (measure-number planck-constant)
                      (u* joule second (u1/ 2pi-unit)))
        (Measureof Positive-Real Angular-Momentum-Unit)))

;; ----------------------------------------------------------------------------

(untyped-module*
 )

