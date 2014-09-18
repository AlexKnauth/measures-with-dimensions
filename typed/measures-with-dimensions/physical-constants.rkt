#lang typed/racket/base

(provide (all-defined-out))

(require "unit-struct.rkt"
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
  (measure #e9.80665 m/s^2 +inf.0))

(: G : Number-Measure)
(define G  ; Newton's universal gravitational constant, 
  #;[]    #; (for use in F = (* G (/ (* m-1 m-2)
                                     (sqr d))))
  (make-Measure (* #i6.7384 (10^ -11))
                (u* newton (u/ (usqr meter)
                               (usqr kilogram)))))

(: c : Speed)
(define c  ; the speed of light in a vacuum
  (measure #e299792458 m/s +inf.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(untyped-module*
 )

