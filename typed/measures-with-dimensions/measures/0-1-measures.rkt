#lang sweet-exp typed/racket/base

provide 0-measure 1-measure

require "../units/unit-struct.rkt"
        "measure-struct.rkt"
        "../untyped-utils.rkt"


(: 0-measure : (measure 0 Dimensionless-Unit +inf.0))
(define 0-measure
  (measure 0 1-unit +inf.0))

(: 1-measure : (measure 1 Dimensionless-Unit +inf.0))
(define 1-measure
  (measure 1 1-unit +inf.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(untyped-module*
 )

