#lang typed/racket/base

(provide (all-defined-out)
         (all-from-out "SI.rkt"))

(require "../units.rkt"
         "../defmulti.rkt"
         (only-in "SI.rkt"
                  s
                  ))

(defmulti
  [lb-m pound-mass]
  [oz ounce-mass]
  [ft foot]
  [in inch]
  [yd yard]
  [mi mile]
  [ft^2 square-foot]
  [in^2 square-inch]
  [mi^2 square-mile]
  [ft^3 cubic-foot]
  [in^3 cubic-inch]
  [gal gallon]
  [qt quart]
  [pt pint]
  [cp cup]
  [floz fluid-ounce]
  [tbs tablespoon]
  [tsp teaspoon]
  [mph mile-per-hour]
  [fps foot-per-second]
  [lb-f pound-force]
  [psi pound-per-square-inch]
  )
