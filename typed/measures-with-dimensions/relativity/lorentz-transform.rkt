#lang typed/racket/base

(require racket/match
         "../measures.rkt"
         "beta-gamma.rkt"
         )

(define-type 4Pos 4pos)
(struct 4pos ([x : Length-Real-Measure]
              [y : Length-Real-Measure]
              [z : Length-Real-Measure]
              [t : Time])
  #:transparent)

(: lorentz-transform-x : [Velocity-Real-Measure -> [4Pos -> 4Pos]])
(define (lorentz-transform-x u-x)
  (define β (beta (mabs u-x)))
  (define γ (gamma/beta β))
  (: transform : [4Pos -> 4Pos])
  (define (transform p)
    (match-define (4pos x y z t) p)
    (4pos
     (cast (m: γ * (m: x - β * t * c)) Length-Real-Measure)
     y
     z
     (cast (m: γ * (m: t - β * x / c)) Time)))
  transform)

