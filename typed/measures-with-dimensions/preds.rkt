#lang typed/racket/base

(provide zero?
         one?
         positive-real?
         exact-rational?
         )

(define zero? (make-predicate Zero))
(define one? (make-predicate One))
(define positive-real? (make-predicate Positive-Real))
(define exact-rational? (make-predicate Exact-Rational))

