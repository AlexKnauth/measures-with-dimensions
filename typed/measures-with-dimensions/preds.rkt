#lang typed/racket/base

(provide zero?
         one?
         positive-real?
         nonnegative-real?
         exact-rational?
         )

(define zero? (make-predicate Zero))
(define one? (make-predicate One))
(define positive-real? (make-predicate Positive-Real))
(define nonnegative-real? (make-predicate Nonnegative-Real))
(define exact-rational? (make-predicate Exact-Rational))

