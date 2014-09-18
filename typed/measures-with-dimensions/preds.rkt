#lang typed/racket/base

(provide zero?
         one?
         positive-real?
         )

(define zero? (make-predicate Zero))
(define one? (make-predicate One))
(define positive-real? (make-predicate Positive-Real))

