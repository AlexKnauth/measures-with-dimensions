#lang typed/racket/base

(provide (struct-out element)
         Element
         )

(require "../measure-types.rkt")

(struct (Sym) element
  ([atomic-number : Positive-Integer] [symbol : Sym] [atomic-mass : Mass])
  #:transparent)

(define-type Element (element Symbol))

