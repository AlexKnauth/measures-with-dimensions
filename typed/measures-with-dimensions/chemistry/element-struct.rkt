#lang typed/racket/base

(provide (struct-out element)
         Element
         Molar-Mass
         g/mol
         )

(require "../unit-operations.rkt"
         "../units.rkt"
         "../measure-types.rkt"
         )

(struct (Sym) element
  ([atomic-number : Positive-Integer] [symbol : Sym] [atomic-mass : Mass])
  #:transparent)

(define-type Element (element Symbol))

(define-type Molar-Mass Mass)

(define-unit g/mol : Mass-Unit (u/ gram mol))

