#lang typed/racket/base

(provide (struct-out element)
         Element
         Molar-Mass
         g/mol
         )

(require racket/match
         (only-in typed/racket/base [U Un])
         "../unit-operations.rkt"
         "../units.rkt"
         "../measure-types.rkt"
         "../untyped-utils.rkt"
         )

(struct (Sym) element
  ([atomic-number : Positive-Integer] [symbol : Sym] [atomic-mass : Mass])
  #:transparent
  #:property prop:custom-write
  (lambda ([e : (element Any)] [out : Output-Port] [mode : (Un 0 1 #t #f)])
    (match mode
      [(or 0 #f) (fprintf out "~a" (element-symbol e))]
      [(or 1 #t) (fprintf out "#<element:~a>" (element-symbol e))]))
  )

(define-type Element (element Symbol))

(define-type Molar-Mass Mass)

(define-unit g/mol : Mass-Unit (u/ gram mol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(untyped-module*
 )
