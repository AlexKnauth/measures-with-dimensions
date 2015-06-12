#lang typed/racket/base

(provide molar-mass
         Molar-Mass
         g/mol
         )

(require racket/match
         (only-in typed/racket/base [U Un])
         "../measure-struct.rkt"
         "../measure-types.rkt"
         "../typed-operations.rkt"
         "element-struct.rkt"
         "compound.rkt"
         "../untyped-utils.rkt"
         )

(: molar-mass : [(Un Element Compound) -> Molar-Mass])
(define (molar-mass comp)
  (match comp
    [(element n sym molar-mass) molar-mass]
    [(compound) (make-measure 0 g/mol)]
    [(compound [#{subs : (Listof (Un Element Compound))} #{ns : (Listof Natural)}] ...)
     (cast
      (apply m+
             (for/list ([sub : (Un Element Compound) (in-list subs)]
                        [n : Natural (in-list ns)])
               : (Listof Molar-Mass)
               (cast (m* (molar-mass sub) n) Molar-Mass)))
      Molar-Mass)]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(untyped-module*
 [g/mol #:from (submod typed/measures-with-dimensions/chemistry/element-struct untyped)]
 [Molar-Mass #:from (submod typed/measures-with-dimensions/chemistry/element-struct untyped)]
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module* test racket/base
  (require (submod ".." untyped)
           rackunit
           "../untyped-operations.rkt"
           "elements.rkt"
           "compound.rkt"
           )
  (check-equal? (molar-mass H) (m: 1.00794 g/mol))
  (check-equal? (molar-mass O) (m: 15.9994 g/mol))
  (check-equal? (+ (* 1.00794 2) 15.9994) 18.01528)
  (check-equal? (molar-mass (compound: H2O)) (m: 18.01528 g/mol))
  )
