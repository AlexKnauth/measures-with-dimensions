#lang typed/racket/base

(provide (struct-out compound)
         Compound
         ->compound
         )

(require racket/match
         (only-in typed/racket/base [U Un])
         "element-struct.rkt"
         "elements.rkt"
         )

(struct compound
  ([alist : (Listof (Pairof (Un Element Compound) Natural))])
  #:transparent)

(define-type Compound compound)

(: ->compound : [(Un Compound Element Symbol) -> Compound])
(define (->compound x)
  (match x
    [(? compound?) x]
    [(? element?) (compound (list (cons x 1)))]
    [(? symbol?) (->compound (->element x))]
    ))

