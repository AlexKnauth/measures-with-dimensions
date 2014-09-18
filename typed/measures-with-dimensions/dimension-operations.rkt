#lang typed/racket/base

(provide (all-defined-out))

(require "dimension-struct.rkt"
         "preds.rkt"
         "untyped-utils.rkt")

(define-type (M^ n) (dimension n 0 0 0 0) #:omit-define-syntaxes)
(define-type (L^ n) (dimension 0 n 0 0 0) #:omit-define-syntaxes)
(define-type (T^ n) (dimension 0 0 n 0 0) #:omit-define-syntaxes)
(define-type (Q^ n) (dimension 0 0 0 n 0) #:omit-define-syntaxes)
(define-type (Θ^ n) (dimension 0 0 0 0 n) #:omit-define-syntaxes)

(: M^ : (All (n) [n -> (M^ n)]))
(: L^ : (All (n) [n -> (L^ n)]))
(: T^ : (All (n) [n -> (T^ n)]))
(: Q^ : (All (n) [n -> (Q^ n)]))
(: Θ^ : (All (n) [n -> (Θ^ n)]))
(define (M^ n) (dimension n 0 0 0 0))
(define (L^ n) (dimension 0 n 0 0 0))
(define (T^ n) (dimension 0 0 n 0 0))
(define (Q^ n) (dimension 0 0 0 n 0))
(define (Θ^ n) (dimension 0 0 0 0 n))



(: dimension->list : (All (M-Expt L-Expt T-Expt Q-Expt Θ-Expt)
                          [(dimension M-Expt L-Expt T-Expt Q-Expt Θ-Expt)
                           -> (List M-Expt L-Expt T-Expt Q-Expt Θ-Expt)]))
(define (dimension->list d)
  (list (dimension-M-expt d)
        (dimension-L-expt d)
        (dimension-T-expt d)
        (dimension-Q-expt d)
        (dimension-Θ-expt d)))

(: dimension-map : (All (n ...) [(Integer ... n -> Integer) Dimension ... n -> Dimension]))
(define (dimension-map f . ds)
  (make-Dimension #:M^ (apply f (map Dimension-M-expt ds))
                  #:L^ (apply f (map Dimension-L-expt ds))
                  #:T^ (apply f (map Dimension-T-expt ds))
                  #:Q^ (apply f (map Dimension-Q-expt ds))
                  #:Θ^ (apply f (map Dimension-Θ-expt ds))))

(: dimension=? : [Dimension Dimension * -> Boolean])
(define (dimension=? d . rst)
  (for/and: : Boolean ([d2 : Dimension (in-list rst)])
    (equal? d d2)))

(define d=? dimension=?)



(define dimensionless-dimension? (make-predicate Dimensionless-Dimension))



(: dexpt : [Dimension Exact-Rational -> Dimension])
(define (dexpt d n)
  (define (raise-exponent-error n_0)
    (error 'dexpt (string-append
                   "can't have a dimension with a non-integer exponent" "\n"
                   "  given: ~v" "\n"
                   "  in: (dexpt ~v ~v)" "\n") n_0 d n))
  (cond [(zero? n) dimensionless-dimension]
        [(dimensionless-dimension? d) dimensionless-dimension]
        [(one? n) d]
        [(dimension? d) (: f : [Integer -> Integer])
                         (define (f expt)
                           (let ([new-expt : Exact-Rational (* expt n)])
                             (cond [(exact-integer? new-expt) new-expt]
                                   [else (raise-exponent-error new-expt)])))
                         (dimension-map f d)]
        ))

(: dsqr : [Dimension -> Dimension])
(define (dsqr d)
  (dexpt d 2))

(: dsqrt : [Dimension -> Dimension])
(define (dsqrt d)
  (dexpt d 1/2))

(: d* : [Dimension * -> Dimension])
(define d*
  (case-lambda
    [() dimensionless-dimension]
    [(d) d]
    [ds (apply dimension-map + ds)]
    ))

(: d1/ : [Dimension -> Dimension])
(define (d1/ d)
  (dimension-map - d))

(: d/ : [Dimension Dimension * -> Dimension])
(define (d/ d1 . rst)
  (apply dimension-map - d1 rst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(untyped-module*
 )



