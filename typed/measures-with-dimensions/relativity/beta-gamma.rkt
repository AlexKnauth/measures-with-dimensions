#lang typed/racket/base

(provide beta gamma gamma/beta)

(require "../measures.rkt"
         "../preds.rkt"
         "../untyped-utils.rkt"
         )

(: beta : [Speed -> Nonnegative-Real])
(define (beta u)
  (assert (measure->number (m: u / c)) nonnegative-real?))

(: gamma/beta : [Nonnegative-Real -> Positive-Real])
(define (gamma/beta β)
  ;; γ = 1 / √[1 - β^2] = 1 / √[(1 + β)*(1 - β)]
  (cond
    [(= β 1) +inf.0]
    [(< β 1)
     (assert (/ (sqrt (* (+ 1 β) (- 1 β)))) positive-real?)]
    [else
     (error 'gamma "expected a speed slower than light, given ~v times that" β)]))

(: gamma : [Speed -> Positive-Real])
(define (gamma u)
  (gamma/beta (beta u)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(untyped-module*
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module* test racket/base
  (require (submod ".." untyped)
           (submod "../measures.rkt" untyped)
           rackunit)
  (check-equal? (gamma (m: 0    c)) 1)
  (check-equal? (gamma (m: 1/10 c)) (* (sqrt 99) 10/99)) ; 10/√[99]
  (check-equal? (gamma (m: 1/5  c)) (* (sqrt 24)  5/24)) ;  5/√[24]
  (check-equal? (gamma (m: 3/10 c)) (* (sqrt 91) 10/91)) ; 10/√[91]
  (check-equal? (gamma (m: 2/5  c)) (* (sqrt 21)  5/21)) ;  5/√[21]
  (check-equal? (gamma (m: 1/2  c)) (/ 2 (sqrt 3)))      ; 2/√[3]
  (check-equal? (gamma (m: 3/5  c)) 5/4)                 ; 5/4        ; Pythagorean?
  (check-equal? (gamma (m: 7/10 c)) (* (sqrt 51) 10/51)) ; 10/√[51]
  (check-equal? (gamma (m: 4/5  c)) 5/3)                 ; 5/3        ; Pythagorean?
  (check-=      (gamma (m: 9/10 c)) (* 10 (sqrt 1/19))   ; 10/√[19]
                1e-15)
  (check-equal? (gamma (m: 1    c)) +inf.0)
  ;; 3 - 4 - 5                                                        ; Yup. Pythagorean.
  (check-equal? (gamma (m: 3/5 c)) 5/4)
  (check-equal? (gamma (m: 4/5 c)) 5/3)
  ;; 5 - 12 - 13
  (check-equal? (gamma (m:  5/13 c)) 13/12)
  (check-equal? (gamma (m: 12/13 c)) 13/5)
  ;; 68 - 1155 - 1157
  (check-equal? (gamma (m:   68/1157 c)) 1157/1155)
  (check-equal? (gamma (m: 1155/1157 c)) 1157/68)
  )

