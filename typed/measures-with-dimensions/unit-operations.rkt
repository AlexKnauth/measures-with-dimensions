#lang typed/racket/base

(provide (all-defined-out))

(require syntax/parse/define
         racket/math
         (except-in racket/list second)
         "unit-struct.rkt"
         "dimension-struct.rkt"
         "dimension-operations.rkt"
         "preds.rkt"
         "untyped-utils.rkt"
         )


(: unit=? (Unitish Unitish * -> Boolean))
(define (unit=? u . rst)
  (let ([u : Unit (->unit u)])
    (let ([u.scalar : Positive-Real (unit-scalar u)]
          [u.dimension : Dimension (unit-dimension u)])
      (for/and: : Boolean ([u2 : Unitish (in-list rst)])
        (let ([u2 : Unit (->unit u2)])
          (and (= u.scalar (unit-scalar u2))
               (dimension=? u.dimension (unit-dimension u2))))))))


(: unit-rename : (Unitish Any -> Unit))
(define (unit-rename u name)
  (let ([u : Unit (->unit u)])
    (make-Unit name
               (unit-scalar u)
               (unit-dimension u))))

(define-simple-macro (define-unit u:id (~literal :) t:expr val:expr)
  (begin
    (: u : t)
    (define u (cast (unit-rename val 'u) t))))

(: usqr : [Unitish -> Unit])
(define (usqr u)
  (let ([u : Unit (->unit u)])
    (cond [(unit=? u 1-unit) 1-unit]
          [else (make-Unit `(usqr ,u)
                           (cast (sqr (Unit-scalar u)) Positive-Real)
                           (dsqr (Unit-dimension u)))])))

(: usqrt : [Unitish -> Unit])
(define (usqrt u)
  (let ([u : Unit (->unit u)])
    (cond [(unit=? u 1-unit) 1-unit]
          [else (make-Unit `(usqrt ,u)
                           (cast (sqrt (Unit-scalar u)) Positive-Real)
                           (dsqrt (Unit-dimension u)))])))

(: uexpt : [Unitish Exact-Rational -> Unit])
(define (uexpt u n)
  (let ([u : Unit (->unit u)])
    (cond [(unit=? u 1-unit) 1-unit]
          [(zero? n) 1-unit]
          [(one? n) u]
          [else (make-Unit `(uexpt ,u ,n)
                           (cast (expt (Unit-scalar u) n) Positive-Real)
                           (dexpt (Unit-dimension u) n))])))

(: u* : [Unitish * -> Unit])
(define (u* . args)
  (let ([args : (Listof Unit) (filter-not (λ (u) (equal? u 1-unit)) (map ->unit args))])
    (cond [(empty? args) 1-unit]
          [(one? (length args)) (first args)]
          [else (make-Unit `(u* ,@args)
                           (cast (apply * (map Unit-scalar args)) Positive-Real)
                           (apply d* (map Unit-dimension args)))])))

(: u1/ : [Unitish -> Unit])
(define (u1/ u)
  (let ([u : Unit (->unit u)])
    (make-Unit `(u1/ ,u)
               (cast (/ (Unit-scalar u)) Positive-Real)
               (d1/ (Unit-dimension u)))))

(: u/ : [Unitish Unitish * -> Unit])
(define u/
  (case-lambda
    [([u : Unitish]) (u1/ u)]
    [([u : Unitish] . [rst : Unitish *])
     (let ([u : Unit (->unit u)]
           [rst : (Listof Unit) (map ->unit rst)])
       (make-Unit `(u/ ,u ,@rst)
                  (cast (apply / (Unit-scalar u) (map Unit-scalar rst)) Positive-Real)
                  (apply d/ (Unit-dimension u) (map Unit-dimension rst))))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(untyped-module*
 )




