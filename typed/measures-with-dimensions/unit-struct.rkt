#lang typed/racket/base

(provide (struct-out unit)
         Unit
         Unitof
         Unitish
         Dimensionless-Unit
         ->unit
         make-Unit
         Unit?
         Unitish?
         Unit-name
         Unit-scalar
         Unit-dimension
         1-unit
         )

(require "dimension-struct.rkt"
         "preds.rkt"
         "untyped-utils.rkt")

(struct: (d) unit ([name : Any] [scalar : Positive-Real] [dimension : d])
  #:transparent
  #:guard (lambda (name scalar dimension _)
            (unless (positive-real? scalar)
              (error 'unit "expected Positive-Real for scalar argument, given ~v" scalar))
            (unless (Dimension? dimension)
              (error 'unit "expected Dimension for dimension argument, given ~v" dimension))
            (values name scalar dimension))
  #:property prop:custom-write (lambda (u out mode)
                                 (let ([name (unit-name u)])
                                   (cond [name (display name out)]
                                         [(zero? mode) (display "(unit #f " out)
                                                       (print (unit-scalar u) out) (display " " out)
                                                       (print (unit-dimension u) out)
                                                       (display ")" out)]
                                         [else (display (unit-scalar u) out)
                                               (display "*" out)
                                               (display (unit-dimension u) out)]))))

(define-type (Unitof d)
  (unit d))

(define-type Unitish
  (U Unit
     Positive-Real
     Dimension
     ))
(define-type Unit
  (Unitof Dimension))



(: ->unit : (All (d)
                 (case-> [(Unitof d) -> (Unitof d)]
                         [Positive-Real -> (Unitof Dimensionless-Dimension)]
                         [Unitish -> Unit])))
(define (->unit u)
  (cond [(unit? u) u]
        [(dimension? u) (unit u 1 u)]
        [(one? u) 1-unit]
        [(positive-real? u) (unit u u dimensionless-dimension)]
        [(Dimension? u) (make-Unit u 1 u)]))



(: make-Unit : (All (d) (case-> [Any Positive-Real d -> (Unitof d)]
                                [Any Positive-Real Dimension -> (Unitof Dimension)])))
(: Unit? : [Any -> Boolean : Unit])
(: Unitish? : [Any -> Boolean : Unitish])
(: Unit-name : [Unitish -> Any])
(: Unit-scalar : [Unitish -> Positive-Real])
(: Unit-dimension : (All (d) (case-> [(Unitof d) -> d]
                                     [Positive-Real -> Dimensionless-Dimension]
                                     [Unitish -> Dimension])))
(define (make-Unit name scalar dimension)
  (unit name scalar dimension))
(define Unit? (make-predicate Unit))
(define Unitish? (make-predicate Unitish))
(define (Unit-name u)
  (unit-name (->unit u)))
(define (Unit-scalar u)
  (unit-scalar (->unit u)))
(define (Unit-dimension u)
  (unit-dimension (->unit u)))



(define-type Dimensionless-Unit
  (Unitof Dimensionless-Dimension))

(: 1-unit : Dimensionless-Unit)
(define 1-unit (unit '1-unit 1 dimensionless-dimension))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(untyped-module*
 [#:begin (require "dimension-struct.rkt")]
 [->unit (Unitish -> Unit)]
 [make-Unit (Any Positive-Real Dimension -> Unit)]
 [Unit-dimension (Unit -> Dimension)]
 )



