#lang typed/racket/base

(provide celsius
         fahrenheit
         get-kelvin
         get-rankine
         get-celsius
         get-fahrenheit
         celsius->fahrenheit
         fahrenheit->celsius
         )

(require "units.rkt"
         "measure-struct.rkt"
         "measure-types.rkt"
         "untyped-utils.rkt"
         )

(define nonnegative-real? (make-predicate Nonnegative-Real))

(: celsius->kelvin : [Real -> Nonnegative-Real])
(define (celsius->kelvin n)
  (assert (+ n #e273.15) nonnegative-real?))

(: fahrenheit->rankine : [Real -> Nonnegative-Real])
(define (fahrenheit->rankine n)
  (assert (+ n #e459.67) nonnegative-real?))

(: kelvin->celsius : [Nonnegative-Real -> Real])
(define (kelvin->celsius n)
  (- n #e273.15))

(: rankine->fahrenheit : [Nonnegative-Real -> Real])
(define (rankine->fahrenheit n)
  (- n #e459.67))

(: celsius : [Real -> Absolute-Temperature])
(define (celsius n)
  (make-measure (celsius->kelvin n) kelvin))

(: fahrenheit : [Real -> Absolute-Temperature])
(define (fahrenheit n)
  (make-measure (fahrenheit->rankine n) rankine))

(: get-kelvin : [Absolute-Temperature -> Nonnegative-Real])
(define (get-kelvin m)
  (assert (measure-number (convert m kelvin)) nonnegative-real?))

(: get-rankine : [Absolute-Temperature -> Nonnegative-Real])
(define (get-rankine m)
  (assert (measure-number (convert m rankine)) nonnegative-real?))

(: get-celsius : [Absolute-Temperature -> Real])
(define (get-celsius m)
  (kelvin->celsius (get-kelvin m)))

(: get-fahrenheit : [Absolute-Temperature -> Real])
(define (get-fahrenheit m)
  (rankine->fahrenheit (get-rankine m)))

(: celsius->fahrenheit : [Real -> Real])
(define (celsius->fahrenheit n)
  (get-fahrenheit (celsius n)))

(: fahrenheit->celsius : [Real -> Real])
(define (fahrenheit->celsius n)
  (get-celsius (fahrenheit n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(untyped-module*
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module* test racket/base
  (require (submod ".." untyped)
           rackunit
           "units.rkt"
           "untyped-operations.rkt"
           )
  (check-equal? (get-kelvin (m 0 kelvin)) 0)
  (check-equal? (get-celsius (m 0 kelvin)) #e-273.15)
  (check-equal? (get-kelvin (celsius 0)) #e273.15)
  (check-equal? (fahrenheit->celsius 32) 0)
  (check-equal? (fahrenheit->celsius 212) 100)
  )
