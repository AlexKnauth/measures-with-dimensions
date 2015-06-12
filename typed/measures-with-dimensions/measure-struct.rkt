#lang typed/racket/base

(provide (all-defined-out))

(require racket/splicing
         math/bigfloat
         (except-in racket/list second)
         "dimension-struct.rkt"
         "dimension-operations.rkt"
         "unit-struct.rkt"
         "unit-operations.rkt"
         "vector-operations.rkt"
         "preds.rkt"
         "untyped-utils.rkt"
         )

(define-type Sig-Figs (U Positive-Integer +inf.0))
(define sig-figs? (make-predicate Sig-Figs))

(: sig-fig-min : [Sig-Figs * -> Sig-Figs])
(define sig-fig-min
  (case-lambda
    [() +inf.0]
    [args (define x (apply min args))
          (cond [(equal? x +inf.0) +inf.0]
                [else (cast (inexact->exact x) Sig-Figs)])]))

(define-type Vec (Vectorof Real))
(define-type Num/Vec (U Number Vec))
(define-type Real/Vec (U Real Vec))

(define-type Measure
  (Measureof Num/Vec Unit))

(define-type (Measureof n u)
  (measure n u Sig-Figs))

(define-type (Number-Measureof u)
  (Measureof Number u))

(define-type (Real-Measureof u)
  (Measureof Real u))

(define-type Number-Measure
  (Number-Measureof Unit))

(define-type Real-Measure
  (Real-Measureof Unit))

(define-type (Vector-Measureof u)
  (Measureof Vec u))

(define-type Vector-Measure
  (Vector-Measureof Unit))

(define-type Zero-Measure
  (Measureof Zero Dimensionless-Unit))

(define-type Measureish
  (U Measure
     Number
     Unitish
     Dimension
     VectorTop))

(define-type Number-Measureish
  (U Number-Measure
     Number
     Unitish
     Dimension))

(define-type Real-Measureish
  (U Real-Measure
     Real
     Unitish
     Dimension))

(define-type Vector-Measureish
  (U Vector-Measure
     VectorTop))

(struct: (n u sfs) measure ([number : n]
                            [unit : u]
                            [sig-figs : sfs]) ; for binary
  #:transparent
  #:guard (lambda (number unit sig-figs _)
            (unless (or (number? number)
                        (and (vector? number)
                             (for/and : Boolean ([n : Any (in-vector number)])
                               (real? n))))
              (error 'measure "expected (or/c number? (vectorof real?)), given ~v" number))
            (unless (Unit? unit)
              (error 'measure "expected Unit? for unit argument, given ~v" unit))
            (unless (sig-figs? sig-figs)
              (error 'measure "expected sig-figs? for sig-figs argument, given ~v" sig-figs))
            (values number unit sig-figs))
  #:property prop:custom-write (lambda (m out mode)
                                 (cond [(zero? mode) (display "(m: " out)
                                                     (print (measure-number m) out)
                                                     (display " " out)
                                                     (print (measure-unit m) out)
                                                     (display ")" out)]
                                       [else (display (measure-number m) out)
                                             (display "*" out)
                                             (display (measure-unit m) out)])))



(: make-measure : (All (n u sfs) (->* [n u] [sfs] (measure n u (U sfs +inf.0)))))
(define (make-measure number unit [sig-figs +inf.0])
  (measure number unit sig-figs))



(define number-measure? (make-predicate Number-Measure))
(define number-measureish? (make-predicate Number-Measureish))
(define real-measure? (make-predicate Real-Measure))
(define real-measureish? (make-predicate Real-Measureish))
(define zero-measure? (make-predicate Zero-Measure))



(: ->measure : (All (n d sfs)
                    (case-> [(measure n (Unitof d) sfs)
                             -> (measure n (Unitof d) sfs)]
                            [(Unitof d)
                             -> (measure 1 (Unitof d) +inf.0)]
                            [Real -> (Measureof Real Dimensionless-Unit)]
                            [Number -> (Measureof Number Dimensionless-Unit)]
                            [Number-Measure -> Number-Measure]
                            [Vector-Measure -> Vector-Measure]
                            [Measureish -> Measure])))
(define (->measure m)
  (cond [(measure? m) m]
        [(unit? m) (measure 1 m +inf.0)]
        [(dimension? m) (measure 1 (unit m 1 m) +inf.0)]
        [(number? m) (number->measure m)]
        [(vector? m) (vector->measure m)]
        [(Unitish? m) ((inst measure 1 Unit +inf.0) 1 (->unit m) +inf.0)]
        ;[else (error '->measure "could not convert to a measure. given: ~v" m)]
        ))

(: number->measure : (All (n) (case-> [n -> (Measureof n Dimensionless-Unit)]
                                      [Number -> Number-Measure])))
(define (number->measure n)
  (define n* : Number (assert n number?))
  (cond [(exact? n*) (measure n 1-unit +inf.0)]
        [(double-flonum? n*) (measure n 1-unit 53)] ; FIXME: shouldn't count end zeros
        [(single-flonum? n*) (measure n 1-unit 52/2)] ; what should this be?
        [else (measure n 1-unit +inf.0)]))


(: vector->measure : [VectorTop -> Vector-Measure])
(define (vector->measure v)
  (: v.length : Index)
  (define v.length (vector-length v))
  (cond [(zero? v.length)
         (measure (ann #() Vec) 1-unit +inf.0)]
        [else
         (let* ([ms : (Listof Measure)
                    (for/list : (Listof Measure) ([m : Any (in-vector v)])
                      (->measure (cast m Measureish)))]
                [m0 : Measure (first ms)]
                [u : Unit (measure-unit m0)]
                [d : Dimension (unit-dimension u)])
           (: ns : (Listof Real))
           (define ns
             (for/list : (Listof Real) ([m : Measure (in-list ms)])
               (unless (dimension=? (measure-dimension m) d)
                 (error 'vector->measure "incompatible units within vector"))
               (define n (measure-number (convert m u)))
               (unless (real? n)
                 (error 'vector->measure "vector contains a non-real number in: ~v" n))
               n))
           (: new-v : Vec)
           (define new-v
             (apply (inst vector-immutable Real) ns))
           (: sig-figs : Sig-Figs)
           (define sig-figs (apply sig-fig-min (map Measure-sig-figs ms)))
           (measure new-v u sig-figs))]))



(: measure-dimension : (All (d) [(measure Num/Vec (Unitof d) Sig-Figs) -> d]))
(define (measure-dimension m)
  (unit-dimension (measure-unit m)))

(: Measure-number : [Measure -> Num/Vec])
(: Measure-unit : [Measure -> Unit])
(: Measure-dimension : [Measure -> Dimension])
(: Measure-sig-figs : [Measure -> Sig-Figs])

(: make-Measure : (All (d) (case-> (-> Num/Vec (Unitof d)
                                       (Measureof Num/Vec (Unitof d)))
                                   (-> Num/Vec (Unitof d) Sig-Figs
                                       (Measureof Num/Vec (Unitof d)))
                                   (-> Number Unitish Number-Measure)
                                   (-> Vec Unitish Vector-Measure)
                                   (-> Num/Vec Unitish Measure)
                                   (-> Num/Vec Unitish Sig-Figs Measure))))
(define (make-Measure num u [sf +inf.0])
  (measure num (->unit u) sf))
(define (Measure-number m)
  (measure-number (->measure m)))
(define (Measure-unit m)
  (measure-unit (->measure m)))
(define (Measure-dimension m)
  (unit-dimension (Measure-unit m)))
(define (Measure-sig-figs m)
  (measure-sig-figs (->measure m)))

(: measure=? : [Measureish Measureish * -> Boolean])
(define (measure=? m . rst)
  (let* ([m : Measure (->measure m)]
         [m.number : Num/Vec (measure-number m)]
         [m.unit : Unit (measure-unit m)]
         [m.dimension : Dimension (unit-dimension m.unit)])
    (cond [(number? m.number) (let: ([m.number*u.scalar : Number (* m.number (Unit-scalar m.unit))])
                                (for/and: : Boolean ([m2 : Measureish (in-list rst)])
                                  (let* ([m2 : Measure (->measure m2)]
                                         [m2.number : Num/Vec (Measure-number m2)]
                                         [m2.unit : Unit (Measure-unit m2)])
                                    (and (number? m2.number)
                                         (let: ([m2.number*u2.scalar : Number (* m2.number (Unit-scalar m2.unit))]
                                                [m2.dimension : Dimension (Unit-dimension m2.unit)])
                                           (and (= m.number*u.scalar m2.number*u2.scalar)
                                                (dimension=? m.dimension m2.dimension)))))))]
          [(vector? m.number) (let: ([m.number*u.scalar : Vec (v* (Unit-scalar m.unit) m.number)])
                                (for/and: : Boolean ([m2 : Measureish (in-list rst)])
                                  (let* ([m2 : Measure (->measure m2)]
                                         [m2.number : Num/Vec (Measure-number m2)]
                                         [m2.unit : Unit (Measure-unit m2)])
                                    (and (vector? m2.number)
                                         (let: ([m2.number*u2.scalar : Vec (v* (Unit-scalar m2.unit) m2.number)]
                                                [m2.dimension : Dimension (Unit-dimension m2.unit)])
                                           (and (v=? m.number*u.scalar m2.number*u2.scalar)
                                                (dimension=? m.dimension m2.dimension)))))))]
          )))

(define m=? measure=?)


(: convert : (All (d)
                  (case->
                   [Real-Measure (Unitof d) -> (Real-Measureof (Unitof d))]
                   [Number-Measure (Unitof d) -> (Number-Measureof (Unitof d))]
                   [Vector-Measure (Unitof d) -> (Vector-Measureof (Unitof d))]
                   [Measure (Unitof d) -> (Measureof Num/Vec (Unitof d))]
                   [Num/Vec Unitish (Unitof d) -> (Measureof Num/Vec (Unitof d))]
                   [Number-Measure Unitish -> Number-Measure]
                   [Measure Unitish -> Measure]
                   [Num/Vec Unitish Unitish -> Measure])))
(define convert
  (case-lambda
    [(m u2)
     (let (;[m (->measure m)]
           [u2 (->unit u2)])
       (unless (dimension=? (Measure-dimension m) (assert (Unit-dimension u2) Dimension?))
         (error 'convert "cannot convert between dimensions, given: (convert ~v ~v)" m u2))
       (cond [(number-measure? m) (let: ([num (measure-number m)]
                                         [u1 : Unit (Measure-unit m)])
                                    (measure (* num (/ (unit-scalar u1)
                                                       (unit-scalar u2)))
                                             u2
                                             (Measure-sig-figs m)))]
             [else (let: ([num : Vec (cast (Measure-number m) Vec)]
                          [u1 : Unit (Measure-unit m)])
                     (measure (v* (/ (unit-scalar u1)
                                     (unit-scalar u2))
                                  num)
                              u2
                              (Measure-sig-figs m)))]))]
    [([n : Num/Vec] [u1 : Unitish] u2)
     (convert (make-Measure n u1) u2)]))



(: measure->num/vec : [Measureish -> Num/Vec])
(define (measure->num/vec m)
  (cond [(number? m) m]
        [else (measure-number (convert (->measure m) 1-unit))]))

(: measure->number : [Number-Measureish -> Number])
(define (measure->number m)
  (cond [(number? m) m]
        [else (assert (measure-number (convert (->measure m) 1-unit)) number?)]))

(: measure->real : [Real-Measureish -> Real])
(define (measure->real m)
  (cond [(real? m) m]
        [else (assert (measure-number (convert (->measure m) 1-unit)) real?)]))

(: measure->vector : [Vector-Measureish -> Vec])
(define (measure->vector m)
  (define vec (assert (measure-number (convert (->measure m) 1-unit)) vector?))
  (apply (inst vector-immutable Real)
         (for/list ([x (in-vector vec)]) : (Listof Real)
           (assert x real?))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(untyped-module*
 [#:begin (require "unit-struct.rkt")]
 [make-Measure (->* [Num/Vec Unitish] [Sig-Figs] Measure)]
 [->measure (-> Measureish Measure)]
 [convert (case-> [-> Measure Unitish Measure]
                  [-> Num/Vec Unitish Unitish Measure])]
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tests:

(module* test racket/base
  (require (submod ".." untyped)
           (submod "units.rkt" untyped))
  (require rackunit)
  (check measure=?
         (make-Measure 1    meter)
         (make-Measure 100  centimeter))
  (check measure=?
         (make-Measure 1    meter)
         (make-Measure 1000 millimeter))
  )
