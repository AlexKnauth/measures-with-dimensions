#lang typed/racket/base

(provide (all-defined-out))

(require racket/splicing
         math/bigfloat
         (except-in racket/list second)
         (only-in plot/typed/utils
                  [v* plot:v*])
         "dimension-struct.rkt"
         "dimension-operations.rkt"
         "unit-struct.rkt"
         "unit-operations.rkt"
         "vector-opperations.rkt"
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

(define-type Measure
  (Measureof (U Number (Vectorof Real)) Unit))

(define-type (Measureof n u)
  (measure n u Sig-Figs))

(define-type (Number-Measureof u)
  (Measureof Number u))

(define-type Number-Measure
  (Number-Measureof Unit))

(define-type (Vector-Measureof u)
  (Measureof (Vectorof Real) u))

(define-type Vector-Measure
  (Vector-Measureof Unit))

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
                                 (cond [(zero? mode) (display "(m " out)
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



(: ->measure : (All (n d sfs)
                    (case-> [(measure n (Unitof d) sfs)
                             -> (measure n (Unitof d) sfs)]
                            [(Unitof d)
                             -> (measure 1 (Unitof d) +inf.0)]
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

(: number->measure : [Number -> Number-Measure])
(define (number->measure n)
  (cond [(exact? n) (measure n 1-unit +inf.0)]
        [(double-flonum? n) (measure n 1-unit 53)] ; FIXME: shouldn't count end zeros
        [(single-flonum? n) (measure n 1-unit 52/2)] ; what should this be?
        [else (measure n 1-unit +inf.0)]))


(: vector->measure : [VectorTop -> Vector-Measure])
(define (vector->measure v)
  (: v.length : Index)
  (define v.length (vector-length v))
  (cond [(zero? v.length)
         (measure (ann #() (Vectorof Real)) 1-unit +inf.0)]
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
           (: new-v : (Vectorof Real))
           (define new-v
             (apply (inst vector-immutable Real) ns))
           (: sig-figs : Sig-Figs)
           (define sig-figs (apply sig-fig-min (map Measure-sig-figs ms)))
           (measure new-v u sig-figs))]))



(: measure-dimension : (All (d) [(measure (U Number (Vectorof Real)) (Unitof d) Sig-Figs) -> d]))
(define (measure-dimension m)
  (unit-dimension (measure-unit m)))

(: Measure-number : [Measure -> (U Number (Vectorof Real))])
(: Measure-unit : [Measure -> Unit])
(: Measure-dimension : [Measure -> Dimension])
(: Measure-sig-figs : [Measure -> Sig-Figs])

(: make-Measure : (All (d) (case-> (-> (U Number (Vectorof Real)) (Unitof d)
                                       (Measureof (U Number (Vectorof Real)) (Unitof d)))
                                   (-> (U Number (Vectorof Real)) (Unitof d) Sig-Figs
                                       (Measureof (U Number (Vectorof Real)) (Unitof d)))
                                   (-> Number Unitish Number-Measure)
                                   (-> (Vectorof Real) Unitish Vector-Measure)
                                   (-> (U Number (Vectorof Real)) Unitish Measure)
                                   (-> (U Number (Vectorof Real)) Unitish Sig-Figs Measure))))
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
         [m.number : (U Number (Vectorof Real)) (measure-number m)]
         [m.unit : Unit (measure-unit m)]
         [m.dimension : Dimension (unit-dimension m.unit)])
    (cond [(number? m.number) (let: ([m.number*u.scalar : Number (* m.number (Unit-scalar m.unit))])
                                (for/and: : Boolean ([m2 : Measureish (in-list rst)])
                                  (let* ([m2 : Measure (->measure m2)]
                                         [m2.number : (U Number (Vectorof Real)) (Measure-number m2)]
                                         [m2.unit : Unit (Measure-unit m2)])
                                    (and (number? m2.number)
                                         (let: ([m2.number*u2.scalar : Number (* m2.number (Unit-scalar m2.unit))]
                                                [m2.dimension : Dimension (Unit-dimension m2.unit)])
                                           (and (= m.number*u.scalar m2.number*u2.scalar)
                                                (dimension=? m.dimension m2.dimension)))))))]
          [(vector? m.number) (let: ([m.number*u.scalar : (Vectorof Real) (plot:v* m.number (Unit-scalar m.unit))])
                                (for/and: : Boolean ([m2 : Measureish (in-list rst)])
                                  (let* ([m2 : Measure (->measure m2)]
                                         [m2.number : (U Number (Vectorof Real)) (Measure-number m2)]
                                         [m2.unit : Unit (Measure-unit m2)])
                                    (and (vector? m2.number)
                                         (let: ([m2.number*u2.scalar : (Vectorof Real) (plot:v* m2.number (Unit-scalar m2.unit))]
                                                [m2.dimension : Dimension (Unit-dimension m2.unit)])
                                           (and (v=? m.number*u.scalar m2.number*u2.scalar)
                                                (dimension=? m.dimension m2.dimension)))))))]
          )))

(define m=? measure=?)


(: convert : (All (d)
                  (case->
                   [Measure (Unitof d) -> (Measureof (U Number (Vectorof Real)) (Unitof d))]
                   [(U Number (Vectorof Real)) Unitish (Unitof d) -> (Measureof (U Number (Vectorof Real)) (Unitof d))]
                   [Number-Measure Unitish -> Number-Measure]
                   [Measure Unitish -> Measure]
                   [(U Number (Vectorof Real)) Unitish Unitish -> Measure])))
(define convert
  (case-lambda
    [(m u2)
     (let (;[m (->measure m)]
           [u2 (->unit u2)])
       (unless (dimension=? (Measure-dimension m) (assert (Unit-dimension u2) Dimension?))
         (error 'convert "cannot convert between dimensions, given: (convert ~v ~v)" m u2))
       (cond [(number-measure? m) (let: ([num : Number (cast (Measure-number m) Number)]
                                         [u1 : Unit (Measure-unit m)])
                                    (measure (* num (/ (unit-scalar u1)
                                                       (unit-scalar u2)))
                                             u2
                                             (Measure-sig-figs m)))]
             [else (let: ([num : (Vectorof Real) (cast (Measure-number m) (Vectorof Real))]
                          [u1 : Unit (Measure-unit m)])
                     (measure (plot:v* num (/ (unit-scalar u1)
                                              (unit-scalar u2)))
                              u2
                              (Measure-sig-figs m)))]))]
    [([n : (U Number (Vectorof Real))] [u1 : Unitish] u2)
     (convert (make-Measure n u1) u2)]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(untyped-module*
 [#:begin (require "unit-struct.rkt")]
 [make-Measure (->* [(U Number (Vectorof Real)) Unitish] [Sig-Figs] Measure)]
 [->measure (-> Measureish Measure)]
 [convert (case-> [-> Measure Unitish Measure]
                  [-> (U Number (Vectorof Real)) Unitish Unitish Measure])]
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
