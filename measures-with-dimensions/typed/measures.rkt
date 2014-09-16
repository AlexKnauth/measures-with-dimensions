#lang typed/racket

(provide (all-defined-out)
         (all-from-out "units.rkt")
         )

(require racket/splicing
         math/bigfloat
         (only-in plot/typed/utils
                  [v* plot:v*]))

(require "units.rkt")
(require "vector-opperations.rkt")

(define-type Sig-Figs (U Positive-Integer +inf.0))
(define sig-figs? (make-predicate Sig-Figs))

(define-type Measure
  (Measureof (U Number (Vectorof Real)) Unit))

(define-type (Measureof n u)
  (pmeasure n u Sig-Figs))

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
     Dimensionish
     VectorTop))

(define-type Number-Measureish
  (U Number-Measure
     Number
     Unitish
     Dimensionish))

(define-type Vector-Measureish
  (U Vector-Measure
     VectorTop))

(struct: (n u sfs) pmeasure ([number : n]
                             [unit : u]
                             [sig-figs : sfs]) ; for binary
  #:transparent
  #:guard (lambda (number unit sig-figs _)
            (unless (or (number? number)
                        (and (vector? number)
                             (for/and : Boolean ([n : Any (in-vector number)])
                               (real? n))))
              (error 'pmeasure "expected (or/c number? (vectorof real?)), given ~v" number))
            (unless (unit? unit)
              (error 'pmeasure "expected unit? for unit argument, given ~v" unit))
            (unless (sig-figs? sig-figs)
              (error 'pmeasure "expected sig-figs? for sig-figs argument, given ~v" sig-figs))
            (values number unit sig-figs))
  #:property prop:custom-write (lambda (m out mode)
                                 (cond [(zero? mode) (display "(measure " out)
                                                     (print (pmeasure-number m) out)
                                                     (display " " out)
                                                     (print (pmeasure-unit m) out)
                                                     (display ")" out)]
                                       [else (display (pmeasure-number m) out)
                                             (display "*" out)
                                             (display (pmeasure-unit m) out)])))



(define number-measure? (make-predicate Number-Measure))
(define number-measureish? (make-predicate Number-Measureish))



(: ->measure : (All (n a b c d e sfs)
                    (case-> [(pmeasure n (Unitof (pdimension a b c d e)) sfs)
                             -> (pmeasure n (Unitof (pdimension a b c d e)) sfs)]
                            [(Unitof (pdimension a b c d e))
                             -> (pmeasure 1 (Unitof (pdimension a b c d e)) +inf.0)]
                            [(pdimension a b c d e)
                             -> (pmeasure 1 (Unitof (pdimension a b c d e)) +inf.0)]
                            [Number-Measure -> Number-Measure]
                            [Vector-Measure -> Vector-Measure]
                            [Measureish -> Measure])))
(define (->measure m)
  (cond [(pmeasure? m) m]
        [(p-unit? m) (pmeasure 1 m +inf.0)]
        [(pdimension? m) (pmeasure 1 (p-unit m 1 m) +inf.0)]
        [(number? m) (number->measure m)]
        [(vector? m) (vector->measure m)]
        [(unitish? m) ((inst pmeasure 1 Unit +inf.0) 1 (->unit m) +inf.0)]
        ;[else (error '->measure "could not convert to a measure. given: ~v" m)]
        ))

(: number->measure : [Number -> Number-Measure])
(define (number->measure n)
  (cond [(exact? n) (pmeasure n 1-unit +inf.0)]
        [(double-flonum? n) (pmeasure n 1-unit 53)] ; FIXME: shouldn't count end zeros
        [(single-flonum? n) (pmeasure n 1-unit 52/2)] ; what should this be?
        [else (pmeasure n 1-unit +inf.0)]))


(: vector->measure : [VectorTop -> Vector-Measure])
(define (vector->measure v)
  (: v.length : Index)
  (define v.length (vector-length v))
  (cond [(zero? v.length)
         (pmeasure (ann #() (Vectorof Real)) 1-unit +inf.0)]
        [else
         (let* ([ms : (Listof Measure)
                    (for/list : (Listof Measure) ([m : Any (in-vector v)])
                      (->measure (cast m Measureish)))]
                [m0 : Measure (first ms)]
                [u : Unit (pmeasure-unit m0)]
                [d : Dimension (p-unit-dimension u)])
           (: ns : (Listof Real))
           (define ns
             (for/list : (Listof Real) ([m : Measure (in-list ms)])
               (unless (dimension=? (pmeasure-dimension m) d)
                 (error 'vector->measure "incompatible units within vector"))
               (define n (pmeasure-number (convert m u)))
               (unless (real? n)
                 (error 'vector->measure "vector contains a non-real number in: ~v" n))
               n))
           (: new-v : (Vectorof Real))
           (define new-v
             (apply (inst vector-immutable Real) ns))
           (: sig-figs : Sig-Figs)
           (define sig-figs (cast (apply min (map measure-sig-figs ms)) Sig-Figs))
           (pmeasure new-v u sig-figs))]))

(: base2-sig-figs->base10-sig-figs : [Sig-Figs -> Sig-Figs])
(splicing-local [(define factor
                   (/ (log 10) (log 2)))]
  (define (base2-sig-figs->base10-sig-figs n)
    (if (equal? n +inf.0)
        +inf.0
        (cast (exact-floor (/ n factor)) Sig-Figs))))



(: pmeasure-dimension : (All (d) [(pmeasure (U Number (Vectorof Real)) (Unitof d) Sig-Figs) -> d]))
(define (pmeasure-dimension m)
  (p-unit-dimension (pmeasure-unit m)))

(: measure-number : [Measure -> (U Number (Vectorof Real))])
(: measure-unit : [Measure -> Unit])
(: measure-dimension : [Measure -> Dimension])
(: measure-sig-figs : [Measure -> Sig-Figs])

(: measure : (case-> [Number Unitish [#:sig-figs Sig-Figs] -> Number-Measure]
                     [(Vectorof Real) Unitish [#:sig-figs Sig-Figs] -> Vector-Measure]
                     [(U Number (Vectorof Real)) Unitish [#:sig-figs Sig-Figs] -> Measure]))
(define (measure num u #:sig-figs [sf +inf.0])
  (pmeasure num (->unit u) sf))
(define (measure-number m)
  (pmeasure-number (->measure m)))
(define (measure-unit m)
  (pmeasure-unit (->measure m)))
(define (measure-dimension m)
  (p-unit-dimension (measure-unit m)))
(define (measure-sig-figs m)
  (pmeasure-sig-figs (->measure m)))

(: measure=? : [Measureish Measureish * -> Boolean])
(define (measure=? m . rst)
  (let* ([m : Measure (->measure m)]
         [m.number : (U Number (Vectorof Real)) (pmeasure-number m)]
         [m.unit : Unit (pmeasure-unit m)]
         [m.dimension : Dimension (p-unit-dimension m.unit)])
    (cond [(number? m.number) (let: ([m.number*u.scalar : Number (* m.number (unit-scalar m.unit))])
                                (for/and: : Boolean ([m2 : Measureish (in-list rst)])
                                  (let* ([m2 : Measure (->measure m2)]
                                         [m2.number : (U Number (Vectorof Real)) (measure-number m2)]
                                         [m2.unit : Unit (measure-unit m2)])
                                    (and (number? m2.number)
                                         (let: ([m2.number*u2.scalar : Number (* m2.number (unit-scalar m2.unit))]
                                                [m2.dimension : Dimension (unit-dimension m2.unit)])
                                           (and (= m.number*u.scalar m2.number*u2.scalar)
                                                (dimension=? m.dimension m2.dimension)))))))]
          [(vector? m.number) (let: ([m.number*u.scalar : (Vectorof Real) (plot:v* m.number (unit-scalar m.unit))])
                                (for/and: : Boolean ([m2 : Measureish (in-list rst)])
                                  (let* ([m2 : Measure (->measure m2)]
                                         [m2.number : (U Number (Vectorof Real)) (measure-number m2)]
                                         [m2.unit : Unit (measure-unit m2)])
                                    (and (vector? m2.number)
                                         (let: ([m2.number*u2.scalar : (Vectorof Real) (plot:v* m2.number (unit-scalar m2.unit))]
                                                [m2.dimension : Dimension (unit-dimension m2.unit)])
                                           (and (v=? m.number*u.scalar m2.number*u2.scalar)
                                                (dimension=? m.dimension m2.dimension)))))))]
          )))


(: convert : (case-> [Number-Measure Unitish -> Number-Measure]
                     [Measure Unitish -> Measure]
                     [(U Number (Vectorof Real)) Unitish Unitish -> Measure]))
(define convert
  (case-lambda
    [(m u2)
     (let ([u2 : Unit (->unit u2)])
       (unless (dimension=? (measure-dimension m) (unit-dimension u2))
         (error 'convert "cannot convert between dimensions, given: (convert ~v ~v)" m u2))
       (cond [(number-measure? m) (let: ([num : Number (cast (measure-number m) Number)]
                                         [u1 : Unit (measure-unit m)])
                                    (measure (* num (/ (unit-scalar u1)
                                                       (unit-scalar u2)))
                                             u2
                                             #:sig-figs (measure-sig-figs m)))]
             [else (let: ([num : (Vectorof Real) (cast (measure-number m) (Vectorof Real))]
                          [u1 : Unit (measure-unit m)])
                     (measure (plot:v* num (/ (unit-scalar u1)
                                              (unit-scalar u2)))
                              u2
                              #:sig-figs (measure-sig-figs m)))]))]
    [([n : (U Number (Vectorof Real))] [u1 : Unitish] [u2 : Unitish])
     (convert (measure n u1) u2)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type Position
  (Measureof (U Real (Vectorof Real)) Length-Unit))
(define-type Length
  (Measureof Nonnegative-Real Length-Unit))
(define-type Position-Vector
  (Measureof (Vectorof Real) Length-Unit))
(define-type Mass
  (Measureof Nonnegative-Real Mass-Unit))
(define-type Time
  (Measureof Real Time-Unit))
(define-type Charge
  (Measureof Real Charge-Unit))
(define-type Absolute-Temperature
  (Measureof Nonnegative-Real Absolute-Temperature-Unit))

(define-type Area
  (Measureof Nonnegative-Real Area-Unit))
(define-type Volume
  (Measureof Nonnegative-Real Volume-Unit))

(define-type Mass-Density
  (Measureof Nonnegative-Real Mass-Density-Unit))
(define-type Charge-Density
  (Measureof Real Charge-Density-Unit))

(define-type Velocity
  (Measureof (U Real (Vectorof Real)) Velocity-Unit))
(define-type Speed
  (Measureof Nonnegative-Real Speed-Unit))
(define-type Velocity-Vector
  (Measureof (Vectorof Real) Velocity-Unit))
(define-type Acceleration
  (Measureof (U Real (Vectorof Real)) Acceleration-Unit))
(define-type Acceleration-Vector
  (Measureof (Vectorof Real) Acceleration-Unit))
(define-type Force
  (Measureof (U Real (Vectorof Real)) Force-Unit))
(define-type Force-Vector
  (Measureof (Vectorof Real) Force-Unit))
(define-type Momentum
  (Measureof (U Real (Vectorof Real)) Momentum-Unit))
(define-type Momentum-Vector
  (Measureof (Vectorof Real) Momentum-Unit))

(define-type Energy
  (Measureof Real Energy-Unit))
(define-type Work Energy)
(define-type Power
  (Measureof Real Power-Unit))

(define-type Pressure
  (Measureof Nonnegative-Real Pressure-Unit))

(define-type Entropy
  (Measureof Real Entropy-Unit))

(define-type Electric-Field
  (Measureof (U Real (Vectorof Real)) Electric-Field-Unit))
(define-type Electric-Field-Vector
  (Measureof (Vectorof Real) Electric-Field-Unit))
(define-type Electric-Potential
  (Measureof Real Electric-Potential-Unit))
(define-type Voltage Electric-Potential)
(define-type Emf Voltage)
(define-type Capacitance
  (Measureof Nonnegative-Real Capacitance-Unit))

(define-type Current
  (Measureof Real Current-Unit))
(define-type Current-Density
  (Measureof (U Real (Vectorof Real)) Current-Density-Unit))
(define-type Current-Density-Vector
  (Measureof (Vectorof Real) Current-Density-Unit))
(define-type Resistance
  (Measureof Nonnegative-Real Resistance-Unit))
(define-type Resistivity
  (Measureof Nonnegative-Real Resistivity-Unit))
(define-type Conductivity
  (Measureof Nonnegative-Real Conductivity-Unit))

(define-type Magnetic-Field
  (Measureof (U Real (Vectorof Real)) Magnetic-Field-Unit))
(define-type Magnetic-Field-Vector
  (Measureof (Vectorof Real) Magnetic-Field-Unit))

(define-type Electric-Flux
  (Measureof Real Electric-Flux-Unit))
(define-type Magnetic-Flux
  (Measureof Real Magnetic-Flux-Unit))

(define-type Inductance
  (Measureof Nonnegative-Real Inductance-Unit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Some Physical Constants:

(: g : Acceleration)
(define g  ; the acceleration due to gravity on earth
  (pmeasure #e9.80665 m/s^2 +inf.0))

(: G : Number-Measure)
(define G  ; Newton's universal gravitational constant, 
  #;()    #; (for use in F = (* G (/ (* m-1 m-2)
                                     (sqr d))))
  (measure (* #i6.7384 (10^ -11))
           (u* newton (u/ (usqr meter)
                          (usqr kilogram)))))

(: c : Speed)
(define c  ; the speed of light in a vacuum
  (pmeasure #e299792458 m/s +inf.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module* untyped racket/base
  (provide (all-from-out (submod "..")) measure)
  (require (except-in (submod "..") measure))
  (require typed/untyped-utils)
  (require/untyped-contract (begin (require (except-in (submod ".." "..") measure)))
                            (submod ".." "..")
                            [measure ((U Number (Vectorof Real)) Unitish [#:sig-figs Sig-Figs]
                                                                 -> Measure)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tests:

(module* test racket/base
  (require (submod ".." untyped))
  (require rackunit)
  (check measure=?
         (measure 1    meter)
         (measure 100  centimeter))
  (check measure=?
         (measure 1    meter)
         (measure 1000 millimeter))
  )
