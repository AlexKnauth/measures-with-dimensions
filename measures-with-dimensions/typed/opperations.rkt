#lang typed/racket

(provide (all-defined-out)
         (all-from-out "measures.rkt"))

(require "measures.rkt")
(require "vector-opperations.rkt")

(define real-zero? (make-predicate Real-Zero))

(: mexpt (Number-Measure Exact-Rational -> Number-Measure))
(define (mexpt m n)
  (cond [(number? m) (expt m n)]
        [else
         (let: ([m.number : Number (cast (measure-number m) Number)]
                [m.unit : Unit (measure-unit m)])
           (measure (expt m.number n)
                    (uexpt m.unit n)
                    #:sig-figs (measure-sig-figs m)))]))

(: msqr (case-> (Zero -> Zero) (One -> One)
                (Positive-Byte -> Positive-Index) (Byte -> Index)
                (Positive-Integer -> Positive-Integer) (Integer -> Nonnegative-Integer)
                (Positive-Exact-Rational -> Positive-Exact-Rational) (Exact-Rational -> Nonnegative-Exact-Rational)
                (Flonum -> Nonnegative-Flonum) (Single-Flonum -> Nonnegative-Single-Flonum)
                (Inexact-Real -> Nonnegative-Inexact-Real) (Real -> Nonnegative-Real)
                (Float-Complex -> Float-Complex) (Single-Flonum-Complex -> Single-Flonum-Complex)
                (Inexact-Complex -> Inexact-Complex) (Exact-Number -> Exact-Number)
                (Number -> Number)
                ((Vectorof Real) -> Nonnegative-Real)
                (Measure -> Number-Measure)))
(define (msqr m)
  (cond [(number? m) (sqr m)]
        [(vector? m) (vsqr (cast m (Vectorof Real)))]
        [else
         (let: ([m.number : (U Number (Vectorof Real)) (measure-number m)]
                [m.unit : Unit (measure-unit m)])
           (measure (cond [(number? m.number) (sqr m.number)]
                          [(vector? m.number) (vsqr m.number)])
                    (usqr m.unit)
                    #:sig-figs (measure-sig-figs m)))]))

(: msqrt (Number-Measure -> Number-Measure))
(define (msqrt m)
  (cond [(number? m) (sqrt m)]
        [else
         (let: ([m.number : Number (cast (measure-number m) Number)]
                [m.unit : Unit (measure-unit m)])
           (measure (sqrt m.number)
                    (usqrt m.unit)
                    #:sig-figs (measure-sig-figs m)))]))

(: mproduct (case-> [(Listof Number) -> (NormalizedMeasureof Number dimmensionless-dimmension)]
                    [(Listof NumberMeasure) -> NormalizedNumberMeasure]
                    [(Rec Args (U (Cons (Vectorof Real) (Listof Number))
                                  (Cons Number Args)))
                     -> (NormalizedMeasureof (Vectorof Real) dimmensionless-dimmension)]
                    [(Rec Args (U (Cons VectorMeasure (Listof NumberMeasure))
                                  (Cons NumberMeasure Args)))
                     -> (NormalizedMeasureof VectorMeasure NormalizedDimmension)]
                    ))
(define (mproduct args)
  (cond [(empty? args) (normalize-measure 1)]
        [(andmap number? args) (normalize-measure (apply * args))]
        ))

#|
(: m+ (case-> (-> Zero)
              (Number * -> Number)
              (Number-Measure * -> Number-Measure)
              (Vector-Measure Vector-Measure * -> Vector-Measure)
              (Vector-Measure * -> (U Vector-Measure Zero))
              #;(Measure * -> Measure)))
(define m+
  (case-lambda
    [() 1]
    [args (define listof-number? (make-predicate (Listof Number)))
          (define listof-number-measure? (make-predicate (Listof Number-Measure)))
          (cond [(empty? args) 0]
                [(listof-number? args) (apply + args)]
                [(listof-number-measure? args) (apply nm+ args)]
                [else (apply vm+ (cast args (Pairof Vector-Measure (Listof Vector-Measure))))])]))

(: nm+ (Number-Measure * -> Number-Measure))
(define (nm+ . args)
  (cond [(empty? args) 0]
        [(andmap number? args) (apply + args)]
        [else (let: ([m1 : Number-Measure (first args)]
                     [rst : (Listof Number-Measure) (rest args)])
                (let: ([n1 : Number (cast (measure-number m1) Number)]
                       [u1 : Unit (measure-unit m1)]
                       [sf1 : (U Positive-Integer +inf.0 #f) (measure-sig-figs m1)])
                  (let-values: ([([ns : (Listof Number)] [sig-figs : (U Positive-Integer +inf.0)])
                                 (for/fold: ([ns : (Listof Number) (list n1)]
                                             [sig-figs : (U Positive-Integer +inf.0) (if sf1 sf1 +inf.0)])
                                   ([m2 : Number-Measure (in-list rst)])
                                   (let: ([n2 : Number (cast (measure-number (convert m2 u1)) Number)])
                                     (values (cond [(real-zero? n2) ns]
                                                   [else (cons n2 ns)])
                                             (let: ([sf2 : (U Positive-Integer +inf.0 #f) (measure-sig-figs m2)])
                                               (if sf2 (cast (min sig-figs sf2) Positive-Integer) sig-figs)))))])
                    (measure (apply + ns)
                             u1
                             #:sig-figs (if (exact-positive-integer? sig-figs) sig-figs #f)))))]))

(: vm+ (Vector-Measure Vector-Measure * -> Vector-Measure))
(define (vm+ m1 . rst)
  (let: ([v1 : (Vectorof Real) (cast (measure-number m1) (Vectorof Real))]
         [u1 : Unit (measure-unit m1)]
         [sf1 : (U Positive-Integer +inf.0 #f) (measure-sig-figs m1)])
    (let-values: ([([vs : (Listof (Vectorof Real))] [length : Natural] [sig-figs : (U Positive-Integer +inf.0)])
                   (for/fold: ([vs : (Listof (Vectorof Real)) (list v1)]
                               [length : Natural (vector-length v1)]
                               [sig-figs : (U Positive-Integer +inf.0) (if sf1 sf1 +inf.0)])
                     ([m2 : Vector-Measure (in-list rst)])
                     (let: ([v2 : (Vectorof Real) (cast (measure-number (convert m2 u1)) (Vectorof Real))])
                       (values (cons v2 vs)
                               (max length (vector-length v2))
                               (let: ([sf2 : (U Positive-Integer +inf.0 #f) (measure-sig-figs m2)])
                                 (if sf2 (cast (min sig-figs sf2) Positive-Integer) sig-figs)))))])
      (measure (for/vector: : (Vectorof Real) #:length length ([i : Natural (in-range length)])
                 (for/sum: : Real ([v (in-list vs)])
                   (vector-ref_0 v i)))
               u1
               #:sig-figs (if (exact-positive-integer? sig-figs) sig-figs #f)))))
|#