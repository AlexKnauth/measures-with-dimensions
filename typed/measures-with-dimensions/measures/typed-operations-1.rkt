#lang typed/racket

(provide m+ m+/lenient m- m1/ mexpt m*/scalar m*/vector m*)

(require "../dimensions/dimension-struct.rkt"
         "../dimensions/dimension-operations.rkt"
         "../units/unit-struct.rkt"
         "../units/unit-operations.rkt"
         "measure-struct.rkt"
         "0-1-measures.rkt"
         "../vector-operations.rkt"
         "../preds.rkt"
         "../untyped-utils.rkt")

(: measure->real-measure : (All (d) [(Measureof Any (Unitof d)) -> (Measureof Real (Unitof d))]))
(define (measure->real-measure m)
  (struct-copy measure m
               [number (assert (measure-number m) real?)]))

(: m+ : (All (d) (case-> [-> (Measureof 0 Dimensionless-Unit)]
                         [-> (U (Measureof Real (Unitof d)) (Unitof d))
                             (U (Measureof Real (Unitof d)) (Unitof d)) *
                             (Measureof Real (Unitof d))]
                         [-> (U (Measureof Number (Unitof d)) (Unitof d))
                             (U (Measureof Number (Unitof d)) (Unitof d)) *
                             (Measureof Number (Unitof d))]
                         [-> (Measureof Vec (Unitof d))
                             (Measureof Vec (Unitof d)) *
                             (Measureof Vec (Unitof d))]
                         [-> (Measureof Num/Vec (Unitof d))
                             (Measureof Num/Vec (Unitof d)) *
                             (Measureof Num/Vec (Unitof d))]
                         [-> Real Real * (Measureof Real Dimensionless-Unit)]
                         [-> Number Number * (Measureof Number Dimensionless-Unit)]
                         [-> VectorTop VectorTop * Vector-Measure]
                         [-> Measureish Measureish * Measure]
                         [-> Measureish * Measure]
                         )))
(define m+
  (case-lambda
    [() 0-measure]
    [(m1 . rst)
     (define m1* : Measureish (cast m1 Measureish))
     (define rst* (cast rst (Listof Measureish)))
     (cond [(number? m1)
            (apply (inst m+/lenient Dimensionless-Dimension) (number->measure m1) rst*)]
           [(vector? m1)
            (apply m+/lenient (vector->measure m1) rst*)]
           [(unit? m1)
            (apply m+/lenient (->measure m1) rst*)]
           [(dimension? m1)
            (apply m+/lenient (->measure (->unit m1)) rst*)]
           [(measure? m1*)
            (apply m+/lenient m1 rst*)]
           [else (error 'm+ "expected Measure, given ~v" m1)])]
    [ms (cond [(empty? ms) 0-measure]
              [else (apply m+ (first ms) (rest ms))])]))

(: m+/lenient : (All (d) (case-> [-> (Measureof 0 Dimensionless-Unit)]
                                 [-> (Measureof Real (Unitof d)) Measureish *
                                     (Measureof Real (Unitof d))]
                                 [-> (Measureof Number (Unitof d)) Measureish *
                                     (Measureof Number (Unitof d))]
                                 [-> (Measureof Vec (Unitof d)) Measureish *
                                     (Measureof Vec (Unitof d))]
                                 [-> (Measureof Num/Vec (Unitof d)) Measureish *
                                     (Measureof Num/Vec (Unitof d))])))
(define m+/lenient
  (case-lambda
    [() 0-measure]
    [(m1 . rst)
     (let* ([rst : (Listof Measure) (map ->measure rst)])
       (: u : (Unitof d))
       (define u (measure-unit m1))
       (: d : d)
       (define d (unit-dimension u))
       (define n1 (measure-number m1))
       (cond [(real? n1)
              (measure->real-measure
               (apply (inst m+/scalar d) m1 (cast rst (Listof Number-Measure))))]
             [(number? n1)
              (apply (inst m+/scalar d) m1 (cast rst (Listof Number-Measure)))]
             [else
              (apply (inst m+/vector d) m1 (cast rst (Listof Vector-Measure)))]))]))



(: m+/scalar : (All (d) [-> (Measureof Num/Vec (Unitof d)) Number-Measure *
                            (Measureof Number (Unitof d))]))
(define (m+/scalar m1 . rst)
  (: u : (Unitof d))
  (define u (measure-unit m1))
  (: u-Unit : Unit)
  (define u-Unit (assert u Unit?))
  (: d : d)
  (define d (unit-dimension u))
  (: d-Dim : Dimension)
  (define d-Dim (assert d Dimension?))
  (: sig-figs : Sig-Figs)
  (define sig-figs (apply sig-fig-min
                          (measure-sig-figs m1)
                          (map (inst measure-sig-figs Number Unit Sig-Figs) rst)))
  (: n : Number)
  (define n
    (for/sum : Number ([m : Number-Measure (in-list (cons (assert m1 number-measure?) rst))])
      (unless (dimension=? (measure-dimension m) d-Dim)
        (error 'm+ (string-append
                    "can't add two measures with different dimensions" "\n"
                    "  given ~v and ~v") m1 m))
      (define mc (convert m u-Unit))
      (define n (Measure-number mc))
      (unless (number? n)
        (error 'm+ (string-append "can't add a number and a vector" "\n"
                                  "  given ~v and ~v") m1 mc))
      n))
  (measure n u sig-figs))



(: m+/vector : (All (d) [-> (Measureof Num/Vec (Unitof d)) Vector-Measure *
                            (Measureof Vec (Unitof d))]))
(define (m+/vector m1 . rst)
  (: u : (Unitof d))
  (define u (measure-unit m1))
  (: u-Unit : Unit)
  (define u-Unit (assert u Unit?))
  (: d : d)
  (define d (unit-dimension u))
  (: d-Dim : Dimension)
  (define d-Dim (assert d Dimension?))
  (: sig-figs : Sig-Figs)
  (define sig-figs (apply sig-fig-min
                          (measure-sig-figs m1)
                          (map (inst measure-sig-figs Vec Unit Sig-Figs) rst)))
  (: vs : (Listof Vec))
  (define vs
    (for/list : (Listof Vec)
      ([m : Vector-Measure (in-list (cons (cast m1 Vector-Measure) rst))])
      (unless (dimension=? (measure-dimension m) d-Dim)
        (error 'm+ (string-append
                    "can't add two measures with different dimensions" "\n"
                    "  given ~v and ~v") m1 m))
      (define mc (convert m u-Unit))
      (define n (Measure-number mc))
      (unless (vector? n)
        (error 'm+ (string-append "can't add a number and a vector" "\n"
                                  "  given ~v and ~v") m1 mc))
      n))
  (: length : Nonnegative-Fixnum)
  (define length
    (apply max (map vector-length vs)))
  (: v : Vec)
  (define v
    (vector->immutable-vector
            (for/vector : Vec #:length length #:fill 0
              ([i : Nonnegative-Integer (in-range length)])
              (for/sum : Real ([v : Vec (in-list vs)])
                (if (<= (sub1 (vector-length v)) i)
                    (vector-ref v i)
                    0)))))
  (measure v u sig-figs))



(: m- : (All (d) (case-> [-> (U (Measureof Real (Unitof d)) (Unitof d))
                             (Measureof Real (Unitof d))]
                         [-> (U (Measureof Number (Unitof d)) (Unitof d))
                             (Measureof Number (Unitof d))]
                         [-> (Measureof Vec (Unitof d))
                             (Measureof Vec (Unitof d))]
                         [-> (U (Measureof Num/Vec (Unitof d)) (Unitof d))
                             (Measureof Num/Vec (Unitof d))]
                         [-> Real (Measureof Real Dimensionless-Unit)]
                         [-> Number (Measureof Number Dimensionless-Unit)]
                         [-> Vec Vector-Measure]
                         [-> Measureish Measure]
                         )))
(define (m- m)
  (cond [(measure? m) (m-/measure m)]
        [(unit? m)    (m-/measure (->measure m))]
        [(number? m)  (m-/measure (number->measure m))]
        [(vector? m)  (m-/measure (vector->measure m))]
        [else         (m-/measure (->measure m))]))

(: m-/measure : (All (d) (case-> [-> (Measureof Real (Unitof d))
                                     (Measureof Real (Unitof d))]
                                 [-> (Measureof Number (Unitof d))
                                     (Measureof Number (Unitof d))]
                                 [-> (Measureof Vec (Unitof d))
                                     (Measureof Vec (Unitof d))]
                                 [-> (Measureof Num/Vec (Unitof d))
                                     (Measureof Num/Vec (Unitof d))])))
(define (m-/measure m)
  (: u : (Unitof d))
  (define u (measure-unit m))
  (define n (measure-number m))
  (: sig-figs : Sig-Figs)
  (define sig-figs (measure-sig-figs m))
  (cond [(number? n)
         (measure (- n) u sig-figs)]
        [else
         (measure (v* -1 n) u sig-figs)]))

(: m1/ : [-> Number-Measureish Number-Measure])
(define (m1/ m)
  (let ([m (assert (->measure m) number-measure?)])
    (: u : Unit)
    (define u (measure-unit m))
    (: n : Number)
    (define n (measure-number m))
    (: sig-figs : Sig-Figs)
    (define sig-figs (measure-sig-figs m))
    (measure (/ n)
             (u1/ u)
             sig-figs)))

(: mexpt : [-> Number-Measureish Number-Measureish Number-Measure])
(define (mexpt b e)
  (let ([b (assert (->measure b) number-measure?)]
        [e (assert (->measure e) number-measure?)])
    (: n : Number)
    (define n
      (assert (measure-number (convert e 1-unit))
              number?))
    (measure (expt (measure-number b) n)
             (uexpt (Measure-unit b) (assert (inexact->exact n) exact-rational?))
             (sig-fig-min (Measure-sig-figs b)
                          (Measure-sig-figs e)))))

(: m*/scalar : [Number-Measure * -> Number-Measure])
;; Note: accepts Number-Measure, not Number-Measureish
(define (m*/scalar . args)
  (define-values (ns us sfs)
    (for/lists ([ns : (Listof Number)] [us : (Listof Unit)] [sfs : (Listof Sig-Figs)])
      ([m : Number-Measure (in-list args)])
      (values (measure-number m)
              (measure-unit m)
              (measure-sig-figs m))))
  (measure (apply * ns)
           (apply u* us)
           (apply sig-fig-min sfs)))

(: m*/vector : [Number-Measure Vector-Measure -> Vector-Measure])
;; Note: accepts _-Measure, not _-Measureish
(define (m*/vector nm vm)
  (: vm.v : Vec)
  (define vm.v (measure-number vm))
  (: nm.n : Real)
  (define nm.n (assert (measure-number nm) real?))
  (measure (v* nm.n vm.v)
           (u* (Measure-unit nm)
               (Measure-unit vm))
           (sig-fig-min (Measure-sig-figs nm)
                        (Measure-sig-figs vm))))

(: m*/no-special-case : (case-> [Number-Measure * -> Number-Measure]
                                [Measureish * -> Measure]))
(define (m*/no-special-case . args)
  (let ([args (map ->measure args)])
    (: vector-measure? : [Measure -> Boolean])
    (define (vector-measure? m)
      (vector? (measure-number m)))
    (define-values (vectors scalars)
      (partition vector-measure? args))
    (define scalars*
      (for/list : (Listof Number-Measure) ([scalar (in-list scalars)])
        (assert scalar number-measure?)))
    (match vectors
      [(list)
       (apply m*/scalar scalars*)]
      [(list v)
       (when (andmap number-measureish? args) (error 'm* "this should never happen"))
       (m*/vector (apply m*/scalar scalars*) (cast v Vector-Measure))]
      [vectors
       (error 'm*
              (string-append
               "can't multiply 2 or more vectors together" "\n"
               "  use mdot or mcross instead" "\n"
               "  given: ~v")
              vectors)])))

(: m* : (All (d) (case-> [Real (Unitof d) -> (Measureof Real (Unitof d))]
                         [Number (Unitof d) -> (Measureof Number (Unitof d))]
                         [Vec (Unitof d) -> (Measureof Vec (Unitof d))]
                         [Number-Measure * -> Number-Measure]
                         [Measureish * -> Measure])))
(define m*
  (case-lambda
    [(n u)
     (cond [(unit? u)
            (cond [(number? n) (make-measure n u)]
                  [(vector? n)
                   (make-measure (cast n Vec) u)]
                  [else
                   (m*/no-special-case n (assert u Unit?))])]
           [else (m*/no-special-case n u)])]
    [args (apply m*/no-special-case args)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(untyped-module*
 [#:begin (require "../units/unit-struct.rkt" "measure-struct.rkt")]
 [m* [Measureish * -> Measure]]
 [m+ [Measureish * -> Measure]]
 [m- [Measureish -> Measure]]
 )


