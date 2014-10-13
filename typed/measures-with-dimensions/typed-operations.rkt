#lang typed/racket

(provide m+)

(require "dimension-struct.rkt"
         "dimension-operations.rkt"
         "unit-struct.rkt"
         "unit-operations.rkt"
         "measure-struct.rkt"
         "physical-constants.rkt"
         "untyped-utils.rkt")

(: m+ : (All (u) (case-> [-> (Measureof 0 Dimensionless-Unit)]
                         [-> (Measureof (U Number (Vectorof Real)) u) Measureish *
                             (Measureof (U Number (Vectorof Real)) u)])))
(define m+
  (case-lambda
    [() 0-measure]
    [(m1 . rst)
     (let* ([rst : (Listof Measure) (map ->measure rst)])
       (: u : u)
       (define u (measure-unit m1))
       (: u-Unit : Unit)
       (define u-Unit (assert u Unit?))
       (: d : Dimension)
       (define d (unit-dimension u-Unit))
       (define n1 (measure-number m1))
       (cond [(number? n1)
              (apply (inst m+/scalar u) m1 (cast rst (Listof Number-Measure)))]
             [else
              (apply (inst m+/vector u) m1 (cast rst (Listof Vector-Measure)))]))]))



(: m+/scalar : (All (u) [-> (Measureof (U Number (Vectorof Real)) u) Number-Measure *
                            (Measureof Number u)]))
(define (m+/scalar m1 . rst)
  (: u : u)
  (define u (measure-unit m1))
  (: u-Unit : Unit)
  (define u-Unit (assert u Unit?))
  (: d : Dimension)
  (define d (unit-dimension u-Unit))
  (: sig-figs : Sig-Figs)
  (define sig-figs (apply sig-fig-min
                          (measure-sig-figs m1)
                          (map (inst measure-sig-figs Number Unit Sig-Figs) rst)))
  (: n : Number)
  (define n
    (for/sum : Number ([m : Number-Measure (in-list (cons (assert m1 number-measure?) rst))])
      (unless (dimension=? (measure-dimension m) d)
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



(: m+/vector : (All (u) [-> (Measureof (U Number (Vectorof Real)) u) Vector-Measure *
                            (Measureof (Vectorof Real) u)]))
(define (m+/vector m1 . rst)
  (: u : u)
  (define u (measure-unit m1))
  (: u-Unit : Unit)
  (define u-Unit (assert u Unit?))
  (: d : Dimension)
  (define d (unit-dimension u-Unit))
  (: sig-figs : Sig-Figs)
  (define sig-figs (apply sig-fig-min
                          (measure-sig-figs m1)
                          (map (inst measure-sig-figs (Vectorof Real) Unit Sig-Figs) rst)))
  (: vs : (Listof (Vectorof Real)))
  (define vs
    (for/list : (Listof (Vectorof Real))
      ([m : Vector-Measure (in-list (cons (cast m1 Vector-Measure) rst))])
      (unless (dimension=? (measure-dimension m) d)
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
  (: v : (Vectorof Real))
  (define v
    (vector->immutable-vector
            (for/vector : (Vectorof Real) #:length length #:fill 0
              ([i : Nonnegative-Integer (in-range length)])
              (for/sum : Real ([v : (Vectorof Real) (in-list vs)])
                (if (<= (sub1 (vector-length v)) i)
                    (vector-ref v i)
                    0)))))
  (measure v u sig-figs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(untyped-module*
 )


