#lang racket

(provide m m+ m- m* m1/ mexpt)

(require (submod "dimension-struct.rkt" untyped)
         (submod "dimension-operations.rkt" untyped)
         (submod "unit-struct.rkt" untyped)
         (submod "unit-operations.rkt" untyped)
         (submod "measure-struct.rkt" untyped)
         (submod "physical-constants.rkt" untyped)
         (submod "typed-operations.rkt" untyped)
         (only-in typed/racket/base assert)
         (for-syntax racket/base
                     syntax/parse))

(module+ test
  (require rackunit
           (submod "units.rkt" untyped)))

(begin-for-syntax
  (define-syntax-class mexpr #:description "non-operation expression"
    #:attributes (norm) #:datum-literals (+ - * / ^)
    [pattern (~and expr:expr (~not (~or + - * / ^)))
             #:with norm #'expr])
  (define-splicing-syntax-class msum #:description "a math expression"
    #:attributes (norm)
    [pattern (~seq (~or a:mproduct a:+-mproduct)) #:with norm #'a.norm]
    [pattern (~seq (~or a:mproduct a:+-mproduct) b:+-mproduct ...+)
             #:with norm #'(m+ a.norm b.norm ...)])
  (define-splicing-syntax-class mproduct #:description "a math expression without + or -"
    #:attributes (norm)
    [pattern (~seq) #:with norm #'1-measure]
    [pattern (~seq a:mexpt) #:with norm #'a.norm]
    [pattern (~seq a:mexpt b:*/mexpt ...) #:with norm #'(m* a.norm b.norm ...)])
  (define-splicing-syntax-class mexpt #:description "a math expression without +, -, *, or /"
    #:attributes (norm) #:datum-literals (^)
    [pattern (~seq a:mexpr) #:with norm #'a.norm]
    [pattern (~seq a:mexpr ^ b:mexpr) #:with norm #'(mexpt a.norm b.norm)])
  
  (define-splicing-syntax-class +-mproduct #:description "an expression with a + or -"
    #:attributes (norm) #:datum-literals (+ -)
    [pattern (~seq + a:mproduct) #:with norm #'a.norm]
    [pattern (~seq - a:mproduct) #:with norm #'(m- a.norm)])
  (define-splicing-syntax-class */mexpt #:description "an expression with a * or /"
    #:attributes (norm) #:datum-literals (* /)
    [pattern (~seq a:mexpt) #:with norm #'a.norm]
    [pattern (~seq * a:mexpt) #:with norm #'a.norm]
    [pattern (~seq / a:mexpt) #:with norm #'(m1/ a.norm)])
  )

(define-syntax m
  (syntax-parser
    [(m a:msum) #'a.norm]))

;; m* : [Measureish * -> Measure]
(define (m* . args)
  (let ([args (map ->measure args)])
    (define (vector-measure? m)
      (vector? (Measure-number m)))
    (define-values (vectors scalars)
      (partition vector-measure? args))
    (match vectors
      [(list)
       (apply m*/scalar scalars)]
      [(list v)
       (m*/vector (apply m*/scalar scalars) v)]
      [vectors
       (error 'm*
              (string-append
               "can't multiply 2 or more vectors together" "\n"
               "  use mdot or mcross instead" "\n"
               "  given: ~v")
              vectors)])))

;; m*/scalar : [Number-Measure * -> Number-Measure]
;; Note: accepts Number-Measure, not Number-Measureish
(define (m*/scalar . args)
  (match-define (list (measure ns us sfs) ...) args)
  (measure (apply * ns)
           (apply u* us)
           (apply sig-fig-min sfs)))

;; m*/vector : [Number-Measure Vector-Measure -> Vector-Measure]
;; Note: accepts _-Measure, not _-Measureish
(define (m*/vector nm vm)
  (define vm.v (Measure-number vm))
  (define nm.n (assert (Measure-number nm) real?))
  (measure (vector->immutable-vector
            (for/vector #:length (vector-length vm.v) #:fill 0
              ([v-i (in-vector vm.v)])
              (* nm.n v-i)))
           (u* (Measure-unit nm)
               (Measure-unit vm))
           (sig-fig-min (Measure-sig-figs nm)
                        (Measure-sig-figs vm))))



;; mexpt : [Number-Measureish Number-Measureish -> Number-Measure]
(define (mexpt b e)
  (let ([b (assert (->measure b) number-measure?)]
        [e (assert (->measure e) number-measure?)])
    (define n
      (Measure-number (convert e 1-unit)))
    (measure (expt (Measure-number b) n)
             (uexpt (Measure-unit b) (inexact->exact n))
             (sig-fig-min (Measure-sig-figs b)
                          (Measure-sig-figs e)))))

;; m1/ : [Measure -> Measure]
;; only the one-argument case, which takes the multiplicative inverse
(define (m1/ m)
  (mexpt m -1))


(module+ test
  (define-check (check-m=? m1 m2)
    (check m=? m1 m2))
  (check-m=? (m) 1-measure)
  (check-m=? (m 1 meter) (make-Measure 1 meter))
  (check-m=? (convert (m 1 meter) centimeter) (m 100 centimeter))
  (check-m=? (m 1 meter + 50 centimeter) (m (+ 1 1/2) meter))
  (check-m=? (m 1 meter - 50 centimeter) (m 1/2 meter))
  (check-m=? (m 1 meter ^ 2) (m 1 square-meter))
  (check-m=? (m 2 meter ^ 2) (m 2 square-meter))
  (check-m=? (m (m 2 meter) ^ 2) (m 4 square-meter))
  (check-m=? (m 1 kilogram meter / second ^ 2) (m 1 newton))
  (check-m=? (m 1 newton meter) (m 1 joule))
  
  )
