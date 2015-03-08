#lang racket/base

(require racket/local
         racket/vector
         racket/function
         racket/format
         (except-in racket/list second)
         racket/math
         (only-in lang/htdp-intermediate-lambda =~)
         typed/measures-with-dimensions/exact-tau-pi-eta
         typed/measures-with-dimensions/preds
         (except-in "../../../measures-with-dimensions/main.rkt" define-unit))

(module+ test
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define n*
  (local [(define scalar? number?)
          (define (scalar*vector s v)
            (vector-map (curry * s) v))]
    (lambda args
      (cond [(andmap scalar? args)
             (apply * args)]
            [(= 0 (length (filter vector? args)))
             (when (not (andmap number? args))
               (displayln (apply string-append 
                                 (cons "warning: n*: not all arguments are numbers. /n args:"
                                       (for/list ([arg args])
                                         (string-append " "(~v arg)""))))))
             (apply * args)]
            [(= 1 (length (filter vector? args)))
             (scalar*vector (apply * (filter-not vector? args))
                            (first (filter vector? args)))]
            [else
             (error "*: expects no more than one vector as an operand")]))))

(define n/
  (local [(define scalar? number?)
          (define (vector*scalar v s)
            (vector-map (curry * s) v))]
    (case-lambda
      [() 1]
      [(arg) (/ arg)]
      [args
       (cond [(andmap scalar? args)
              (apply / args)]
             [(and (vector? (first args))
                   (andmap scalar? (rest args)))
              (vector*scalar (first args)
                             (/ (apply * (rest args))))]
             [else
              (when (not (andmap number? args))
                (displayln (apply string-append 
                                  (cons "warning: n*: not all arguments are numbers. /n args:"
                                        (for/list ([arg args])
                                          (string-append " "(~v arg)""))))))
              (error "/: no vectors allowed in the denominator")])])))

(define average
  (lambda args
    (nwu/ (apply nwu+ args)
          (length args))))

(define (dot a b)
  (apply nwu+
         (for/list ([a-i a]
                    [b-i b])
           (nwu* a-i b-i))))

(define (vsqr v)
  (define v->nwu (->measure v))
  (define v-number (measure-number v->nwu))
  (cond [(for/and ([item v-number])
           (number? item))
         (define v-unit (measure-unit v->nwu))
         (measure (for/sum ([v-i v-number])
                    (sqr v-i))
                  (u* v-unit v-unit))]
        [else
         (error "vector-with-unit: not all components have the same unit-dimension, given:" v)]))
(define (vector-magnitude v)
  (define v->nwu (->measure v))
  (define v-number (measure-number v->nwu))
  (cond [(for/and ([item v-number])
           (number? item))
         (measure (sqrt (for/sum ([v-i v-number])
                          (sqr v-i)))
                  (measure-unit v->nwu))]
        [else
         (error "vector-with-unit: not all components have the same unit-dimension, given:" v)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; scalar->SI-prefix : [Number -> Symbol]
(module+ test
  (check-equal? (scalar->SI-prefix 1)
                '||)
  (check-equal? (scalar->SI-prefix 1000)
                'kilo)
  (check-equal? (scalar->SI-prefix 1/100)
                'centi)
  (check-equal? (scalar->SI-prefix 1/1000)
                'milli)
  )

(define (scalar->SI-prefix n)
  (define (n=? x)
    (= n x))
  (cond [(n=? 1) '||]
        [(n=? 10) 'deka]
        [(n=? 100) 'hecto]
        [(n=? 1000) 'kilo]
        [(n=? (10^ 6)) 'mega]
        [(n=? (10^ 9)) 'giga]
        [(n=? (10^ 12)) 'tera]
        [(n=? (10^ 15)) 'peta]
        [(n=? (10^ 18)) 'exa]
        [(n=? (10^ 21)) 'zetta]
        [(n=? (10^ 24)) 'yotta]
        [(n=? 1) '||]
        [(n=? 1/10) 'deci]
        [(n=? 1/100) 'centi]
        [(n=? 1/1000) 'milli]
        [(n=? (10^ -6)) 'micro]
        [(n=? (10^ -9)) 'nano]
        [(n=? (10^ -12)) 'pico]
        [(n=? (10^ -15)) 'femto]
        [(n=? (10^ -18)) 'atto]
        [(n=? (10^ -21)) 'zepto]
        [(n=? (10^ -24)) 'yocto]
        [else (error 'scalar->SI-prefix "prefix not found for ~v" n)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; nwu=
;; like =, but for numbers with units, without requiring that they be numbers with units
(module+ test
  (check-equal? (nwu= (m 1    meter)
                      (m 100  centimeter)
                      (m 1000 millimeter))
                #t)
  )

(define nwu=
  (lambda args
    (cond [(empty? args)
           (error "nwu=: expects 2 or more arguments, given 0")]
          [else
           (and (apply dimension=? (map measure-dimension args))
                (local [(define u (measure-unit (first args)))
                        (define converted-args
                          (for/list ([arg args])
                            (convert arg u)))]
                  (and (or (>= 1 (length converted-args))
                           (apply = (map measure-number converted-args)))
                       (apply unit=? (map measure-unit converted-args)))))])))

;; nwu*
;; like *, but for numbers with units, without requiring that they be numbers with units
(module+ test
  (check-equal? (m=? (nwu* 2 newton 3 meter)
                     (m 6 (unit-rename joule #f)))
                #t)
  )

(define nwu*
  (lambda args
    (let ([args (map ->measure args)])
      (make-measure (apply n* (map measure-number args))
                    (apply u* (map measure-unit args))))))

;; nwu/
;; like /, but for numbers with units, without requiring that they be numbers with units
(module+ test
  (check-equal? (m=? (nwu/ (m 6 joule)
                           (m 2 meter))
                     (m 3 (unit-rename newton #f)))
                #t)
  )

(define nwu/
  (lambda args
    (let ([args (map ->measure args)])
      (make-measure (apply n/ (map measure-number args))
                    (apply u/ (map measure-unit args))))))

;; nwu+
;; like +, but for numbers with units, without requiring that they be numbers with units, but requiring that they have the same dimension
(module+ test
  (check-equal? (nwu+ (m  2 meter)
                      (m 50 centimeter))
                (m #e2.5 meter))
  )

(define nwu+
  (lambda args
    (let ([args (map ->measure args)])
      (cond [(empty? args) 0]
            [(not (apply dimension=? (map measure-dimension args)))
             (apply error (cons "nwu+: cannot add numbers with different dimensions, given:" args))]
            [(apply unit=? (map measure-unit args))
             (make-measure (apply + (map measure-number args))
                           (measure-unit (first args)))]
            [else
             (apply nwu+
                    (map (lambda (arg)
                           (convert arg (measure-unit (first args))))
                         args))]))))

;; nwu-
;; like -, but for numbers with units, without requiring that they be numbers with units, but requiring that they have the same dimension
(module+ test
  (check-equal? (nwu- (m  2 meter)
                      (m 50 centimeter))
                (m #e1.5 meter))
  )

(define nwu-
  (lambda args
    (let ([args (map ->measure args)])
      (cond [(empty? args) 0]
            [(not (apply dimension=? (map measure-dimension args)))
             (apply error (cons "nwu-: cannot subtract numbers with different dimensions, given:" args))]
            [(apply unit=? (map measure-unit args))
             (make-measure (apply - (map measure-number args))
                           (measure-unit (first args)))]
            [else
             (apply nwu-
                    (map (lambda (arg)
                           (convert arg (measure-unit (first args))))
                         args))]))))

;; nwuexpt
;; like expt, but for numbers with units, without requiring that they be numbers with units

(define (nwuexpt m e)
  (let ([m (->measure m)])
    (make-measure (expt (measure-number m) e)
                  (uexpt (measure-unit m) e))))

(define (nwusqr m)
  (let ([m (->measure m)])
    (make-measure (sqr (measure-number m))
                  (usqr (measure-unit m)))))

(define (nwusqrt m)
  (let ([m (->measure m)])
    (make-measure (sqrt (measure-number m))
                  (usqrt (measure-unit m)))))




;; Some Physics Formulas:

(define (kinetic-energy #:m [m #f] #:v [v #f] 
                        #:I [I #f] #:w [w #f])
  (cond [(and m v I w)
         (nwu+ (kinetic-energy #:m m #:v v)
               (kinetic-energy #:I I #:w w))]
        [(and m v)
         (cond [(number? (measure-number v))
                (nwu* 1/2 m (nwusqr v))]
               [(vector? (measure-number v))
                (nwu* 1/2 m 
                      (make-measure (vsqr   (measure-number v))
                                    (nwusqr (measure-unit v))))]
               [else
                (error "kinetic-energy: not a proper velocity, given:" v)])]
        [(and I w)
         (cond [(number? (measure-number w))
                (nwu* 1/2 I (sqr w))]
               [(vector? (measure-number v))
                (nwu* 1/2 I 
                      (make-measure (vsqr (measure-number v))
                                    (usqr (measure-unit w))))]
               [else
                (error "kinetic-energy: not a proper angular velocity, given:" w)])]
        [else
         (define-syntax-rule 
           (cond-string-append clause ...)
           (string-append (cond clause 
                                [else ""])
                          ...))
         (error (cond-string-append [#t "kinetic-energy: not enough information, given: "]
                                    [m (string-append "#:m "(~v m)" ")]
                                    [v (string-append "#:v "(~v v)" ")]
                                    [I (string-append "#:I "(~v I)" ")]
                                    [w (string-append "#:w "(~v w)" ")]
                                    [(and (not m) (not v)
                                          (not I) (not w))
                                     (string-append "nothing")]
                                    ))]))






