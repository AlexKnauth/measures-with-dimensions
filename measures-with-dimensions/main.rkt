#lang racket

(require (prefix-in racket: racket))
(require (only-in lang/htdp-intermediate-lambda =~))
(require mutable-match-lambda/syntax-to-string)
(require "typed/exact-tau-pi-eta.rkt")














(define n*
  (local [(define (scalar? x)
            (number? x))
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
  (local [(define (scalar? x)
            (number? x))
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
         (define v-unit (nwu-unit v->nwu))
         (measure (for/sum ([v-i v-number])
                    (sqr v-i))
                  (u* v-unit v-unit))]
        [else
         (error "vector-with-unit: not all components have the same unit-dimension, given:" v)]))
(define (vector-magnitude v)
  (define v->nwu (any->number-with-unit v))
  (define v-number (nwu-number v->nwu))
  (cond [(for/and ([item v-number])
           (number-without-unit? item))
         (number-with-unit (sqrt (for/sum ([v-i v-number])
                                   (sqr v-i)))
                           (nwu-unit v->nwu))]
        [else
         (error "vector-with-unit: not all components have the same unit-dimension, given:" v)]))




(define-syntax define-unit
  (syntax-rules ()
    [(define-unit (u) expr)
     (define (u)
       (unit-rename (any->unit expr)
                    'u))]
    [(define-unit (u) expr1 ... expr)
     (define (u)
       expr1 ...
       (unit-rename (any->unit expr)
                    'u))]
    [(define-unit (u a ...) expr1 ... expr)
     (define (u a ...)
       expr1 ...
       (unit-rename (any->unit expr)
                    (string->symbol
                     (apply string-append 
                            (cons (symbol->string 'u)
                                  (list (string-append " "(~v a))
                                        ...))))))]
    [(define-unit u expr)
     (define u
       (procedure-rename
        (procedure-reduce-arity 
         (lambda args
           (unit-rename (any->unit (apply expr args))
                        (string->symbol
                         (apply string-append 
                                (cons (symbol->string 'u)
                                      (for/list ([a args])
                                        (string-append " "(~v a))))))))
         (procedure-arity expr))
        'u))]))








;; scalar->SI-prefix : (Number -> (or/c Symbol False))
(check-expect (scalar->SI-prefix 1)
              '||)
(check-expect (scalar->SI-prefix 1000)
              'kilo)
(check-expect (scalar->SI-prefix 1/100)
              'centi)
(check-expect (scalar->SI-prefix 1/1000)
              'milli)

(define (scalar->SI-prefix n)
  (local [(define (n=? x)
            (= n x))]
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
          [(not (= n (10^ (exact-round (logbase 10 n)))))
           (scalar->SI-prefix (10^ (exact-round (logbase 10 n))))]
          [else false])))








;; nwu=
;; like =, but for numbers with units, without requiring that they be numbers with units
(check-expect (nwu= (number-with-unit 1    (meter))
                    (number-with-unit 100  (centimeter))
                    (number-with-unit 1000 (millimeter)))
              true)

(define nwu=
  (lambda args
    (cond [(empty? args)
           (error "nwu=: expects 2 or more arguments, given 0")]
          [else
           (and (apply dimension=? (map nwu-dimension args))
                (local [(define u (nwu-unit (first args)))
                        (define converted-args
                          (for/list ([arg args])
                            (convert arg u)))]
                  (and (or (>= 1 (length converted-args))
                           (apply = (map nwu-number converted-args)))
                       (apply unit=? (map nwu-unit converted-args)))))])))

;; nwu*
;; like *, but for numbers with units, without requiring that they be numbers with units
(check-expect (my-equal? (nwu* 2 (newton) 3 (meter))
                         (number-with-unit 6 (unit-rename (joule) #f)))
              true)

(define nwu*
  (lambda args
    (number-with-unit (apply n* (map nwu-number args))
                      (apply u* (map nwu-unit args)))))

;; nwu/
;; like /, but for numbers with units, without requiring that they be numbers with units
(check-expect (my-equal? (nwu/ (number-with-unit 6 (joule))
                               (number-with-unit 2 (meter)))
                         (number-with-unit 3 (unit-rename (newton) #f)))
              true)

(define nwu/
  (lambda args
    (number-with-unit (apply n/ (map nwu-number args))
                      (apply u/ (map nwu-unit args)))))

;; nwu+
;; like +, but for numbers with units, without requiring that they be numbers with units, but requiring that they have the same dimension
(check-expect (nwu+ (number-with-unit 2 (meter))
                    (number-with-unit 50 (centimeter)))
              (number-with-unit 2.5 (meter)))

(define nwu+
  (lambda args
    (cond [(empty? args)
           0]
          [(not (apply dimension=? (map nwu-dimension args)))
           (apply error (cons "nwu+: cannot add numbers with different dimensions, given:" args))]
          [(apply unit=? (map nwu-unit args))
           (number-with-unit (apply + (map nwu-number args))
                             (nwu-unit (first args)))]
          [else
           (apply nwu+
                  (map (lambda (arg)
                         (convert arg (nwu-unit (first args))))
                       args))])))

;; nwu-
;; like -, but for numbers with units, without requiring that they be numbers with units, but requiring that they have the same dimension
(check-expect (nwu- (number-with-unit 2 (meter))
                    (number-with-unit 50 (centimeter)))
              (number-with-unit 1.5 (meter)))

(define nwu-
  (lambda args
    (cond [(empty? args)
           0]
          [(not (apply dimension=? (map nwu-dimension args)))
           (apply error (cons "nwu-: cannot subtract numbers with different dimensions, given:" args))]
          [(apply unit=? (map nwu-unit args))
           (number-with-unit (apply - (map nwu-number args))
                             (nwu-unit (first args)))]
          [else
           (apply nwu-
                  (map (lambda (arg)
                         (convert arg (nwu-unit (first args))))
                       args))])))

;; nwuexpt
;; like expt, but for numbers with units, without requiring that they be numbers with units

(define (nwuexpt nwu e)
  (number-with-unit (expt (nwu-number nwu) e)
                    (uexpt (nwu-unit nwu) e)))

(define (nwusqr nwu)
  (number-with-unit (sqr (nwu-number nwu))
                    (u* (nwu-unit nwu)
                        (nwu-unit nwu))))

(define (nwusqrt nwu)
  (number-with-unit (sqrt (nwu-number nwu))
                    (uexpt (nwu-unit nwu) 1/2)))





;; print-number-with-unit : (Any [OutputPort (current-output-port)] [(or/c 0 1) 0] -> Void)
;; print a number with a unit
;; !!!
(define (print-number-with-unit expr [out (current-output-port)] [quote-depth 0])
  (cond [(angle? expr)
         (print-angle expr out quote-depth)]
        [(number? expr)
         (print expr out quote-depth)]
        [(and (number-with-unit? expr)
              (= 1 (nwu-unit-scalar expr))
              (dimensionless? (nwu-dimension expr)))
         (print (nwu-number expr)
                out quote-depth)]
        [(number-with-unit? expr)
         (display (string-append
                   (if (number? quote-depth)
                       (apply string-append (build-list quote-depth (const ",")))
                       "")
                   (string-append "(* "(~v(nwu-number expr))" "(~v(nwu-unit expr))")"))
                  out)]
        [else
         (print expr out quote-depth)]))



;; print-angle : (Any [OutputPort (current-output-port)] [(or/c 0 1) 0] -> Void)
;; print an angle
(define (print-angle expr [out (current-output-port)] [quote-depth 0])
  (cond [(and (number-with-unit? expr)
              (dimensionless? (nwu-dimension expr)))
         (cond [(unit=? (nwu-unit expr) (radian))
                (define as (current-angle-measurement-system))
                (cond [(angle-system=radians? as)
                       (print-angle (nwu-number expr) out quote-depth)]
                      [(angle-system=radians-with-pi? as)
                       (print-angle (nwu-number expr) out quote-depth)]
                      [(angle-system=radians-with-2pi? as)
                       (print-angle (nwu-number expr) out quote-depth)]
                      [(angle-system=radians-with-tau? as)
                       (print-angle (nwu-number expr) out quote-depth)]
                      [(angle-system=radians-with-eta? as)
                       (print-angle (nwu-number expr) out quote-depth)]
                      [else
                       (print (nwu-number expr) out quote-depth)])]
               [(unit=? (nwu-unit expr) (turn))
                (display (string-append (~v(nwu-number expr))"*turns")
                         out)]
               [(unit=? (nwu-unit expr) (degree))
                (display (string-append (~v(nwu-number expr))"°")
                         out)]
               [else
                (print expr out quote-depth)])]
        [(number? expr)
         (define as (current-angle-measurement-system))
         (cond [(angle-system=radians? as)
                (print expr out quote-depth)]
               [(angle-system=radians-with-pi? as)
                (display (string-append (~v(n/ expr really-close-to-pi))"*pi")
                         out)]
               [(angle-system=radians-with-2pi? as)
                (display (string-append (~v(n/ expr really-close-to-2pi))"*2pi")
                         out)]
               [(angle-system=radians-with-tau? as)
                (display (string-append (~v(n/ expr really-close-to-tau))"*tau")
                         out)]
               [(angle-system=radians-with-eta? as)
                (display (string-append (~v(n/ expr really-close-to-eta))"*eta")
                         out)]
               [(angle-system=turns? as)
                (display (string-append (~v(n/ expr really-close-to-tau))"*turns")
                         out)]
               [(angle-system=degrees? as)
                (display (string-append (~v(n* expr (/ 360 really-close-to-tau)))"°")
                         out)]
               [else 
                (print expr out quote-depth)])]
        [else
         (print expr out quote-depth)]))













;; SI units:

(define-unit (gram-per-cubic-centimeter)
  (u/ (gram)
      (cubic-centimeter)))
(define-unit (kilogram-per-cubic-meter)
  (u/ (kilogram)
      (cubic-meter)))
(define SI-BASE-DENSITY-UNITS
  (list (gram-per-cubic-centimeter)
        (kilogram-per-cubic-meter)))

(define-unit (meter-per-second)
  (u/ (meter)
      (second)))
(define-unit (kilometer-per-hour)
  (u/ (kilometer)
      (hour)))
(define SI-BASE-VELOCITY-UNITS
  (list (meter-per-second)
        (kilometer-per-hour)))

(define-unit (meter-per-second-squared)
  (u/ (meter)
      (second)
      (second)))
(define SI-BASE-ACCELERATION-UNITS
  (list (meter-per-second-squared)))


(define-unit (kilogram-meter-per-second)
  (u* (kilogram) (meter-per-second)))
(define-unit (newton-second)
  (u* (newton) (second)))
(define SI-BASE-MOMENTUM-UNITS
  (list (kilogram-meter-per-second)
        (newton-second)))





;; Some Physics Formulas:

(define (kinetic-energy #:m [m #f] #:v [v #f] 
                        #:I [I #f] #:w [w #f])
  (cond [(and m v I w)
         (nwu+ (kinetic-energy #:m m #:v v)
               (kinetic-energy #:I I #:w w))]
        [(and m v)
         (cond [(number-without-unit? (nwu-number v))
                (nwu* 1/2 m (nwusqr v))]
               [(vector-without-units? (nwu-number v))
                (nwu* 1/2 m 
                      (number-with-unit (vsqr   (nwu-number v))
                                        (nwusqr (nwu-unit v))))]
               [else
                (error "kinetic-energy: not a proper velocity, given:" v)])]
        [(and I w)
         (cond [(number-without-unit? (nwu-number w))
                (nwu* 1/2 I (sqr w))]
               [(vector-without-units? (nwu-number v))
                (nwu* 1/2 I 
                      (number-with-unit (vsqr   (nwu-number v))
                                        (nwusqr (nwu-unit w))))]
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






(test)