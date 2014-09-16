#lang typed/racket

(provide (all-defined-out)
         (all-from-out "dimensions.rkt")
         )

(require "dimensions.rkt"
         "exact-tau-pi-eta.rkt")

(require syntax/parse/define)

(require/typed typed/racket
               [~a (Any -> String)])

(: 10^ (case-> [Zero -> One]
               [Real -> Real]
               [Natural -> Natural]
               [Integer -> Exact-Rational]
               [Number -> Number]))
(define (10^ n)
  (expt 10 n))

(define positive-real? (make-predicate Positive-Real))



(define-type Unitish
  (U Unit
     Positive-Real
     Dimensionish
     ))
(define-type (Unitof d)
  (p-unit d))
(define-type Unit
  (Unitof Dimension))

(: ->unit : (All (a b c d e)
                 (case-> [(Unitof (pdimension a b c d e)) -> (Unitof (pdimension a b c d e))]
                         [(pdimension a b c d e) -> (Unitof (pdimension a b c d e))]
                         [Positive-Real -> (Unitof Dimensionless-Dimension)]
                         [Dimensionish -> (Unitof Dimension)]
                         [Unitish -> Unit])))
(define (->unit u)
  (cond [(p-unit? u) (p-unit (p-unit-name u)
                             (p-unit-scalar u)
                             (->dimension (p-unit-dimension u)))]
        [(pdimension? u) (p-unit u 1 u)]
        [(positive-real? u) (p-unit u u dimensionless-dimension)]
        [(dimensionish? u) (unit u 1 (->dimension u))]))




(struct: (d) p-unit ([name : (U Any #f)] [scalar : Positive-Real] [dimension : d])
  #:transparent
  #:guard (lambda (name scalar dimension _)
            (unless (positive-real? scalar)
              (error 'p-unit "expected positive-real? for scalar argument, given ~v" scalar))
            (unless (dimension? dimension)
              (error 'p-unit "expected dimension? for dimension argument, given ~v" dimension))
            (values name scalar dimension))
  #:property prop:custom-write (lambda (u out mode)
                                 (let ([name (p-unit-name u)])
                                   (cond [name (display name out)]
                                         [(zero? mode) (display "(unit #f " out)
                                                       (print (p-unit-scalar u) out) (display " " out)
                                                       (print (p-unit-dimension u) out)
                                                       (display ")" out)]
                                         [else (display (p-unit-scalar u) out)
                                               (display "*" out)
                                               (display (p-unit-dimension u) out)]))))



(: unit : [(U Any #f) Positive-Real Dimension -> (p-unit Dimension)])
(: unit? : [Any -> Boolean : Unit])
(: unitish? : [Any -> Boolean : Unitish])
(: unit-name : [Unitish -> (U Any #f)])
(: unit-scalar : [Unit -> Positive-Real])
(: unit-dimension : (case-> [Positive-Real -> Dimensionless-Dimension]
                            [Unit -> Dimension]))
(define (unit name scalar dimension)
  ((inst p-unit Dimension) name scalar (->dimension dimension)))
(define unit? (make-predicate Unit))
(define unitish? (make-predicate Unitish))
(define (unit-name u)
  (p-unit-name (->unit u)))
(define (unit-scalar u)
  (p-unit-scalar (->unit u)))
(define (unit-dimension u)
  (p-unit-dimension (->unit u)))

(: unit=? (Unitish Unitish * -> Boolean))
(define (unit=? u . rst)
  (let ([u : Unit (->unit u)])
    (let ([u.scalar : Positive-Real (p-unit-scalar u)]
          [u.dimension : Dimension (p-unit-dimension u)])
      (for/and: : Boolean ([u2 : Unitish (in-list rst)])
        (let ([u2 : Unit (->unit u2)])
          (and (= u.scalar (p-unit-scalar u2))
               (dimension=? u.dimension (p-unit-dimension u2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type Dimensionless-Unit
  (Unitof Dimensionless-Dimension))

(define-type Length-Unit
  (Unitof Length-Dimension))
(define-type Mass-Unit
  (Unitof Mass-Dimension))
(define-type Time-Unit
  (Unitof Time-Dimension))
(define-type Charge-Unit
  (Unitof Charge-Dimension))
(define-type Absolute-Temperature-Unit
  (Unitof Temperature-Dimension))

(define-type Area-Unit
  (Unitof Area-Dimension))
(define-type Volume-Unit
  (Unitof Volume-Dimension))

(define-type Mass-Density-Unit
  (Unitof Mass-Density-Dimension))
(define-type Charge-Density-Unit
  (Unitof Charge-Density-Dimension))

(define-type Velocity-Unit
  (Unitof Velocity-Dimension))
(define-type Speed-Unit Velocity-Unit)
(define-type Acceleration-Unit
  (Unitof Acceleration-Dimension))
(define-type Force-Unit
  (Unitof Force-Dimension))
(define-type Momentum-Unit
  (Unitof Momentum-Dimension))

(define-type Energy-Unit
  (Unitof Energy-Dimension))
(define-type Work-Unit
  Energy-Unit)
(define-type Power-Unit
  (Unitof Power-Dimension))

(define-type Pressure-Unit
  (Unitof Pressure-Dimension))

(define-type Entropy-Unit
  (Unitof Entropy-Dimension))

(define-type Electric-Field-Unit
  (Unitof Electric-Field-Dimension))
(define-type Electric-Potential-Unit
  (Unitof Electric-Potential-Dimension))
(define-type Voltage-Unit
  Electric-Potential-Unit)
(define-type Emf-Unit
  Voltage-Unit)
(define-type Capacitance-Unit
  (Unitof Capacitance-Dimension))

(define-type Current-Unit
  (Unitof Current-Dimension))
(define-type Current-Density-Unit
  (Unitof Current-Density-Dimension))
(define-type Resistance-Unit
  (Unitof Resistance-Dimension))
(define-type Resistivity-Unit
  (Unitof Resistivity-Dimension))
(define-type Conductivity-Unit
  (Unitof Conductivity-Dimension))

(define-type Magnetic-Field-Unit
  (Unitof Magnetic-Field-Dimension))

(define-type Electric-Flux-Unit
  (Unitof Electric-Flux-Dimension))
(define-type Magnetic-Flux-Unit
  (Unitof Magnetic-Flux-Dimension))

(define-type Inductance-Unit
  (Unitof Inductance-Dimension))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: unit-rename : (Unitish Any -> Unit))
(define (unit-rename u name)
  (let ([u : Unit (->unit u)])
    (unit name
          (p-unit-scalar u)
          (p-unit-dimension u))))

(define-simple-macro (define-unit u:id (~literal :) t:expr val:expr)
  (define: u : t (cast (unit-rename val 'u) t)))

(: usqr : [Unitish -> Unit])
(define (usqr u)
  (let ([u : Unit (->unit u)])
    (cond [(unit=? u 1-unit) 1-unit]
          [else (unit `(usqr ,u)
                      (cast (sqr (unit-scalar u)) Positive-Real)
                      (dsqr (unit-dimension u)))])))

(: usqrt : [Unitish -> Unit])
(define (usqrt u)
  (let ([u : Unit (->unit u)])
    (cond [(unit=? u 1-unit) 1-unit]
          [else (unit `(usqrt ,u)
                      (cast (sqrt (unit-scalar u)) Positive-Real)
                      (dsqrt (unit-dimension u)))])))

(: uexpt : [Unitish Exact-Rational -> Unit])
(define (uexpt u n)
  (let ([u : Unit (->unit u)])
    (cond [(unit=? u 1-unit) 1-unit]
          [(zero? n) 1-unit]
          [(one? n) u]
          [else (unit `(uexpt ,u ,n)
                      (cast (expt (unit-scalar u) n) Positive-Real)
                      (dexpt (unit-dimension u) n))])))

(: u* : [Unitish * -> Unit])
(define (u* . args)
  (let ([args : (Listof Unit) (map ->unit args)])
    (cond [(empty? args) 1-unit]
          [(one? (length args)) (first args)]
          [else (unit `(u* ,@args)
                      (cast (apply * (map unit-scalar args)) Positive-Real)
                      (apply d* (map unit-dimension args)))])))

(: u/ : [Unitish Unitish * -> Unit])
(define u/
  (case-lambda
    [([u : Unitish])
     (let ([u : Unit (->unit u)])
       (unit `(u1/ ,u)
             (cast (/ (unit-scalar u)) Positive-Real)
             (d/ (unit-dimension u))))]
    [([u : Unitish] . [rst : Unitish *])
     (let ([u : Unit (->unit u)]
           [rst : (Listof Unit) (map ->unit rst)])
       (unit `(u/ ,u ,@rst)
             (cast (apply / (unit-scalar u) (map unit-scalar rst)) Positive-Real)
             (apply d/ (unit-dimension u) (map unit-dimension rst))))]))


(define-simple-macro (defprefix prefix:id factor-expr:expr)
  (begin
    (: factor : Positive-Real)
    (define factor (cast factor-expr Positive-Real))
    (: prefix : [Unitish -> Unit])
    (define (prefix u)
      (let ([u : Unit (->unit u)])
        (unit `(prefix ,u)
              (cast (* factor (unit-scalar u)) Positive-Real)
              (unit-dimension u))))))

(defprefix deci 1/10)
(defprefix centi 1/100)
(defprefix milli 1/1000)
(defprefix micro (10^ -6))
(defprefix nano (10^ -9))
(defprefix pico (10^ -12))
(defprefix femto (10^ -15))
(defprefix atto (10^ -18))
(defprefix zepto (10^ -21))
(defprefix yocto (10^ -24))
(defprefix deca 10)
(defprefix hecto 100)
(defprefix kilo 1000)
(defprefix mega (10^ 6))
(defprefix giga (10^ 9))
(defprefix tera (10^ 12))
(defprefix peta (10^ 15))
(defprefix exa (10^ 18))
(defprefix zetta (10^ 21))
(defprefix yotta (10^ 24))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Units:
(define-simple-macro
  (define-units/type Unit-Type:expr
    [id:id expr:expr] ...)
  (begin (define-unit id : Unit-Type expr) ...))

;; Dimensionless-Units:
(define-units/type Dimensionless-Unit
  [1-unit (unit '1-unit 1 dimensionless-dimension)]
  [radian 1-unit]
  [turn (u* really-close-to-tau radian)]
  [revolution turn]
  [cycle turn]
  [degree (u/ turn 360)]
  )
(define ° : Dimensionless-Unit degree)

;; Mass:
(define-units/type Mass-Unit
  [kilogram (unit 'kilogram 1 mass-dimension)]
  [gram (unit 'gram 1/1000 mass-dimension)]
  [pound-mass (u* #e0.45359237 kilogram)]
  [ounce-mass (u/ pound-mass 16)]
  [ton (u* 2000 pound-mass)]
  )

;; Length:
(define-units/type Length-Unit
  [meter (unit 'meter 1 length-dimension)]
  [centimeter (centi meter)]
  [millimeter (milli meter)]
  [kilometer (kilo meter)]
  [decimeter (deci meter)]
  [foot (u* #e30.48 centimeter)]
  [inch (u/ foot 12)]
  [yard (u* foot 3)]
  [mile (u* 5280 foot)]
  )

;; Time:
(define-units/type Time-Unit
  [second (unit 'second 1 time-dimension)]
  [minute (u* 60 second)]
  [hour (u* 60 minute)]
  [day (u* 24 hour)]
  [week (u* 7 day)]
  [common-year (u* 365 day)]
  [leap-year (u* 366 day)]
  [average-year (u* #e365.25 day)]
  [average-month (u/ average-year 12)]
  [average-decade (u* 10 average-year)]
  [average-century (u* 100 average-year)]
  [century average-century]
  [millisecond (milli second)]
  [microsecond (micro second)]
  [nanosecond (nano second)]
  )

;; Charge:
(define-units/type Charge-Unit
  [columb (unit 'columb 1 charge-dimension)]
  )

;; Absolute Temperature:
(define-units/type Absolute-Temperature-Unit
  [kelvin (unit 'kelvin 1 temperature-dimension)]
  [rankine (u* 5/9 kelvin)]
  )

;; Area:
(define-units/type Area-Unit
  [square-meter (usqr meter)]
  [square-centimeter (usqr centimeter)]
  [square-foot (usqr foot)]
  [square-inch (usqr inch)]
  [square-mile (usqr mile)]
  [acre (u* 43560 square-foot)]
  )

;; Volume:
(define-units/type Volume-Unit
  [cubic-meter (uexpt meter 3)]
  [cubic-centimeter (uexpt centimeter 3)]
  [liter (uexpt decimeter 3)]
  [milliliter cubic-centimeter]
  [cubic-foot (uexpt foot 3)]
  [cubic-inch (uexpt inch 3)]
  [gallon (u* 231 cubic-inch)]
  [quart (u/ gallon 4)]
  [pint (u/ quart 2)]
  [cup (u/ pint 2)]
  [fluid-ounce (u/ cup 8)]
  )

;; Velocity/Speed:
(define-units/type Velocity-Unit
  [m/s (unit 'm/s 1 velocity-dimension)]
  [mph (u/ mile hour)]
  [fps (u/ foot second)]
  )

;; Acceleration
(define-units/type Acceleration-Unit
  [m/s^2 (unit 'm/s^2 1 acceleration-dimension)]
  )

;; Force:
(define-units/type Force-Unit
  [newton (unit 'newton 1 force-dimension)]
  [pound-force (u* (u* #e9.80665 (u/ meter (usqr second))) pound-mass)]
  )

;; Energy/Work/Torque:
(define-units/type Energy-Unit
  [joule (unit 'joule 1 energy-dimension)]
  [calorie (u* #e4.184 joule)]
  [newton-meter (u* newton meter)]
  [foot-pound (u* foot pound-force)]
  [british-thermal-unit (u* #e1055.05585262 joule)]
  ;kilowatt-hour defined later because it uses kilowatt
  )

;; Power:
(define-units/type Power-Unit
  [watt (unit 'watt 1 power-dimension)]
  [kilowatt (kilo watt)]
  [horsepower (u* #e746 watt)]
  )

;; Energy Again
(define-unit kilowatt-hour : Energy-Unit (u* kilowatt hour))

;; Pressure:
(define-units/type Pressure-Unit
  [pascal (unit 'pascal 1 pressure-dimension)]
  [psi (u/ pound-force square-inch)]
  [atmosphere (u* #e101325 pascal)]
  [bar (u* #e100000 pascal)]
  [millibar (milli bar)]
  )

;; Voltage/Emf/Electric-Potential
(define-units/type Voltage-Unit
  [volt (unit 'volt 1 voltage-dimension)]
  )

;; Capacitance
(define-units/type Capacitance-Unit
  [farrad (unit 'farrad 1 capacitance-dimension)]
  )

;; Current
(define-units/type Current-Unit
  [ampere (unit 'ampere 1 current-dimension)]
  )

;; Resistance
(define-units/type Resistance-Unit
  [ohm (unit 'ohm 1 resistance-dimension)]
  )

;; Magnetic-Field
(define-units/type Magnetic-Field-Unit
  [tesla (unit 'tesla 1 magnetic-field-dimension)]
  )

;; Inductance
(define-units/type Inductance-Unit
  [henry (unit 'henry 1 inductance-dimension)]
  )


