#lang typed/racket/base

(provide (all-defined-out))

(require syntax/parse/define
         "dimension-struct.rkt"
         "dimension-operations.rkt"
         "dimensions.rkt"
         "unit-struct.rkt"
         "unit-operations.rkt"
         "preds.rkt"
         "exact-tau-pi-eta.rkt"
         "untyped-utils.rkt"
         )

(: 10^ : (case-> [Zero -> One]
                 [Natural -> Natural]
                 [Integer -> Exact-Rational]
                 [Real -> Real]
                 [Number -> Number]))
(define (10^ n)
  (expt 10 n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
(define-type Torque-Unit
  Work-Unit)
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




(define-simple-macro (defprefix prefix:id factor-expr:expr)
  (begin
    (: factor : Positive-Real)
    (define factor (cast factor-expr Positive-Real))
    (: prefix : [Unitish -> Unit])
    (define (prefix u)
      (let ([u : Unit (->unit u)])
        (make-Unit `(prefix ,u)
                   (cast (* factor (Unit-scalar u)) Positive-Real)
                   (Unit-dimension u))))))

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
  [thing (make-Unit 'thing 1 dimensionless-dimension)]
  [mol (make-Unit 'mol (cast (* #i6.02214129 (10^ 23)) Positive-Real) dimensionless-dimension)]
  [radian 1-unit]
  [turn (u* really-close-to-tau radian)]
  [revolution turn]
  [cycle turn]
  [degree (u/ turn 360)]
  [percent (u/ 1-unit 100)]
  )
(define ° : Dimensionless-Unit degree)

;; Mass:
(define-units/type Mass-Unit
  [kilogram (make-Unit 'kilogram 1 mass-dimension)]
  [gram (make-Unit 'gram 1/1000 mass-dimension)]
  [pound-mass (u* #e0.45359237 kilogram)]
  [ounce-mass (u/ pound-mass 16)]
  [ton (u* 2000 pound-mass)]
  [atomic-mass-unit (u* #i1.6605402e-27 kilogram)]
  [planck-mass (u* #i2.17651e-8 kilogram)]
  )

;; Length:
(define-units/type Length-Unit
  [meter (make-Unit 'meter 1 length-dimension)]
  [centimeter (centi meter)]
  [millimeter (milli meter)]
  [kilometer (kilo meter)]
  [decimeter (deci meter)]
  [foot (u* #e30.48 centimeter)]
  [inch (u/ foot 12)]
  [yard (u* foot 3)]
  [mile (u* 5280 foot)]
  [astronomical-unit (u* 149597870700 meter)]
  [angstrom (u* 1e-10 meter)]
  [light-year (u* 9.4607304725808e15 meter)]
  [nautical-mile (u* 1852 meter)]
  [planck-length (u* #i1.616199e-35 meter)]
  )

;; Time:
(define-units/type Time-Unit
  [second (make-Unit 'second 1 time-dimension)]
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
  [planck-time (u* #i5.39106e-44 second)]
  )

;; Charge:
(define-units/type Charge-Unit
  [coulomb (make-Unit 'coulomb 1 charge-dimension)]
  [planck-charge (u* #i1.875545956e-18 coulomb)]
  )

;; Absolute Temperature:
(define-units/type Absolute-Temperature-Unit
  [kelvin (make-Unit 'kelvin 1 temperature-dimension)]
  [rankine (u* 5/9 kelvin)]
  [plank-temperature (u* #i1.416833e32 kelvin)]
  )

;; Area:
(define-units/type Area-Unit
  [square-meter (usqr meter)]
  [square-centimeter (usqr centimeter)]
  [square-foot (usqr foot)]
  [square-inch (usqr inch)]
  [square-mile (usqr mile)]
  [acre (u* 43560 square-foot)]
  [planck-area (usqr planck-length)]
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
  [tablespoon (u* 2 fluid-ounce)]
  [teaspoon (u/ tablespoon 3)]
  [planck-volume (uexpt planck-length 3)]
  )

;; Velocity/Speed:
(define-units/type Velocity-Unit
  [m/s (make-Unit 'm/s 1 velocity-dimension)]
  [mph (u/ mile hour)]
  [fps (u/ foot second)]
  [c-unit (u* 299792458 m/s)]
  [knot (u/ nautical-mile hour)]
  [planck-velocity c-unit]
  )

;; Acceleration
(define-units/type Acceleration-Unit
  [m/s^2 (make-Unit 'm/s^2 1 acceleration-dimension)]
  )

;; Force:
(define-units/type Force-Unit
  [newton (make-Unit 'newton 1 force-dimension)]
  [pound-force (u* (u* #e9.80665 (u/ meter (usqr second))) pound-mass)]
  )

;; Momentum:
(define-units/type Momentum-Unit
  )

;; Power:
(define-units/type Power-Unit
  [watt (make-Unit 'watt 1 power-dimension)]
  [kilowatt (kilo watt)]
  [horsepower (u* #e746 watt)]
  )

;; Energy/Work/Torque:
(define-units/type Energy-Unit
  [joule (make-Unit 'joule 1 energy-dimension)]
  [newton-meter (u* newton meter)]
  [calorie (u* #e4.184 joule)]
  [foot-pound (u* foot pound-force)]
  [british-thermal-unit (u* #e1055.05585262 joule)]
  [kilowatt-hour (u* kilowatt hour)]
  [electron-volt (u* #i1.60217733e-19 joule)]
  )

;; Pressure:
(define-units/type Pressure-Unit
  [pascal (make-Unit 'pascal 1 pressure-dimension)]
  [psi (u/ pound-force square-inch)]
  [atmosphere (u* #e101325 pascal)]
  [bar (u* #e100000 pascal)]
  [millibar (milli bar)]
  )

;; Entropy:
(define-units/type Entropy-Unit
  )

;; Electric-Field:
(define-units/type Electric-Field-Unit
  )

;; Voltage/Emf/Electric-Potential:
(define-units/type Voltage-Unit
  [volt (make-Unit 'volt 1 voltage-dimension)]
  )

;; Capacitance:
(define-units/type Capacitance-Unit
  [farrad (make-Unit 'farrad 1 capacitance-dimension)]
  )

;; Current:
(define-units/type Current-Unit
  [ampere (make-Unit 'ampere 1 current-dimension)]
  )

;; Current-Density:
(define-units/type Current-Density-Unit
  )

;; Resistance:
(define-units/type Resistance-Unit
  [ohm (make-Unit 'ohm 1 resistance-dimension)]
  )

;; Resistivity:
(define-units/type Resistivity-Unit
  )

;; Conductivity:
(define-units/type Conductivity-Unit
  )

;; Magnetic-Field:
(define-units/type Magnetic-Field-Unit
  [tesla (make-Unit 'tesla 1 magnetic-field-dimension)]
  [gauss (u* 1e-4 tesla)]
  )

;; Inductance:
(define-units/type Inductance-Unit
  [henry (make-Unit 'henry 1 inductance-dimension)]
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(untyped-module*
 [10^ (Real -> Real)]
 [#:all-from (submod "dimensions.rkt" untyped)
             (submod "unit-struct.rkt" untyped)
             (submod "unit-operations.rkt" untyped)]
 )


