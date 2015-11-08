#lang sweet-exp typed/racket/base

provide (all-defined-out)

require syntax/parse/define
        "../units/unit-struct.rkt"
        "../units/unit-operations.rkt"
        "../units/units.rkt"
        "measure-struct.rkt"
        "../untyped-utils.rkt"
        for-syntax racket/base


(define-simple-macro
  (defmulti-types [id:id ty:expr] ...)
  (begin (define-type id ty) ...))

(define-simple-macro
  (defmulti-types/Measureof unit-ty:expr
    [id:id num-ty:expr] ...)
  (defmulti-types [id (Measureof num-ty unit-ty)] ...))

(defmulti-types/Measureof Dimensionless-Unit
  [Dimensionless Num/Vec]
  [Dimensionless-Number Number]
  [Dimensionless-Real Real]
  [Dimensionless-Vector Vec])
(define-type Dimensionless-Mesarue Dimensionless)

(defmulti-types/Measureof Mass-Unit
  [Mass Nonnegative-Real]
  [Mass-Real-Measure Real]
  [Mass-Measure Num/Vec])
(defmulti-types/Measureof Length-Unit
  [Position Real/Vec]
  [Length Nonnegative-Real]
  [Position-Vector Vec]
  [Length-Real-Measure Real]
  [Length-Measure Num/Vec])
(defmulti-types/Measureof Time-Unit
  [Time Real]
  [Time-Measure Num/Vec])
(defmulti-types/Measureof Charge-Unit
  [Charge Real]
  [Charge-Measure Num/Vec])
(defmulti-types/Measureof Absolute-Temperature-Unit
  [Absolute-Temperature Nonnegative-Real]
  [Temperature-Real-Measure Real]
  [Temperature-Measure Num/Vec])

(defmulti-types/Measureof Area-Unit
  [Area Nonnegative-Real]
  [Area-Real-Measure Real]
  [Area-Measure Num/Vec])
(defmulti-types/Measureof Volume-Unit
  [Volume Nonnegative-Real]
  [Volume-Real-Measure Real]
  [Volume-Measure Num/Vec])

(defmulti-types/Measureof Mass-Density-Unit
  [Mass-Density Nonnegative-Real]
  [Mass-Density-Real-Measure Real]
  [Mass-Density-Measure Num/Vec])
(defmulti-types/Measureof Charge-Density-Unit
  [Charge-Density Real]
  [Change-Density-Measure Num/Vec])

(defmulti-types/Measureof Velocity-Unit
  [Velocity Real/Vec]
  [Speed Nonnegative-Real]
  [Velocity-Vector Vec]
  [Velocity-Real-Measure Real]
  [Velocity-Measure Num/Vec])
(defmulti-types/Measureof Acceleration-Unit
  [Acceleration Real/Vec]
  [Acceleration-Real-Measure Real]
  [Acceleration-Vector Vec]
  [Acceleration-Measure Num/Vec])
(defmulti-types/Measureof Force-Unit
  [Force Real/Vec]
  [Force-Vector Vec]
  [Force-Real-Measure Real]
  [Force-Measure Num/Vec])
(defmulti-types/Measureof Momentum-Unit
  [Momentum Real/Vec]
  [Momentum-Vector Vec]
  [Momentum-Real-Measure Real]
  [Momentum-Measure Num/Vec])

(defmulti-types/Measureof Energy-Unit
  [Energy Real]
  [Work Real]
  [Energy/Work/Torqe-Measure Num/Vec])
(defmulti-types/Measureof Torque-Unit
  [Torque Real/Vec]
  [Torque-Vector Vec])
(defmulti-types/Measureof Power-Unit
  [Power Real]
  [Power-Measure Num/Vec])

(defmulti-types/Measureof Pressure-Unit
  [Pressure Nonnegative-Real]
  [Pressure-Real-Measure Real]
  [Pressure-Measure Num/Vec])

(defmulti-types/Measureof Entropy-Unit
  [Entropy Real]
  [Heat-Capacity Nonnegative-Real]
  [Molar-Specific-Heat Nonnegative-Real]
  [Entropy/Heat-Capacity/Molar-Specific-Heat-Measure Num/Vec])
(defmulti-types/Measureof Specific-Heat-Unit
  [Specific-Heat Nonnegative-Real]
  [Specific-Heat-Real-Measure Real]
  [Specific-Heat-Measure Num/Vec])

(defmulti-types/Measureof Electric-Field-Unit
  [Electric-Field Real/Vec]
  [Electric-Field-Vector Vec]
  [Electric-Field-Real-Measure Real]
  [Electric-Field-Measure Num/Vec])
(defmulti-types/Measureof Electric-Potential-Unit
  [Electric-Potential Real]
  [Electric-Potential-Measure Num/Vec])
(defmulti-types
  [Voltage Electric-Potential]
  [Emf Voltage])
(defmulti-types/Measureof Capacitance-Unit
  [Capacitance Nonnegative-Real]
  [Capacitance-Real-Measure Real]
  [Capacitance-Measure Num/Vec])

(defmulti-types/Measureof Current-Unit
  [Current Real]
  [Current-Measure Num/Vec])
(defmulti-types/Measureof Current-Density-Unit
  [Current-Density Real/Vec]
  [Current-Density-Vector Vec]
  [Current-Density-Real-Measure Real]
  [Current-Density-Measure Num/Vec])
(defmulti-types/Measureof Resistance-Unit
  [Resistance Nonnegative-Real]
  [Resistance-Real-Measure Real]
  [Resistance-Measure Num/Vec])
(defmulti-types/Measureof Resistivity-Unit
  [Resistivity Nonnegative-Real]
  [Resistivity-Real-Measure Real]
  [Resistivity-Measure Num/Vec])
(defmulti-types/Measureof Conductivity-Unit
  [Conductivity Nonnegative-Real]
  [Conductivity-Real-Measure Real]
  [Conductivity-Measure Num/Vec])

(defmulti-types/Measureof Magnetic-Field-Unit
  [Magnetic-Field Real/Vec]
  [Magnetic-Field-Vector Vec]
  [Magnetic-Field-Real-Measure Real]
  [Magnetic-Field-Measure Num/Vec])

(defmulti-types/Measureof Electric-Flux-Unit
  [Electric-Flux Real]
  [Electric-Flux-Measure Num/Vec])
(defmulti-types/Measureof Magnetic-Flux-Unit
  [Magnetic-Flux Real]
  [Magnetic-Flux-Measure Num/Vec])

(defmulti-types/Measureof Inductance-Unit
  [Inductance Nonnegative-Real]
  [Inductance-Real-Measure Real]
  [Inductance-Measure Num/Vec])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(untyped-module*
 )


