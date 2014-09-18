#lang typed/racket/base

(provide (all-defined-out))

(require "unit-struct.rkt"
         "unit-operations.rkt"
         "units.rkt"
         "measure-struct.rkt"
         "untyped-utils.rkt"
         )

(define-type Dimensionless
  (Measureof (U Real (Vectorof Real)) Dimensionless-Unit))
(define-type Dimensionless-Number
  (Measureof Real Dimensionless-Unit))
(define-type Dimensionless-Vector
  (Measureof (Vectorof Real) Dimensionless-Unit))

(define-type Mass
  (Measureof Nonnegative-Real Mass-Unit))
(define-type Position
  (Measureof (U Real (Vectorof Real)) Length-Unit))
(define-type Length
  (Measureof Nonnegative-Real Length-Unit))
(define-type Position-Vector
  (Measureof (Vectorof Real) Length-Unit))
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
(define-type Torque
  (Measureof (U Real (Vectorof Real)) Torque-Unit))
(define-type Torque-Vector
  (Measureof (Vectorof Real) Torque-Unit))
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

(untyped-module*
 )


