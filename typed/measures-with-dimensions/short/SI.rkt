#lang typed/racket/base

(provide (all-defined-out))

(require "../units.rkt"
         "../defmulti.rkt"
         )

(defmulti
  [kg kilogram]
  [g gram]
  [m meter]
  [cm centimeter]
  [mm millimeter]
  [km kilometer]
  [dm decimeter]
  [s second]
  [ms millisecond]
  [µs microsecond]
  [ns nanosecond]
  [C coulomb]
  [µC microcoulomb]
  [nC nanocoulomb]
  [pC picocoulomb]
  [K kelvin]
  [m^2 square-meter]
  [cm^2 square-centimeter]
  [m^3 cubic-meter]
  [cm^3 cubic-centimeter]
  [L liter]
  [mL milliliter]
  [m/s meter-per-second]
  [m/s^2 meter-per-second-squared]
  [N newton]
  [W watt]
  [kW kilowatt]
  [J joule]
  [Nm newton-meter]
  [cal calorie]
  [eV electron-volt]
  [Pa pascal]
  [atm atmosphere]
  [V volt]
  [A ampere]
  [Ω ohm]
  [Ωm ohm-meter]
  [T tesla]
  )
