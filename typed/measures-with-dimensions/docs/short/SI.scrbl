#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          racket/require
          typed/measures-with-dimensions/untyped-utils
          (for-label (combine-in/priority
                      typed/measures-with-dimensions/short/SI
                      (submod typed/measures-with-dimensions untyped)
                      racket
                      typed/racket))
          "../deftype.rkt"
          )

@title{SI}

@defmodule[typed/measures-with-dimensions/short/SI]

@deftogether[[
  @defthing[kg Mass-Unit]
  @defthing[g Mass-Unit]
  @defthing[m Length-Unit]
  @defthing[cm Length-Unit]
  @defthing[mm Length-Unit]
  @defthing[km Length-Unit]
  @defthing[dm Length-Unit]
  @defthing[s Time-Unit]
  @defthing[ms Time-Unit]
  @defthing[µs Time-Unit]
  @defthing[ns Time-Unit]
  @defthing[C Charge-Unit]
  @defthing[µC Charge-Unit]
  @defthing[nC Charge-Unit]
  @defthing[pC Charge-Unit]
  @defthing[K Absolute-Temperature-Unit]
  @defthing[m^2 Area-Unit]
  @defthing[cm^2 Area-Unit]
  @defthing[m^3 Volume-Unit]
  @defthing[cm^3 Volume-Unit]
  @defthing[L Volume-Unit]
  @defthing[mL Volume-Unit]
  @defthing[m/s Velocity-Unit]
  @defthing[m/s^2 Acceleration-Unit]
  @defthing[N Force-Unit]
  @defthing[J Energy-Unit]
  @defthing[Nm Work-Unit]
  @defthing[cal Energy-Unit]
  @defthing[eV Energy-Unit]
  @defthing[W Power-Unit]
  @defthing[kW Power-Unit]
  @defthing[Pa Pressure-Unit]
  @defthing[kPa Pressure-Unit]
  @defthing[atm Pressure-Unit]
  @defthing[V Voltage-Unit]
  @defthing[A Current-Unit]
  @defthing[Ω Resistance-Unit]
  @defthing[Ωm Resistivity-Unit]
  @defthing[T Magnetic-Field-Unit]
]]
