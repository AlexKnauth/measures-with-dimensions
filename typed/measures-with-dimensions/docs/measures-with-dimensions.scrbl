#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          racket/require
          typed/measures-with-dimensions/untyped-utils
          (for-label (combine-in/priority
                      (submod typed/measures-with-dimensions untyped)
                      racket
                      typed/racket))
          (for-syntax racket/base
                      syntax/parse))

@title{measures-with-dimensions}

@defmodule[(submod typed/measures-with-dimensions untyped)]

This module provides structs to represent measures, units, and dimensions,
and functions and macros to manipulate these.

@section{Operations}

@subsection{Operations on Measures}

@defform[(m expr-or-op ...)
         #:grammar ([expr-or-op expr op]
                    [op @#,tt{+} @#,tt{-} @#,tt{*} @#,tt{/} @#,tt{^}])]{
this macro lets you write measures and operations on measures using infix notation with the symbols
@tt{+}, @tt{-}, @tt{*}, @tt{/}, and @tt{^}.

It is syntactic sugar for using @racket[m+], @racket[m-], @racket[m*], @racket[m1/], and
@racket[mexpt] in prefix notation.

@examples[
  (require (submod typed/measures-with-dimensions untyped))
  (m 1 meter)
  (m 50 centimeter)
  (m 1 meter + 50 centimeter)
  (m 1 meter - 50 centimeter)
  (m 1 meter ^ 2)
  (m 1 meter ^ 2 + 100 centimeter ^ 2)
]}

@defproc[(m+ [m Measureish] ...) Measure]{
adds the measures together.
}

@defproc[(m- [m Measureish]) Measure]{
negates a measure.
}

@defproc[(m* [m Measureish] ...) Measure]{
multiplies the measures.
}

@defproc[(m1/ [m Measureish]) Measure]{
takes the multiplicative inverse of the measure.
}

@defproc[(mexpt [m1 Measureish] [m2 Measureish]) Measure]{
takes @racket[m1] to the exponent of @racket[m2].
}

@subsection{Operations on Units and Dimensions}

@defproc[(u* [u Unitish] ...) Unit]{
multiplies the units.
}

@defproc[(u1/ [u Unitish]) Unit]{
takes the multiplicative inverse of @racket[u].
}

@defproc[(u/ [u1 Unitish] [u Unitish] ...+) Unit]{
divides @racket[u1] by the @racket[u]s.
}

@defproc[(uexpt [u Unitish] [n Exact-Rational]) Unit]{
takes @racket[u] to the exponent of @racket[n].
}

@defproc[(d* [u Dimension] ...) Dimension]{
multiplies the dimensions.
}

@defproc[(d1/ [d Dimension]) Dimension]{
takes the multiplicative inverse of @racket[d].
}

@defproc[(d/ [d1 Dimension] [d Dimension] ...+) Dimension]{
divides @racket[d1] by the @racket[d]s.
}

@defproc[(dexpt [d Dimension] [n Exact-Rational]) Dimension]{
takes @racket[d] to the exponent of @racket[n].
}

@section{Types and Structs}

@(define-syntax deftype
   (syntax-parser
     [@deftype[id:id stuff:expr ...]
      #'@defidform[#:kind "type" id stuff ...]]
     [@deftype[(id:id . args:expr) stuff:expr ...]
      #'@defform[#:kind "type" (id . args) stuff ...]]))

@deftype[Measure]

@deftype[Measureish]

@deftype[Unit]

@deftype[Unitish]

@deftype[Dimension]

@deftype[Sig-Figs]{
equivalent to @racket[(U Positive-Integer +inf.0)].
}

@defstruct*[dimension ([M-expt Integer]
                       [L-expt Integer]
                       [T-expt Integer]
                       [Q-expt Integer]
                       [Θ-expt Integer])]{
a struct representing a dimension.
}

@defstruct*[unit ([name Any] [scalar Positive-Real] [dimension Dimension])]{
a struct representing a unit.
}

@defstruct*[measure ([number (U Number (Vectorof Real))]
                     [unit Unit]
                     [sig-figs Sig-Figs])]{
a struct representing a measure.
}

@section{Units}

@subsection{SI prefixes}

@deftogether[[
  @defproc[(deci [u Unitish]) Unit]
  @defproc[(centi [u Unitish]) Unit]
  @defproc[(milli [u Unitish]) Unit]
  @defproc[(micro [u Unitish]) Unit]
  @defproc[(nano [u Unitish]) Unit]
  @defproc[(pico [u Unitish]) Unit]
  @defproc[(deca [u Unitish]) Unit]
  @defproc[(hecto [u Unitish]) Unit]
  @defproc[(kilo [u Unitish]) Unit]
  @defproc[(mega [u Unitish]) Unit]
  @defproc[(giga [u Unitish]) Unit]
  @defproc[(tera [u Unitish]) Unit]
]]{
functions for the SI prefixes.
}

@subsection{Dimensionless Units}

@deftogether[[
  @deftype[Dimensionless]
  @deftype[Dimensionless-Number]
  @deftype[Dimensionless-Vector]
  @deftype[Dimensionless-Dimension]
  @deftype[Dimensionless-Unit]
  @defthing[dimensionless-dimension Dimensionless-Dimension]
  @defthing[1-unit Dimensionless-Unit]
  @defthing[mol Dimensionless-Unit]
  @defthing[radian Dimensionless-Unit]
  @defthing[turn Dimensionless-Unit]
  @defthing[degree Dimensionless-Unit]
  @defthing[percent Dimensionless-Unit]
]]

@subsection{Mass Units}

@deftogether[[
  @deftype[Mass]
  @deftype[Mass-Dimension]
  @deftype[Mass-Unit]
  @defthing[mass-dimension Mass-Dimension]
  @defthing[kilogram Mass-Unit]
  @defthing[gram Mass-Unit]
  @defthing[pound-mass Mass-Unit]
  @defthing[ounce-mass Mass-Unit]
  @defthing[ton Mass-Unit]
]]

@subsection{Length Units}

@deftogether[[
  @deftype[Position]
  @deftype[Length]
  @deftype[Position-Vector]
  @deftype[Length-Dimension]
  @deftype[Length-Unit]
  @defthing[length-dimension Length-Dimension]
  @defthing[meter Length-Unit]
  @defthing[centimeter Length-Unit]
  @defthing[millimeter Length-Unit]
  @defthing[kilometer Length-Unit]
  @defthing[decimeter Length-Unit]
  @defthing[foot Length-Unit]
  @defthing[inch Length-Unit]
  @defthing[yard Length-Unit]
  @defthing[mile Length-Unit]
]]

@subsection{Time Units}

@deftogether[[
  @deftype[Time]
  @deftype[Time-Dimension]
  @deftype[Time-Unit]
  @defthing[time-dimension Time-Dimension]
  @defthing[second Time-Unit]
  @defthing[minute Time-Unit]
  @defthing[hour Time-Unit]
  @defthing[day Time-Unit]
  @defthing[week Time-Unit]
  @defthing[average-year Time-Unit]
  @defthing[average-month Time-Unit]
  @defthing[average-decade Time-Unit]
  @defthing[century Time-Unit]
  @defthing[millisecond Time-Unit]
  @defthing[microsecond Time-Unit]
  @defthing[nanosecond Time-Unit]
]]

@subsection{Charge Units}

@deftogether[[
  @deftype[Charge]
  @deftype[Charge-Dimension]
  @deftype[Charge-Unit]
  @defthing[charge-dimension Charge-Dimension]
  @defthing[coulomb Charge-Unit]
]]

@subsection{Absolute-Temperature Units}

@deftogether[[
  @deftype[Absolute-Temperature]
  @deftype[Temperature-Dimension]
  @deftype[Absolute-Temperature-Unit]
  @defthing[temperature-dimension Temperature-Dimension]
  @defthing[kelvin Absolute-Temperature-Unit]
  @defthing[rankine Absolute-Temperature-Unit]
]]

@subsection{Area Units}

@deftogether[[
  @deftype[Area]
  @deftype[Area-Dimension]
  @deftype[Area-Unit]
  @defthing[area-dimension Area-Dimension]
  @defthing[square-meter Area-Unit]
  @defthing[square-centimeter Area-Unit]
  @defthing[square-foot Area-Unit]
  @defthing[square-inch Area-Unit]
  @defthing[square-mile Area-Unit]
  @defthing[acre Area-Unit]
]]

@subsection{Volume Units}

@deftogether[[
  @deftype[Volume]
  @deftype[Volume-Dimension]
  @deftype[Volume-Unit]
  @defthing[volume-dimension Volume-Dimension]
  @defthing[cubic-meter Volume-Unit]
  @defthing[cubic-centimeter Volume-Unit]
  @defthing[liter Volume-Unit]
  @defthing[milliliter Volume-Unit]
  @defthing[cubic-foot Volume-Unit]
  @defthing[cubic-inch Volume-Unit]
  @defthing[gallon Volume-Unit]
  @defthing[quart Volume-Unit]
  @defthing[pint Volume-Unit]
  @defthing[cup Volume-Unit]
  @defthing[fluid-ounce Volume-Unit]
]]

@subsection{Density Units}

@deftogether[[
  @deftype[Mass-Density]
  @deftype[Mass-Density-Dimension]
  @deftype[Mass-Density-Unit]
  @defthing[mass-density-dimension Mass-Density-Dimension]
]]

@deftogether[[  
  @deftype[Charge-Density]
  @deftype[Charge-Density-Dimension]
  @deftype[Charge-Density-Unit]
  @defthing[charge-density-dimension Charge-Density-Dimension]
]]

@subsection{Velocity and Speed Units}

@deftogether[[
  @deftype[Velocity]
  @deftype[Speed]
  @deftype[Velocity-Vector]
  @deftype[Velocity-Dimension]
  @deftype[Speed-Dimension]
  @deftype[Velocity-Unit]
  @deftype[Speed-Unit]
  @defthing[velocity-dimension Velocity-Dimension]
  @defthing[speed-dimension Speed-Dimension]
  @defthing[m/s Velocity-Unit]
  @defthing[mph Velocity-Unit]
]]

@subsection{Acceleration Units}

@deftogether[[
  @deftype[Acceleration]
  @deftype[Acceleration-Vector]
  @deftype[Acceleration-Dimension]
  @deftype[Acceleration-Unit]
  @defthing[acceleration-dimension Acceleration-Dimension]
  @defthing[m/s^2 Acceleration-Unit]
]]

@subsection{Force Units}

@deftogether[[
  @deftype[Force]
  @deftype[Force-Vector]
  @deftype[Force-Dimension]
  @deftype[Force-Unit]
  @defthing[force-dimension Force-Dimension]
  @defthing[newton Force-Unit]
  @defthing[pound-force Force-Unit]
]]

@subsection{Momentum}

@deftogether[[
  @deftype[Momentum]
  @deftype[Momentum-Vector]
  @deftype[Momentum-Dimension]
  @deftype[Momentum-Unit]
  @defthing[momentum-dimension Momentum-Dimension]
]]

@subsection{Energy, Work, and Torque Units}

@deftogether[[
  @deftype[Energy]
  @deftype[Work]
  @deftype[Torque]
  @deftype[Torque-Vector]
  @deftype[Energy-Dimension]
  @deftype[Work-Dimension]
  @deftype[Torque-Dimension]
  @deftype[Energy-Unit]
  @deftype[Work-Unit]
  @deftype[Torque-Unit]
  @defthing[energy-dimension Energy-Dimension]
  @defthing[work-dimension Work-Dimension]
  @defthing[torque-dimension Torque-Dimension]
  @defthing[joule Energy-Unit]
  @defthing[newton-meter Work-Unit]
  @defthing[calorie Energy-Unit]
  @defthing[foot-pound Work-Unit]
]]

@subsection{Power Units}

@deftogether[[
  @deftype[Power]
  @deftype[Power-Dimension]
  @deftype[Power-Unit]
  @defthing[power-dimension Power-Dimension]
  @defthing[watt Power-Unit]
  @defthing[kilowatt Power-Unit]
  @defthing[horsepower Power-Unit]
]]

@subsection{Pressure Units}

@deftogether[[
  @deftype[Pressure]
  @deftype[Pressure-Dimension]
  @deftype[Pressure-Unit]
  @defthing[pressure-dimension Pressure-Dimension]
  @defthing[pascal Pressure-Unit]
  @defthing[psi Pressure-Unit]
  @defthing[atmosphere Pressure-Unit]
  @defthing[bar Pressure-Unit]
  @defthing[millibar Pressure-Unit]
]]

@subsection{Entropy}

@deftogether[[
  @deftype[Entropy]
  @deftype[Entropy-Dimension]
  @deftype[Entropy-Unit]
  @defthing[entropy-dimension Entropy-Dimension]
]]

@subsection{Electric Field}

@deftogether[[
  @deftype[Electric-Field]
  @deftype[Electric-Field-Vector]
  @deftype[Electric-Field-Dimension]
  @deftype[Electric-Field-Unit]
  @defthing[electric-field-dimension Electric-Field-Dimension]
]]

@subsection{Voltage, Emf, and Electric Potential Units}

@deftogether[[
  @deftype[Voltage]
  @deftype[Voltage-Dimension]
  @deftype[Voltage-Unit]
  @defthing[voltage-dimension Voltage-Dimension]
  @defthing[volt Voltage-Unit]
]]

@subsection{Capacitance Units}

@deftogether[[
  @deftype[Capacitance]
  @deftype[Capacitance-Dimension]
  @deftype[Capacitance-Unit]
  @defthing[capacitance-dimension Capacitance-Dimension]
  @defthing[farrad Capacitance-Unit]
]]

@subsection{Current and Current-Density Units}

@deftogether[[
  @deftype[Current]
  @deftype[Current-Dimension]
  @deftype[Current-Unit]
  @defthing[current-dimension Current-Dimension]
  @defthing[ampere Current-Unit]
]]

@deftogether[[
  @deftype[Current-Density]
  @deftype[Current-Density-Vector]
  @deftype[Current-Density-Dimension]
  @deftype[Current-Density-Unit]
  @defthing[current-density-dimension Current-Density-Dimension]
]]

@subsection{Resistance, Resistivity, and Conductivity Units}

@deftogether[[
  @deftype[Resistance]
  @deftype[Resistance-Dimension]
  @deftype[Resistance-Unit]
  @defthing[resistance-dimension Resistance-Dimension]
  @defthing[ohm Resistance-Unit]
]]

@deftogether[[
  @deftype[Resistivity]
  @deftype[Resistivity-Dimension]
  @deftype[Resistivity-Unit]
  @defthing[resistivity-dimension Resistivity-Dimension]
]]

@deftogether[[
  @deftype[Conductivity]
  @deftype[Conductivity-Dimension]
  @deftype[Conductivity-Unit]
  @defthing[curductivity-dimension Conductivity-Dimension]
]]

@subsection{Magnetic Field Units}

@deftogether[[
  @deftype[Magnetic-Field]
  @deftype[Magnetic-Field-Vector]
  @deftype[Magnetic-Field-Dimension]
  @deftype[Magnetic-Field-Unit]
  @defthing[magnetic-field-dimension Magnetic-Field-Dimension]
  @defthing[tesla Magnetic-Field-Unit]
]]

@subsection{Inductance Units}

@deftogether[[
  @deftype[Inductance]
  @deftype[Inductance-Dimension]
  @deftype[Inductance-Unit]
  @defthing[inductance-dimension Inductance-Dimension]
  @defthing[henry Inductance-Unit]
]]






