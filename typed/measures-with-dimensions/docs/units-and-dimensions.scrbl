#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          racket/require
          typed/measures-with-dimensions/untyped-utils
          (for-label (combine-in/priority
                      typed/measures-with-dimensions
                      racket
                      typed/racket))
          "deftype.rkt"
          )

@title{Units and Dimensions}

@section{SI prefixes}

@deftogether[[
  @defproc[(deci [u Unitish]) Unit]
  @defproc[(centi [u Unitish]) Unit]
  @defproc[(milli [u Unitish]) Unit]
  @defproc[(micro [u Unitish]) Unit]
  @defproc[(nano [u Unitish]) Unit]
  @defproc[(pico [u Unitish]) Unit]
  @defproc[(femto [u Unitish]) Unit]
  @defproc[(atto [u Unitish]) Unit]
  @defproc[(zepto [u Unitish]) Unit]
  @defproc[(yocto [u Unitish]) Unit]
  @defproc[(deca [u Unitish]) Unit]
  @defproc[(hecto [u Unitish]) Unit]
  @defproc[(kilo [u Unitish]) Unit]
  @defproc[(mega [u Unitish]) Unit]
  @defproc[(giga [u Unitish]) Unit]
  @defproc[(tera [u Unitish]) Unit]
  @defproc[(peta [u Unitish]) Unit]
  @defproc[(exa [u Unitish]) Unit]
  @defproc[(zetta [u Unitish]) Unit]
  @defproc[(yotta [u Unitish]) Unit]
]]{
functions for the SI prefixes.
}

@section{Dimensionless Units}

@deftogether[[
  @deftype[Dimensionless]
  @deftype[Dimensionless-Number]
  @deftype[Dimensionless-Vector]
  @deftype[Dimensionless-Dimension]
  @deftype[Dimensionless-Unit]
  @defthing[dimensionless-dimension Dimensionless-Dimension]
  @defthing[1-unit Dimensionless-Unit]
  @defthing[thing Dimensionless-Unit]
  @defthing[mol Dimensionless-Unit]
  @defthing[radian Dimensionless-Unit]
  @defthing[turn Dimensionless-Unit]
  @defthing[revolution Dimensionless-Unit]
  @defthing[cycle Dimensionless-Unit]
  @defthing[degree Dimensionless-Unit]
  @defthing[° Dimensionless-Unit]
  @defthing[percent Dimensionless-Unit]
]]

@section{Mass Units}

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
  @defthing[atomic-mass-unit Mass-Unit]
  @defthing[planck-mass Mass-Unit]
]]

@section{Length Units}

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
  @defthing[astronomical-unit Length-Unit]
  @defthing[angstrom Length-Unit]
  @defthing[light-year Length-Unit]
  @defthing[nautical-mile Length-Unit]
  @defthing[planck-length Length-Unit]
]]

@section{Time Units}

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
  @defthing[common-year Time-Unit]
  @defthing[leap-year Time-Unit]
  @defthing[average-month Time-Unit]
  @defthing[average-decade Time-Unit]
  @defthing[century Time-Unit]
  @defthing[millisecond Time-Unit]
  @defthing[microsecond Time-Unit]
  @defthing[nanosecond Time-Unit]
  @defthing[planck-time Time-Unit]
]]

@section{Charge Units}

@deftogether[[
  @deftype[Charge]
  @deftype[Charge-Dimension]
  @deftype[Charge-Unit]
  @defthing[charge-dimension Charge-Dimension]
  @defthing[coulomb Charge-Unit]
  @defthing[elementary-charge-unit Charge-Unit]
  @defthing[planck-charge Charge-Unit]
]]

@section{Absolute-Temperature Units}

@deftogether[[
  @deftype[Absolute-Temperature]
  @deftype[Temperature-Dimension]
  @deftype[Absolute-Temperature-Unit]
  @defthing[temperature-dimension Temperature-Dimension]
  @defthing[kelvin Absolute-Temperature-Unit]
  @defthing[rankine Absolute-Temperature-Unit]
  @defthing[planck-temperature Absolute-Temperature-Unit]
]]

@section{Area Units}

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

@section{Volume Units}

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
  @defthing[tablespoon Volume-Unit]
  @defthing[teaspoon Volume-Unit]
]]

@section{Density Units}

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

@section{Velocity and Speed Units}

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
  @defthing[meter-per-second Velocity-Unit]
  @defthing[mile-per-hour Velocity-Unit]
  @defthing[foot-per-second Velocity-Unit]
  @defthing[knot Velocity-Unit]
  @defthing[speed-of-light-unit Velocity-Unit]
  @defthing[planck-velocity Velocity-Unit]
]]

@section{Acceleration Units}

@deftogether[[
  @deftype[Acceleration]
  @deftype[Acceleration-Vector]
  @deftype[Acceleration-Dimension]
  @deftype[Acceleration-Unit]
  @defthing[acceleration-dimension Acceleration-Dimension]
  @defthing[meter-per-second-squared Acceleration-Unit]
  @defthing[gravitational-acceleration-unit Acceleration-Unit]
]]

@section{Force Units}

@deftogether[[
  @deftype[Force]
  @deftype[Force-Vector]
  @deftype[Force-Dimension]
  @deftype[Force-Unit]
  @defthing[force-dimension Force-Dimension]
  @defthing[newton Force-Unit]
  @defthing[pound-force Force-Unit]
]]

@section{Momentum}

@deftogether[[
  @deftype[Momentum]
  @deftype[Momentum-Vector]
  @deftype[Momentum-Dimension]
  @deftype[Momentum-Unit]
  @defthing[momentum-dimension Momentum-Dimension]
]]

@section{Energy, Work, and Torque Units}

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
  @defthing[british-thermal-unit Energy-Unit]
  @defthing[kilowatt-hour Energy-Unit]
  @defthing[electron-volt Energy-Unit]
]]

@section{Power Units}

@deftogether[[
  @deftype[Power]
  @deftype[Power-Dimension]
  @deftype[Power-Unit]
  @defthing[power-dimension Power-Dimension]
  @defthing[watt Power-Unit]
  @defthing[kilowatt Power-Unit]
  @defthing[horsepower Power-Unit]
]]

@section{Pressure Units}

@deftogether[[
  @deftype[Pressure]
  @deftype[Pressure-Dimension]
  @deftype[Pressure-Unit]
  @defthing[pressure-dimension Pressure-Dimension]
  @defthing[pascal Pressure-Unit]
  @defthing[pound-per-square-inch Pressure-Unit]
  @defthing[atmosphere Pressure-Unit]
  @defthing[bar Pressure-Unit]
  @defthing[millibar Pressure-Unit]
  @defthing[torr Pressure-Unit]
  @defthing[millimeter-of-mercury Pressure-Unit]
]]

@section{Entropy}

@deftogether[[
  @deftype[Entropy]
  @deftype[Entropy-Dimension]
  @deftype[Entropy-Unit]
  @defthing[entropy-dimension Entropy-Dimension]
]]

@section{Electric Field}

@deftogether[[
  @deftype[Electric-Field]
  @deftype[Electric-Field-Vector]
  @deftype[Electric-Field-Dimension]
  @deftype[Electric-Field-Unit]
  @defthing[electric-field-dimension Electric-Field-Dimension]
]]

@section{Voltage, Emf, and Electric Potential Units}

@deftogether[[
  @deftype[Voltage]
  @deftype[Voltage-Dimension]
  @deftype[Voltage-Unit]
  @defthing[voltage-dimension Voltage-Dimension]
  @defthing[volt Voltage-Unit]
]]

@section{Capacitance Units}

@deftogether[[
  @deftype[Capacitance]
  @deftype[Capacitance-Dimension]
  @deftype[Capacitance-Unit]
  @defthing[capacitance-dimension Capacitance-Dimension]
  @defthing[farrad Capacitance-Unit]
]]

@section{Current and Current-Density Units}

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

@section{Resistance, Resistivity, and Conductivity Units}

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
  @defthing[ohm-meter Resistivity-Unit]
]]

@deftogether[[
  @deftype[Conductivity]
  @deftype[Conductivity-Dimension]
  @deftype[Conductivity-Unit]
  @defthing[conductivity-dimension Conductivity-Dimension]
]]

@section{Magnetic Field Units}

@deftogether[[
  @deftype[Magnetic-Field]
  @deftype[Magnetic-Field-Vector]
  @deftype[Magnetic-Field-Dimension]
  @deftype[Magnetic-Field-Unit]
  @defthing[magnetic-field-dimension Magnetic-Field-Dimension]
  @defthing[tesla Magnetic-Field-Unit]
  @defthing[gauss Magnetic-Field-Unit]
]]

@section{Inductance Units}

@deftogether[[
  @deftype[Inductance]
  @deftype[Inductance-Dimension]
  @deftype[Inductance-Unit]
  @defthing[inductance-dimension Inductance-Dimension]
  @defthing[henry Inductance-Unit]
]]


