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

@title{Operations, Types, and Structs}

@section{Operations}

@subsection{Operations on Measures}

@defform*[[(m expr-or-op ...)
           (m expr-or-op ... : type)]
          #:grammar ([expr-or-op expr op]
                     [op @#,tt{+} @#,tt{-} @#,tt{*} @#,tt{/} @#,tt{^}])]{
this macro lets you write measures and operations on measures using infix notation with the symbols
@tt{+}, @tt{-}, @tt{*}, @tt{/}, and @tt{^}.

It is syntactic sugar for using @racket[m+], @racket[m-], @racket[m*], @racket[m1/], and
@racket[mexpt] in prefix notation.

@examples[
  (require typed/racket)
  (require typed/measures-with-dimensions)
  (m 1 meter)
  (m 50 centimeter)
  (m 1 meter + 50 centimeter)
  (m 1 meter - 50 centimeter)
  (m 1 meter ^ 2)
  (m 1 meter ^ 2 + 100 centimeter ^ 2)
  (m 1 foot + 3 inch)
  (m 1 meter - 50 centimeter : Length-Real-Measure)
]}

@defproc[(m+ [m Measureish] ...) Measure]{
adds the measures together.

@margin-note{
Note for typed racket users: when the typechecker typechecks @racket[m+], it doesn't actually check
that its arguments have the same dimensions (it does check at runtime).  Instead it typechecks as
returning a measure with a union of the dimensions of its arguments.  For example,
@racket[(m+ (make-measure 1 meter) (make-measure 1 second))] typechecks as something that could have
either the length dimension or the time dimension.  You can fix this by either requiring the type of
the result to be what you want, or using @racket[inst] to instantiate it for a certain dimension.  
}

@examples[
  (require typed/racket)
  (require typed/measures-with-dimensions)
  (ann (m+) Zero-Measure)
  (m+ (m 1 meter))
  (m+ (m 1 meter) (m 50 centimeter))
  (m 1 foot + 3 inch)
  ((inst m+ Length-Dimension) (m 1 meter) (m 50 centimeter))
]}

@defproc[(m- [m Measureish]) Measure]{
negates a measure.  To do subtraction, either use the @racket[m] macro or use a pattern like
@racket[(m+ a (m- b))].

@examples[
  (require typed/racket)
  (require typed/measures-with-dimensions)
  (m- (m 1 meter))
  (m 1 meter - 50 centimeter)
]}

@defproc[(m* [m Measureish] ...) Measure]{
multiplies the measures.
}

@defproc[(m1/ [m Number-Measureish]) Number-Measure]{
takes the multiplicative inverse of the measure.  To do division, either use the @racket[m] macro or
use a pattern like @racket[(m* a (m1/ b))].
}

@defproc[(mexpt [m1 Number-Measureish] [m2 Number-Measureish]) Number-Measure]{
takes @racket[m1] to the exponent of @racket[m2].
}

@defproc*[([(convert [m Measure] [u Unitish]) Measure]
           [(convert [n (U Number (Vectorof Real))] [u1 Unitish] [u2 Unitish]) Measure])]{
The first form converts the measure @racket[m] to the unit @racket[u].
The second form converts the number or vector @racket[n] from @racket[u1] to @racket[u2].

@examples[
  (require typed/racket)
  (require typed/measures-with-dimensions)
  (convert (m 1 meter) centimeter)
  (convert 1 meter centimeter)
]}

@defproc[(measure=? [m1 Measureish] [m Measureish] ...) Boolean]{
returns true if the measures represent the same quantities.

@examples[
  (require typed/racket)
  (require typed/measures-with-dimensions)
  (measure=? (m 1 meter)
             (m 100 centimeter))
  (measure=? (m 1 meter)
             (m 1 kilogram))
  (measure=? (m 1 radian)
             (m 1 degree))
]}

@defproc[(->measure [m Measureish]) Measure]{
converts from something "measureish" to a @racket[Measure].
}

@deftogether[[
  @defproc[(measure->num/vec [m Measureish]) (U Number (Vectorof Real))]
  @defproc[(measure->number [m Number-Measureish]) Number]
  @defproc[(measure->real [m Real-Measureish]) Real]
  @defproc[(measure->vector [m Vector-Measureish]) (Vectorof Real)]]]{
Functions for converting dimensionless measures to numbers or vectors.
For measures that have a unit with a scalar, it converts to @racket[1-unit]
first.  So @racket[mol]s will be multiplied my approx. 6.02*10^23,
@racket[percent]s will be divided by 100, @racket[degree]s will be multiplied by
2π/360, etc. 
}

@subsection{Operations on Units and Dimensions}

@deftogether[[
  @defproc[(u* [u Unitish] ...) Unit]
  @defproc[(u1/ [u Unitish]) Unit]
  @defproc[(u/ [u1 Unitish] [u Unitish] ...+) Unit]
  @defproc[(uexpt [u Unitish] [n Exact-Rational]) Unit]
  @defproc[(->unit [u Unitish]) Unit]
  @defproc[(unit-rename [u Unit] [n Any]) Unit]
  @defproc[(unit=? [u1 Unitish] [u Unitish] ...) Boolean]
]]{
operations on units
}

@deftogether[[
  @defproc[(d* [u Dimension] ...) Dimension]
  @defproc[(d1/ [d Dimension]) Dimension]
  @defproc[(d/ [d1 Dimension] [d Dimension] ...+) Dimension]
  @defproc[(dexpt [d Dimension] [n Exact-Rational]) Dimension]
  @defproc[(dimension=? [d1 Dimension] [d Dimension] ...) Boolean]
]]{
operations on dimensions
}



@section{Types and Structs}

@deftype[Measure]{
equivalent to @racket[(Measureof (U Number (Vectorof Real)) Unit)].
}

@deftype[(Measureof n u)]{
equivalent to @racket[(measure n u Sig-Figs)].
}

@deftype[Number-Measure]{
equivalent to @racket[(Number-Measureof Unit)].
}

@deftype[Vector-Measure]{
equivalent to @racket[(Vector-Measureof Unit)].
}

@deftype[(Number-Measureof u)]{
equivalent to @racket[(Measureof Number u)].
}

@deftype[(Vector-Measureof u)]{
equivalent to @racket[(Measureof (Vectorof Real) u)].
}

@deftogether[[
  @deftype[Measureish]
  @deftype[Number-Measureish]
  @deftype[Vector-Measureish]
]]

@deftype[Unit]{
equivalent to @racket[(Unitof Dimension)].
}

@deftype[(Unitof d)]{
the type for a unit with the dimension @racket[d].
}

@deftype[Unitish]

@deftype[Dimension]{
equivalent to the type @racket[(dimension Integer Integer Integer Integer Integer)].
}

@deftype[Sig-Figs]{
equivalent to @racket[(U Positive-Integer +inf.0)].
}

@deftogether[[
  @defstruct*[dimension ([M-expt Integer]
                         [L-expt Integer]
                         [T-expt Integer]
                         [Q-expt Integer]
                         [Θ-expt Integer])]
  @defproc[(make-dimension [#:M^ M-expt Integer]
                           [#:L^ L-expt Integer]
                           [#:T^ T-expt Integer]
                           [#:Q^ Q-expt Integer]
                           [#:Θ^ Θ-expt Integer])
           Dimension]
]]{
a struct representing a dimension.
}

@deftogether[[
  @defstruct*[unit ([name Any] [scalar Positive-Real] [dimension Dimension])]
]]{
a struct representing a unit.
}

@deftogether[[
  @defstruct*[measure ([number (U Number (Vectorof Real))]
                       [unit Unit]
                       [sig-figs Sig-Figs])]
  @defproc[(make-measure [number (U Number (Vectorof Real))]
                         [unit Unit]
                         [sig-figs Sig-Figs +inf.0])
           Measure]
  @defproc[(measure-dimension [m Measure]) Dimension]
]]{
a struct representing a measure.
}


