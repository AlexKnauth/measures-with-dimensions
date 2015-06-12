#lang scribble/manual

@(require scribble/eval
          racket/require
          typed/measures-with-dimensions/untyped-utils
          (for-label (combine-in/priority
                      typed/measures-with-dimensions
                      racket
                      typed/racket)))

@title{Temperatures}

@defmodule[typed/measures-with-dimensions/temperature #:no-declare]{

Celsius and Fahrenheit temperatures are not represented as @racket[measure]s,
but temperatures in an absolute temperature scale such as @racket[kelvin] or
@racket[rankine] are. 

So instead, @racket[celsius] and @racket[fahrenheit] are functions that take
numbers and convert them to an absolute temperature scale like
@racket[kelvin] or @racket[rankine], and functions like @racket[get-celsius]
and @racket[get-fahrenheit] take @racket[measure]s and produce the
corrosponding temperature in celsius or fahrenheit, represented as a number.  
}

@defproc[(celsius [n Real]) Absolute-Temperature]{
Takes a temperature in celsius, represented as a number, and produces a
@racket[measure] representing the corrosponding temperature in @racket[kelvin].
@examples[
  (require typed/racket)
  (require typed/measures-with-dimensions)
  (celsius 0.0)
  (celsius 100.0)
  (celsius -273.15)
]}

@defproc[(fahrenheit [n Real]) Absolute-Temperature]{
Takes a temperature in fahrenheit (represented as a number), and produces a
@racket[measure] representing the corrosponding temperature in @racket[rankine].
(Rankine is an absolute temperature scale where 0 is at absolute zero, but the
size of one degree is the same as the size of one fahrenheit degree)
@examples[
  (require typed/racket)
  (require typed/measures-with-dimensions)
  (fahrenheit 32.0)
  (fahrenheit 212.0)
  (fahrenheit 0.0)
  (fahrenheit -459.67)
]}

@defproc[(get-kelvin [m Absolute-Temperature]) Nonnegative-Real]{
Takes a @racket[measure] and produces the corrosponding temperature in
kelvin, represented as a number.
@examples[
  (require typed/racket)
  (require typed/measures-with-dimensions)
  (get-kelvin (make-measure 300 kelvin))
  (get-kelvin (celsius 0.0))
  (get-kelvin (celsius 100.0))
  (get-kelvin (fahrenheit 32.0))
]}

@defproc[(get-rankine [m Absolute-Temperature]) Nonnegative-Real]{
Takes a @racket[measure] and produces the corrosponding temperature in
rankines, represented as a number.
@examples[
  (require typed/racket)
  (require typed/measures-with-dimensions)
  (get-rankine (make-measure 500 rankine))
  (get-rankine (celsius 0.0))
  (get-rankine (fahrenheit 32.0))
  (get-rankine (fahrenheit 212.0))
]}

@defproc[(get-celsius [m Absolute-Temperature]) Real]{
Takes a @racket[measure] and produces the corrosponding temperature in
celsius, represented as a number.
@examples[
  (require typed/racket)
  (require typed/measures-with-dimensions)
  (get-celsius (make-measure 273.15 kelvin))
  (get-celsius (celsius 0))
  (get-celsius (fahrenheit 32))
  (get-celsius (fahrenheit 212))
]}

@defproc[(get-fahrenheit [m Absolute-Temperature]) Real]{
Takes a @racket[measure] and produces the corrosponding temperature in
fahrenheit, represented as a number.
@examples[
  (require typed/racket)
  (require typed/measures-with-dimensions)
  (get-fahrenheit (make-measure 273.15 kelvin))
  (get-fahrenheit (fahrenheit 32))
  (get-fahrenheit (celsius 0))
  (get-fahrenheit (celsius 100))
]}

