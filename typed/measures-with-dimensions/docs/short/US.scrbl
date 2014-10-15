#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          racket/require
          typed/measures-with-dimensions/untyped-utils
          (for-label (combine-in/priority
                      typed/measures-with-dimensions/short/US
                      (submod typed/measures-with-dimensions untyped)
                      racket
                      typed/racket))
          "../deftype.rkt"
          )

@title{US}

@defmodule[typed/measures-with-dimensions/short/US]

@deftogether[[
  @defthing[lb-m Mass-Unit]
  @defthing[oz Mass-Unit]
  @defthing[ft Length-Unit]
  @defthing[in Length-Unit]
  @defthing[yd Length-Unit]
  @defthing[mi Length-Unit]
  @defthing[ft^2 Area-Unit]
  @defthing[in^2 Area-Unit]
  @defthing[mi^2 Area-Unit]
  @defthing[ft^3 Volume-Unit]
  @defthing[in^3 Volume-Unit]
  @defthing[gal Volume-Unit]
  @defthing[qt Volume-Unit]
  @defthing[pt Volume-Unit]
  @defthing[cp Volume-Unit]
  @defthing[floz Volume-Unit]
  @defthing[tbs Volume-Unit]
  @defthing[tsp Volume-Unit]
  @defthing[mph Velocity-Unit]
  @defthing[fps Velocity-Unit]
  @defthing[lb-f Force-Unit]
  @defthing[psi Pressure-Unit]
]]
