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

@title{Physical Constants}

@deftogether[[
  @defthing[0-measure Zero-Measure]
  @defthing[1-measure (Measureof 1 Dimensionless-Unit)]
  @defthing[g Acceleration]
  @defthing[G Number-Measure]
  @defthing[c Speed]
  @defthing[elementary-charge Charge]
  @defthing[R Number-Measure]
  @defthing[k_B Number-Measure]
]]

