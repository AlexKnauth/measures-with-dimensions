#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          racket/require
          typed/measures-with-dimensions/untyped-utils
          (for-label (combine-in/priority
                      (submod typed/measures-with-dimensions untyped)
                      racket
                      typed/racket))
          "deftype.rkt"
          )

@title{Physical Constants}

@deftogether[[
  @defthing[0-measure (Measureof 0 Dimensionless-Unit)]
  @defthing[1-measure (Measureof 1 Dimensionless-Unit)]
  @defthing[g Acceleration]
  @defthing[G Number-Measure]
  @defthing[c Speed]
  @defthing[elementary-charge Charge]
  @defthing[R Number-Measure]
  @defthing[k_B Number-Measure]
]]

