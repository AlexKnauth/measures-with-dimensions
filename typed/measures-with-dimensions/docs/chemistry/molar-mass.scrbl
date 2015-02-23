#lang scribble/manual

@(require scribble/eval
          racket/require
          typed/measures-with-dimensions/untyped-utils
          syntax/parse/define
          "../deftype.rkt"
          (for-label (combine-in/priority
                      typed/measures-with-dimensions/chemistry
                      typed/measures-with-dimensions
                      racket
                      typed/racket)
                     (only-in typed/racket/base [U Un]))
          (for-syntax racket/base
                      syntax/parse
                      racket/list
                      ))

@title{Molar Mass}

@defproc[(molar-mass [comp (Un Element Compound)]) Molar-Mass]{
finds the molar mass of the element or compound.

@examples[
  (require typed/racket)
  (require typed/measures-with-dimensions/chemistry)
  (molar-mass H)
  (molar-mass O)
  (molar-mass (compound: H2O))
]}
