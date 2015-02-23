#lang scribble/manual

@(require scribble/eval
          racket/require
          typed/measures-with-dimensions/untyped-utils
          (for-label (combine-in/priority
                      typed/measures-with-dimensions/chemistry
                      racket
                      typed/racket)))

@title[#:style '(toc)]{Chemistry}

@defmodule[typed/measures-with-dimensions/chemistry]

@local-table-of-contents[]

@include-section["chemistry/elements.scrbl"]

@include-section["chemistry/compounds.scrbl"]

@include-section["chemistry/molar-mass.scrbl"]

