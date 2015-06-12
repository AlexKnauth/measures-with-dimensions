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

@title[#:style '(toc)]{measures-with-dimensions}

@defmodule[#:multi [measures-with-dimensions typed/measures-with-dimensions]]

source code: @url["https://github.com/AlexKnauth/measures-with-dimensions"]

This module provides structs to represent measures, units, and dimensions,
and functions and macros to manipulate these.

It was originally inspired by @url["https://github.com/Metaxal/measures"], but
is more based on the concept of dimensions, and it also has types for
@racketmodname[typed/racket].

@local-table-of-contents[]

@include-section["operations-types-structs.scrbl"]

@include-section["units-and-dimensions.scrbl"]

@include-section["physical-constants.scrbl"]

@include-section["temperature.scrbl"]

@include-section["short-names.scrbl"]

@include-section["chemistry.scrbl"]

