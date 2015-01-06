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

@defmodule[#:multi [typed/measures-with-dimensions
                    (submod typed/measures-with-dimensions untyped)
                    measures-with-dimensions]]

source code: @url["https://github.com/AlexKnauth/measures-with-dimensions"]

This module provides structs to represent measures, units, and dimensions,
and functions and macros to manipulate these.

@local-table-of-contents[]

@include-section["operations-types-structs.scrbl"]

@include-section["units-and-dimensions.scrbl"]

@include-section["physical-constants.scrbl"]

@include-section["temperature.scrbl"]

@include-section["short-names.scrbl"]

