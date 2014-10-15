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

@title{measures-with-dimensions}

@defmodule[(submod typed/measures-with-dimensions untyped)]

source code: @url["https://github.com/AlexKnauth/measures-with-dimensions"]

This module provides structs to represent measures, units, and dimensions,
and functions and macros to manipulate these.

@local-table-of-contents[]

@include-section["operations-types-structs.scrbl"]

@include-section["units-and-dimensions.scrbl"]

@include-section["physical-constants.scrbl"]

