#lang typed/racket

(provide typed:m*)

(require "untyped-operations-2.rkt"
         "measure-struct.rkt"
         "untyped-utils.rkt")
(require/typed "untyped-operations-2.rkt"
               [(untyped:m* typed:m*)
                (case-> [(U Number-Measureish (Vectorof Real) Measure) * -> Measure])]
               )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(untyped-module*
 )


