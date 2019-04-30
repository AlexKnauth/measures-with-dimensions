#lang info

(define collection 'multi)

(define deps '("base"
               "typed-racket-lib"
               "typed-racket-more"
               "threading"  ; TODO: change to threading-lib when the version exceptions are fixed
               "math-lib"
               "htdp-lib"
               "unstable-lib"
               "sweet-exp"
               "reprovide-lang"
               "predicates"
               "colon-match"
               "scribble-lib"))

(define build-deps '("rackunit-lib"
                     "scribble-lib"
                     "sandbox-lib"
                     "racket-doc"
                     "typed-racket-doc"
                     "at-exp-lib"))

