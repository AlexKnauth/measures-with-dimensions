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

@title{Compounds}

@defform[(compound [sub n] ...)
         #:contracts ([sub (Un Element Compound)] [n Natural])]{
A macro that constructs a compound, and a match-expander that deconstructs a compound.

@examples[
  (require racket)
  (require measures-with-dimensions/chemistry)
  (define H2O (compound [H 2] [O 1]))
  (match H2O
    [(compound [a x] [b y])
     (list a x b y)])
]}

@defform[(compound: id)]{
A macro that constructs a compound from an identifier representing its formula.

@examples[
  (require typed/racket)
  (require typed/measures-with-dimensions/chemistry)
  (define H2O (compound: H2O))
]}

@deftogether[[
  @deftype[Compound]
  @defproc[(compound? [v Any]) Boolean]]]{
A type and a predicate for compounds.
}

@deftogether[[
  @defproc[(make-compound [alist (Listof (Pairof (Un Element Compound) Natural))]) Compound]
  @defproc[(compound-alist [compound Compound]) (Listof (Pairof (Un Element Compound) Natural))]]]{
A constructor that creates a compound from an association list, and an accessor
to get a compound's association list.  
}

