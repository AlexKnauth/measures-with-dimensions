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

@title{Elements}

@deftogether[[
  @defstruct*[element ([atomic-number Positive-Integer] [symbol Symbol] [atomic-mass Molar-Mass])]
  @deftype[Element]]]{
A struct and a type for elements.
}

@(define-simple-macro @defid[id:id]
   @defidentifier[#'id])

@(define-simple-macro
   @(defelements
      [H He]
      [Li Be B ...]
      [Na Mg Al ...]
      [K Ca Sc Ti ...]
      [Rb Sr Y Zr ...]
      [Cs ...]
      [Fr ...])
   #:with (S1 ...)  (make-list (+ 14 10 6) #'@hspace[1])
   #:with (S23 ...) (make-list (+ 14 10) #'@hspace[1])
   #:with (S45 ...) (make-list (+ 14) #'@hspace[1])
   (tabular
    #:sep @hspace[1] #:style 'boxed
    (list (list @defid[H]  S1 ...                                @defid[He])
          (list @defid[Li] @defid[Be] S23 ...                @defid[B]  ...)
          (list @defid[Na] @defid[Mg] S23 ...                @defid[Al] ...)
          (list @defid[K]  @defid[Ca] @defid[Sc] S45 ... @defid[Ti]     ...)
          (list @defid[Rb] @defid[Sr] @defid[Y]  S45 ... @defid[Zr]     ...)
          (list @defid[Cs]                                              ...)
          (list @defid[Fr]                                              ...)))
   )

@bold{The Periodic Table of Elements:}
@defelements[
 [H                                                                                               He]
 [Li Be                                                                         B   C  N   O  F   Ne]
 [Na Mg                                                                         Al  Si P   S  Cl  Ar]
 [K  Ca Sc                                           Ti V  Cr Mn Fe Co Ni Cu Zn Ga  Ge As  Se Br  Kr]
 [Rb Sr Y                                            Zr Nb Mo Tc Ru Rh Pd Ag Cd In  Sn Sb  Te I   Xe]
 [Cs Ba La Ce Pr Nd Pm Sm Eu Gd Tb Dy Ho Er Tm Yb Lu Hf Ta W  Re Os Ir Pt Au Hg Tl  Pb Bi  Po At  Rn]
 [Fr Ra Ac Th Pa U  Np Pu Am Cm Bk Cf Es Fm Md No Lr Rf Db Sg Bh Hs Mt Ds Rg Cn Uut Fl Uup Lv Uus Uuo]
]

@margin-note{
Note: The element @racket[U] conflicts with the type constructor for unions.  To work around this, you
can import it as @racket[Un] using @racket[(require (only-in typed/racket/base [U Un]))].
}

@deftogether[[
  @deftype[Molar-Mass]
  @deftype[Molar-Mass-Unit]]]{
Since @racket[mol]s are dimensionless, these are equivalent to the @racket[Mass]
and @racket[Mass-Unit] types.
}

@defthing[g/mol Molar-Mass-Unit]{
A @racket[gram] per @racket[mol].
}

@deftogether[[
  @defthing[elements/sym (HashTable Symbol Element)]
  @defthing[elements/n (HashTable Positive-Integer Element)]]]{
Two hash tables containing the elements 1-118.
The first maps the element symbols to the elements, and the second maps the
atomic numbers to the elements.
}

@defproc[(->element [e (Un Element Symbol Positive-Integer)]) Element]{
If given an element, returns that element.  If given a symbol or atomic-number,
returns the element with that symbol or atomic-number.
}

