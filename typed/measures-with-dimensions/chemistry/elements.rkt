#lang typed/racket/base

(require racket/match
         syntax/parse/define
         (only-in typed/racket/base [U Un])
         "../unit-operations.rkt"
         "../units.rkt"
         "../measure-struct.rkt"
         "../measure-types.rkt"
         "element-struct.rkt"
         "../untyped-utils.rkt"
         )

(define-simple-macro
  (def-elements elements/sym elements/n ->element [Sym num mass] ...)
  (begin (provide elements/sym elements/n ->element Sym ...)
         (define Sym : (element 'Sym)
           ((inst element 'Sym) num 'Sym (pos-real->molar-mass mass)))
         ...
         (define elements/sym : (HashTable Symbol Element)
           (make-immutable-hasheq
            (list (cons 'Sym Sym) ...)))
         (define elements/n : (HashTable Positive-Integer Element)
           (make-immutable-hasheq
            (list (cons num Sym) ...)))
         (: ->element : [(Un Element Symbol Positive-Integer) -> Element])
         (define (->element x)
           (cond
             [(element? x) x]
             [(symbol? x) (hash-ref elements/sym x)]
             [(exact-positive-integer? x) (hash-ref elements/n x)]
             ))
         ))

(: pos-real->molar-mass : [Positive-Real -> Mass])
(define (pos-real->molar-mass m)
  (make-measure m g/mol))


(def-elements elements/sym elements/n ->element
  [H  1 1.00794]
  [He 2 4.00260]
  [Li 3 6.941]
  [Be 4 9.01218]
  [B  5 10.811]
  [C  6 12.011]
  [N  7 14.0067]
  [O  8 15.9994]
  [F  9 18.998403]
  [Ne 10 20.1797]
  [Na 11 22.98977]
  [Mg 12 24.305]
  [Al 13 26.98154]
  [Si 14 28.0855]
  [P  15 30.97376]
  [S  16 32.066]
  [Cl 17 35.453]
  [Ar 18 39.948]
  [K  19 39.0983]
  [Ca 20 40.078]
  [Sc 21 44.9559]
  [Ti 22 47.88]
  [V  23 50.9415]
  [Cr 24 51.996]
  [Mn 25 54.9380]
  [Fe 26 55.847]
  [Co 27 58.9332]
  [Ni 28 58.69]
  [Cu 29 63.546]
  [Zn 30 65.38]
  [Ga 31 69.72]
  [Ge 32 72.61]
  [As 33 74.9216]
  [Se 34 78.96]
  [Br 35 79.904]
  [Kr 36 83.80]
  [Rb 37 85.4678]
  [Sr 38 87.62]
  [Y  39 88.9059]
  [Zr 40 91.22]
  [Nb 41 92.9064]
  [Mo 42 95.94]
  [Tc 43 98]
  [Ru 44 101.07]
  [Rh 45 102.9055]
  [Pd 46 106.42]
  [Ag 47 107.8682]
  [Cd 48 112.41]
  [In 49 114.82]
  [Sn 50 118.710]
  [Sb 51 121.75]
  [Te 52 127.60]
  [I  53 126.9045]
  [Xe 54 131.29]
  [Cs 55 132.9054]
  [Ba 56 137.33]
  [La 57 138.9055]
  [Ce 58 140.12]
  [Pr 59 140.12]
  [Nd 60 144.24]
  [Pm 61 145]
  [Sm 62 150.36]
  [Eu 63 151.96]
  [Gd 64 157.25]
  [Tb 65 158.9253]
  [Dy 66 162.50]
  [Ho 67 164.9304]
  [Er 68 167.26]
  [Tm 69 168.9342]
  [Yb 70 173.04]
  [Lu 71 174.967]
  [Hf 72 178.49]
  [Ta 73 180.9479]
  [W  74 183.85]
  [Re 75 186.207]
  [Os 76 190.2]
  [Ir 77 192.22]
  [Pt 78 195.08]
  [Au 79 196.9665]
  [Hg 80 200.59]
  [Tl 81 204.383]
  [Pb 82 207.2]
  [Bi 83 208.9804]
  [Po 84 209]
  [At 85 210]
  [Rn 86 222]
  [Fr 87 223]
  [Ra 88 226]
  [Ac 89 227]
  [Th 90 232.0381]
  [Pa 91 231.0359]
  [U  92 238.0289]
  [Np 93 237]
  [Pu 94 244]
  [Am 95 243]
  [Cm 96 247]
  [Bk 97 247]
  [Cf 98 251]
  [Es 99 252]
  [Fm 100 257]
  [Md 101 258]
  [No 102 259]
  [Lr 103 262]
  [Rf 104 261]
  [Db 105 262]
  [Sg 106 266]
  [Bh 107 264]
  [Hs 108 269]
  [Mt 109 268]
  [Ds 110 271]
  [Rg 111 272]
  [Cn 112 285]
  [Uut 113 286]
  [Fl 114 289]
  [Uup 115 289]
  [Lv 116 293]
  [Uus 117 294]
  [Uuo 118 294]
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module* untyped racket/base
  (require (submod ".."))
  (provide (all-from-out (submod ".."))))
