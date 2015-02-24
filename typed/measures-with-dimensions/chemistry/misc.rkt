#lang typed/racket/base

(require racket/match
         (only-in typed/racket/base [U Un])
         "element-struct.rkt"
         "elements.rkt"
         "compound.rkt"
         "molar-mass.rkt"
         "../untyped-utils.rkt"
         )

(: contains-element? : [Element (Un Element Compound) -> Boolean])
(define (contains-element? elt comp)
  (<= 1 (count-element elt comp)))

(: count-element : [Element (Un Element Compound) -> Natural])
(define (count-element elt comp)
  (define (elt? e)
    (equal? e elt))
  (count-element/pred elt? comp))

(: count-element/pred : [(Element -> Any) (Un Element Compound) -> Natural])
(define (count-element/pred elt? comp)
  (match comp
    [(? element? e) (if (elt? e) 1 0)]
    [(make-compound alist)
     (for/sum ([p (in-list alist)]) : Natural
       (match-define (cons sub n) p)
       (* n (count-element/pred elt? sub)))]))

(: element-period : [Element -> Positive-Integer])
(define (element-period e)
  (define n (element-atomic-number e))
  (cond
    [(<= 1 n 2) 1]
    [(<= 3 n 10) 2]
    [(<= 11 n 18) 3]
    [(<= 19 n 36) 4]
    [(<= 37 n 54) 5]
    [(<= 55 n 86) 6]
    [(<= 87 n 118) 7]
    [else (error 'element-period "n: ~v" n)]))

(: element-group : [Element -> Positive-Integer])
(define (element-group e)
  (define n (element-atomic-number e))
  (case n
    [(1  3 11 19 37 55  87) 1]
    [(   4 12 20 38 56  88) 2]
    [(        21 39       ) 3]
    [(        22 40 72 104) 4]
    [(        23 41 73 105) 5]
    [(        24 42 74 106) 6]
    [(        25 43 75 107) 7]
    [(        26 44 76 108) 8]
    [(        27 45 77 109) 9]
    [(        28 46 78 110) 10]
    [(        29 47 79 111) 11]
    [(        30 48 80 112) 12]
    [(   5 13 31 49 81 113) 13]
    [(   6 14 32 50 82 114) 14]
    [(   7 15 33 51 83 115) 15]
    [(   8 16 34 52 84 116) 16]
    [(   9 17 35 53 85 117) 17]
    [(2 10 18 36 54 86 118) 18]
    [else (error 'element-group "n: ~v" n)]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(untyped-module*
 )
