#lang typed/racket

(provide (all-defined-out))

(require/typed/provide racket/vector
                       [vector-map (All (a b) ((a * -> b) (Vectorof a) * -> (Vectorof b)))])

(: v=? : [(Vectorof Real) (Vectorof Real) * -> Boolean])
(define (v=? v . rst)
  (let: ([v.length : Natural (vector-length v)]
         [rst.lengths : (Listof Natural) (map vector-length rst)])
    (let: ([max-length : Natural (apply max v.length rst.lengths)])
      (for/and: : Boolean ([i : Natural (in-range max-length)])
        (let: ([v-i : Real (vector-ref_0 v i)])
           (for/and: : Boolean ([v2 : (Vectorof Real) (in-list rst)])
             (= v-i (vector-ref_0 v2 i))))))))

(: v* : [Real (Vectorof Real) -> (Vectorof Real)])
(define (v* n v)
  (vector->immutable-vector
   (for/vector : (Vectorof Real) #:length (vector-length v) #:fill 0
     ([v-i : Real (in-vector v)])
     (* n v-i))))


(: vector-ref_0 : [(Vectorof Real) Natural -> Real])
(define (vector-ref_0 v i)
  (cond [(<= (vector-length v) i) 0]
        [else (vector-ref v i)]))

(: vector-x : [(Vectorof Real) -> Real])
(define (vector-x v) (vector-ref_0 v 0))

(: vector-y : [(Vectorof Real) -> Real])
(define (vector-y v) (vector-ref_0 v 1))

(: vector-z : [(Vectorof Real) -> Real])
(define (vector-z v) (vector-ref_0 v 2))


(: vsqr : [(Vectorof Real) -> Nonnegative-Real])
(define (vsqr v)
  (for/sum: : Nonnegative-Real ([v-i : Real (in-vector v)])
    (sqr v-i)))