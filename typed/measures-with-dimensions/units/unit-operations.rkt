#lang sweet-exp typed/racket/base

provide (all-defined-out)

require syntax/parse/define
        racket/math
        racket/match
        racket/local
        racket/splicing
        (except-in racket/list second)
        "unit-struct.rkt"
        "../dimensions/dimension-struct.rkt"
        "../dimensions/dimension-operations.rkt"
        "../preds.rkt"
        "../untyped-utils.rkt"

require/typed racket/set
              [#:opaque Mutable-Set set-mutable?]
              [mutable-set [-> Mutable-Set]]
              [set-add! [Mutable-Set Unit -> Void]]
              [set-empty? [Mutable-Set -> Boolean]]
              [set-first [Mutable-Set -> Unit]]


(: unit=? (Unitish Unitish * -> Boolean))
(define (unit=? u . rst)
  (let ([u : Unit (->unit u)])
    (let ([u.scalar : Positive-Real (unit-scalar u)]
          [u.dimension : Dimension (unit-dimension u)])
      (for/and: : Boolean ([u2 : Unitish (in-list rst)])
        (let ([u2 : Unit (->unit u2)])
          (and (= u.scalar (unit-scalar u2))
               (dimension=? u.dimension (unit-dimension u2))))))))


(: unit-rename : (All (d) [(Unitof d) Any -> (Unitof d)]))
(define (unit-rename u name)
  (unit name
        (unit-scalar u)
        (unit-dimension u)))

(define-simple-macro (define-unit u:id (~literal :) t:expr val:expr)
  (begin
    (: u : t)
    (define u (cast (unit-rename val 'u) t))))

(: usqr : [Unitish -> Unit])
(define (usqr u)
  (let ([u : Unit (->unit u)])
    (unit-simplify-name
     (cond [(unit=? u 1-unit) 1-unit]
           [else (make-Unit `(usqr ,u)
                            (cast (sqr (Unit-scalar u)) Positive-Real)
                            (dsqr (Unit-dimension u)))]))))

(: usqrt : [Unitish -> Unit])
(define (usqrt u)
  (let ([u : Unit (->unit u)])
    (unit-simplify-name
     (cond [(unit=? u 1-unit) 1-unit]
           [else (make-Unit `(usqrt ,u)
                            (cast (sqrt (Unit-scalar u)) Positive-Real)
                            (dsqrt (Unit-dimension u)))]))))

(: uexpt : [Unitish Exact-Rational -> Unit])
(define (uexpt u n)
  (let ([u : Unit (->unit u)])
    (unit-simplify-name
     (cond [(unit=? u 1-unit) 1-unit]
           [(zero? n) 1-unit]
           [(one? n) u]
           [else (make-Unit `(uexpt ,u ,n)
                            (cast (expt (Unit-scalar u) n) Positive-Real)
                            (dexpt (Unit-dimension u) n))]))))

(: u* : [Unitish * -> Unit])
(define (u* . args)
  (let ([args : (Listof Unit) (filter-not (λ (u) (equal? u 1-unit)) (map ->unit args))])
    (unit-simplify-name
     (cond [(empty? args) 1-unit]
           [(one? (length args)) (first args)]
           [else (make-Unit `(u* ,@args)
                            (cast (apply * (map Unit-scalar args)) Positive-Real)
                            (apply d* (map Unit-dimension args)))]))))

(: u1/ : [Unitish -> Unit])
(define (u1/ u)
  (let ([u : Unit (->unit u)])
    (unit-simplify-name
     (make-Unit `(u1/ ,u)
                (cast (/ (Unit-scalar u)) Positive-Real)
                (d1/ (Unit-dimension u))))))

(: u/ : [Unitish Unitish * -> Unit])
(define u/
  (case-lambda
    [([u : Unitish]) (unit-simplify-name (u1/ u))]
    [([u : Unitish] . [rst : Unitish *])
     (let ([u : Unit (->unit u)]
           [rst : (Listof Unit) (map ->unit rst)])
       (unit-simplify-name
        (make-Unit `(u/ ,u ,@rst)
                   (cast (apply / (Unit-scalar u) (map Unit-scalar rst)) Positive-Real)
                   (apply d/ (Unit-dimension u) (map Unit-dimension rst)))))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: unit-simplify-name : (All (d) [(Unitof d) -> (Unitof d)]))
(define (unit-simplify-name u)
  (match-define (unit name scalar dimension) u)
  (cond [(unit? name) (unit-simplify-name (unit-rename u (unit->name name)))]
        [(symbol? name) u]
        [(find-named-unit u)
         => (lambda (u) u)]
        [else
         (with-handlers ([exn:fail? (λ (e) u)])
           (unit-rename u (simplify-unit-name name)))]))

(splicing-local [(: named-units : (HashTable Dimension (HashTable Positive-Real Mutable-Set)))
                 (define named-units
                   ((inst make-hash Dimension (HashTable Positive-Real Mutable-Set))))]
  (: find-named-unit : (All (d) [(Unitof d) -> (U (Unitof d) #f)]))
  (define (find-named-unit u)
    (define d : d (unit-dimension u))
    (define d-Dim : Dimension (assert d Dimension?))
    (: scalar->units : (U (HashTable Positive-Real Mutable-Set) #f))
    (define scalar->units
      (hash-ref named-units d-Dim #f))
    (cond [(not scalar->units) #f]
          [else
           (: result-units : (U Mutable-Set #f))
           (define result-units (hash-ref scalar->units (unit-scalar u) #f))
           (and result-units
                (not (set-empty? result-units))
                (unit-rename u (unit-name (set-first result-units))))]))
  
  (: add-named-unit! : [Unit -> Void])
  (define (add-named-unit! u)
    (: scalar->units : (HashTable Positive-Real Mutable-Set))
    (define scalar->units
      (hash-ref! named-units (unit-dimension u)
                 (λ () ((inst make-hash Positive-Real Mutable-Set)))))
    (define units
      (hash-ref! scalar->units (unit-scalar u) (λ () (mutable-set))))
    (set-add! units u)))

(define-type (Cons a b) (Pairof a b))

(define-type Op-Sym (U 'usqr 'usqrt 'uexpt 'u* 'u1/ 'u/))
(define-type Name/Op (Cons Op-Sym (Listof Any)))
(define name/op? (make-predicate Name/Op))

(: unit->name : [Any -> Any])
(define (unit->name name)
  (cond [(Unit? name) (define u name)
                      (define u.name (unit-name u))
                      (when (Unit? u.name)
                        (unless (unit=? u u.name)
                          (error 'unit->name "bad: (unit (unit ~v ~v ~v) ~v ~v)"
                                 (unit-name u.name) (unit-scalar u.name) (unit-dimension u.name)
                                 (unit-scalar u) (unit-dimension u))))
                      (unit->name (unit-name name))]
        [else name]))

(: simplify-unit-name : [Any -> Any])
(define (simplify-unit-name name)
  (unit->name
   (cond [(unit? name) (simplify-unit-name (unit->name name))]
         [(symbol? name) name]
         [(name/op? name)
          (with-handlers ([exn:fail? (λ (e) name)])
            (simplify-unit-name/op name))]
         [else name])))

(: simplify-unit-name/op : [Name/Op -> Any])
(define (simplify-unit-name/op name)
  (unit->name (hash->unit-name (unit-name->hash name))))        

(: hash->unit-name : [(HashTable (U Symbol Unit) Exact-Rational) -> Any])
(define (hash->unit-name hash)
  (let* ([hash (for/hash ([(key expt) (in-hash hash)]
                          #:when (not (zero? expt))) : (HashTable (U Symbol Unit) Exact-Rational)
                 (values key expt))]
         [keys : (Listof (U Symbol Unit))
               (hash-keys hash)]
         [keys.length (length keys)])
    (: simplify-unit-name/single-key-expt : [(U Symbol Unit) Exact-Rational -> Any])
    (define (simplify-unit-name/single-key-expt key expt)
      (cond [(= expt 0) '1-unit]
            [(= expt 1) key]
            [(= expt 2) `(usqr ,key)]
            [(= expt 1/2) `(usqrt ,key)]
            [(= expt -1) `(u1/ ,key)]
            [(negative? expt) `(u1/ ,(simplify-unit-name/single-key-expt key (- expt)))]
            [else `(uexpt ,key ,expt)]))
    (unit->name
     (cond [(= keys.length 0) '1-unit]
           [(= keys.length 1) (define key (first keys))
                              (simplify-unit-name/single-key-expt key (hash-ref hash key))]
           [else
            (local [(define pos-expts
                      (for/list ([(key expt) (in-hash hash)]
                                 #:when (not (negative? expt))) : (Listof Any)
                        (simplify-unit-name/single-key-expt key expt)))
                    (define neg-expts
                      (for/list ([(key expt) (in-hash hash)]
                                 #:when (negative? expt)) : (Listof Any)
                        (simplify-unit-name/single-key-expt key (- expt))))
                    (define pos-expts.length (length pos-expts))
                    (define neg-expts.length (length neg-expts))]
              (cond [(= pos-expts.length 0) (cond [(= neg-expts.length 0) '1-unit]
                                                  [(= neg-expts.length 1) `(u1/ ,(first neg-expts))]
                                                  [else `(u1/ (u* ,@neg-expts))])]
                    [(= pos-expts.length 1) (cond [(= neg-expts.length 0) (first pos-expts)]
                                                  [(= neg-expts.length 1)
                                                   `(u* ,(first pos-expts)
                                                        (u1/ ,(first neg-expts)))]
                                                  [else `(u* ,(first pos-expts)
                                                             (u1/ (u* ,@neg-expts)))])]
                    [else (cond [(= neg-expts.length 0) `(u* ,@pos-expts)]
                                [(= neg-expts.length 1) `(u* (u* ,@pos-expts)
                                                             (u1/ ,(first neg-expts)))]
                                [else `(u* (u* ,@pos-expts)
                                           (u1/ (u* ,@neg-expts)))])]))]))))

(: unit-name->hash : [Any -> (HashTable (U Symbol Unit) Exact-Rational)])
(define (unit-name->hash name)
  (: hash : [(U Symbol Unit) Integer -> (HashTable (U Symbol Unit) Exact-Rational)])
  (define (hash a b)
    (make-immutable-hash (list (cons a b))))
  (cond [(unit? name) (: sub-unit-name->hash :
                         [Any -> (U (HashTable (U Symbol Unit) Exact-Rational) #f)])
                      (define (sub-unit-name->hash name)
                        (cond [(symbol? name) #f]
                              [(unit? name) (sub-unit-name->hash (unit-name name))]
                              [(name/op? name) (unit-name->hash/op name)]
                              [else #f]))
                      (cond [(sub-unit-name->hash name)
                             => (lambda (hsh) hsh)]
                            [else (hash (assert name Unit?) 1)])]
        [(symbol? name) (hash name 1)]
        [(name/op? name)
         (unit-name->hash/op name)]
        [else (error 'simplify-unit-name "bad unit name: ~v" name)]))

(: unit-name->hash/op : [Name/Op -> (HashTable (U Symbol Unit) Exact-Rational)])
(define (unit-name->hash/op name)
  (define sym (first name))
  (define args (rest name))
  (define (bad-unit-name)
    (error 'unit-simplify-name "bad unit name: ~v" name))
  (case sym
    [(usqr) (unless (= (length args) 1) (bad-unit-name))
            (for/hash ([(sym/u expt) (in-hash (unit-name->hash (first args)))])
              : (HashTable (U Symbol Unit) Exact-Rational)
              (values sym/u (* 2 expt)))]
    [(usqrt) (unless (= (length args) 1) (bad-unit-name))
             (for/hash ([(sym/u expt) (in-hash (unit-name->hash (first args)))])
               : (HashTable (U Symbol Unit) Exact-Rational)
               (values sym/u (* 1/2 expt)))]
    [(uexpt) (unless (= (length args) 2) (bad-unit-name))
             (let* ([base (first args)] [n (first (rest args))])
               (unless (exact-rational? n) (bad-unit-name))
               (for/hash ([(sym/u expt) (in-hash (unit-name->hash base))])
                 : (HashTable (U Symbol Unit) Exact-Rational)
                 (values sym/u (* n expt))))]
    [(u*) (let* ([arg-hashes (map unit-name->hash args)])
            (: keys : (Listof (U Symbol Unit)))
            (define keys
              (remove-duplicates
               (apply append (map (inst hash-keys (U Symbol Unit) Exact-Rational)
                                  arg-hashes))))
            (for/hash ([key (in-list keys)]) : (HashTable (U Symbol Unit) Exact-Rational)
              (values key
                      (for/sum ([arg-hash (in-list arg-hashes)]) : Exact-Rational
                        (hash-ref arg-hash key (λ () 0))))))]
    [(u1/) (unless (= (length args) 1) (bad-unit-name))
           (for/hash ([(sym/u expt) (in-hash (unit-name->hash (first args)))])
             : (HashTable (U Symbol Unit) Exact-Rational)
             (values sym/u (- expt)))]
    [(u/) (define n (length args))
          (cond [(= n 0) (bad-unit-name)]
                [(= n 1) (unit-name->hash/op `(u1/ ,(first args)))]
                [else (unit-name->hash/op `(u* ,(first args) (u1/ (u* ,@(rest args)))))])]
    [else (bad-unit-name)]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(untyped-module*
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

module* test racket/base
  require (submod "../dimensions/dimension-struct.rkt" untyped)
          (submod "unit-struct.rkt" untyped)
          (submod ".." untyped)
          rackunit
          racket/match
  (check-equal? (simplify-unit-name '1-unit) '1-unit)
  (let ([sym (gensym)])
    (check-equal? (simplify-unit-name sym) sym)
    (check-equal? (simplify-unit-name (unit sym 2 dimensionless-dimension)) sym))
  (check-equal? (simplify-unit-name '(u/ a a)) '1-unit)
  (check-equal? (simplify-unit-name '(u/ a (uexpt a 2))) '(u1/ a))
  (check-equal? (simplify-unit-name '(uexpt (u/ a (uexpt b 6) (usqr a) (u1/ a)) 1/3)) '(u1/ (usqr b)))
  (check-equal? (simplify-unit-name '(u/ (usqrt a) a)) '(u1/ (usqrt a)))
  (let* ([a (unit 'a 1 dimensionless-dimension)]
         [b (unit 'b 1 dimensionless-dimension)]
         [ab (u* a b)]
         [1/a (u1/ a)]
         [1/b (u1/ b)])
    (check-match (unit-name ab) (or `(u* ,(== a) ,(== b))
                                    `(u* ,(== b) ,(== a))))
    (check-equal? (unit-name 1/a) `(u1/ ,a))
    (check-equal? (unit-name 1/b) `(u1/ ,b))
    (check-equal? (unit-name (u/ a a)) '1-unit)
    (check-equal? (unit-name (u/ a b)) `(u* ,a (u1/ ,b)))
    (check-equal? (unit-name (u* a b (u1/ a))) 'b)
    (check-equal? (unit-name (u/ ab a)) 'b)
    (define a2 (unit 'a 2 dimensionless-dimension))
    (check-equal? (unit-name (u/ a a2)) `(u* ,a (u1/ ,a2))))

