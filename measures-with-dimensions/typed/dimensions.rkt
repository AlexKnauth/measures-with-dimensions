#lang typed/racket 

(provide (all-defined-out))

(require syntax-parse-with-pattern-expanders/syntax/parse/define
         (for-syntax racket/base
                     syntax-parse-with-pattern-expanders
                     seq-no-order
                     racket/syntax
                     racket/match
                     rackjure/threading))

(define-simple-macro (def/cast id:id (~literal :) t:expr val:expr)
  (begin
    (: id : t)
    (define id (cast val t))))

(define-simple-macro (inst-Int*5 f:expr)
  (inst f Integer Integer Integer Integer Integer))

(define-simple-macro (define-predicates [pred:id type:expr] ...)
  (begin (define-predicate pred type) ...))

(define zero? (make-predicate Zero))
(define one? (make-predicate One))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type Dimension
  (pdimension Integer Integer Integer Integer Integer))

(: make-pdimension : (All (Mass-Expt Length-Expt Time-Expt Charge-Expt Temp-Expt)
                          (-> #:mass^ Mass-Expt
                              #:length^ Length-Expt
                              #:time^ Time-Expt
                              #:charge^ Charge-Expt
                              #:temperature^ Temp-Expt
                              (pdimension Mass-Expt Length-Expt Time-Expt Charge-Expt Temp-Expt)
                              )))
(define (make-pdimension #:mass^ mass-expt
                         #:length^ length-expt
                         #:time^ time-expt
                         #:charge^ charge-expt
                         #:temperature^ temp-expt)
  (pdimension mass-expt length-expt time-expt charge-expt temp-expt))
(define (make-dimension #:mass^ [mass-expt : Integer 0]
                        #:length^ [length-expt : Integer 0]
                        #:time^ [time-expt : Integer 0]
                        #:charge^ [charge-expt : Integer 0]
                        #:temperature^ [temp-expt : Integer 0]) : Dimension
  (pdimension mass-expt length-expt time-expt charge-expt temp-expt))


(define-type Dimensionless-Dimension (pdimension 0 0 0 0 0))
(define-type (Mass^ n)        (pdimension n 0 0 0 0))
(define-type (Length^ n)      (pdimension 0 n 0 0 0))
(define-type (Time^ n)        (pdimension 0 0 n 0 0))
(define-type (Charge^ n)      (pdimension 0 0 0 n 0))
(define-type (Temperature^ n) (pdimension 0 0 0 0 n))
(define-type Mass-Dimension   (Mass^ 1))
(define-type Length-Dimension (Length^ 1))
(define-type Time-Dimension   (Time^ 1))
(define-type Charge-Dimension (Charge^ 1))
(define-type Temperature-Dimension (Temperature^ 1))
(struct: (Mass-Expt Length-Expt Time-Expt Charge-Expt Temp-Expt)
  pdimension
  ([mass-expt   : Mass-Expt]
   [length-expt : Length-Expt]
   [time-expt   : Time-Expt]
   [charge-expt : Charge-Expt]
   [temperature-expt : Temp-Expt])
  #:transparent
  #:guard (lambda (mass-expt length-expt time-expt charge-expt temp-expt _)
            (define (check val sym)
              (unless (exact-integer? val)
                (error 'pdimension "expected exact-integer? for ~a argument, given ~v" sym val))
              val)
            (values (check mass-expt 'mass-expt)
                    (check length-expt 'length-expt)
                    (check time-expt 'time-expt)
                    (check charge-expt 'charge-expt)
                    (check temp-expt 'temperature-expt)))
  #:property prop:custom-write (lambda (d out mode)
                                 (pdimension-write-proc d out mode)))

(define dimensionless-dimension? (make-predicate Dimensionless-Dimension))
(define dimension? (make-predicate Dimension))

(define dimension-mass-expt   (inst-Int*5 pdimension-mass-expt))
(define dimension-length-expt (inst-Int*5 pdimension-length-expt))
(define dimension-time-expt   (inst-Int*5 pdimension-time-expt))
(define dimension-charge-expt (inst-Int*5 pdimension-charge-expt))
(define dimension-temperature-expt (inst-Int*5 pdimension-temperature-expt))


(: mass^   : (All (n) [n -> (Mass^ n)]))
(: length^ : (All (n) [n -> (Length^ n)]))
(: time^   : (All (n) [n -> (Time^ n)]))
(: charge^ : (All (n) [n -> (Charge^ n)]))
(: temperature^ : (All (n) [n -> (Temperature^ n)]))
(define (mass^ n)
  (pdimension n 0 0 0 0))
(define (length^ n)
  (pdimension 0 n 0 0 0))
(define (time^ n)
  (pdimension 0 0 n 0 0))
(define (charge^ n)
  (pdimension 0 0 0 n 0))
(define (temperature^ n)
  (pdimension 0 0 0 0 n))

(: dimension->list : (All (Mass-Expt Length-Expt Time-Expt Charge-Expt Temp-Expt)
                          [(pdimension Mass-Expt Length-Expt Time-Expt Charge-Expt Temp-Expt)
                           -> (List Mass-Expt Length-Expt Time-Expt Charge-Expt Temp-Expt)]))
(define (dimension->list d)
  (list (pdimension-mass-expt d)
        (pdimension-length-expt d)
        (pdimension-time-expt d)
        (pdimension-charge-expt d)
        (pdimension-temperature-expt d)))

(: dimension-map : (All (n ...) [(Integer ... n -> Integer) Dimensionish ... n -> Dimension]))
(define (dimension-map f . ds)
  (let ([ds (map ->dimension ds)])
    (make-dimension #:mass^        (apply f (map dimension-mass-expt   ds))
                    #:length^      (apply f (map dimension-length-expt ds))
                    #:time^        (apply f (map dimension-time-expt   ds))
                    #:charge^      (apply f (map dimension-charge-expt ds))
                    #:temperature^ (apply f (map dimension-temperature-expt ds)))))



(begin-for-syntax
  (define-syntax-class TYPE
    [pattern TYPE-DIMENSION:id
             #:do [(define str (~> #'TYPE-DIMENSION syntax-e symbol->string string-downcase))
                   (define str.length (string-length str))
                   (define -dimension "-dimension")
                   (define -dimension.length (string-length -dimension))]
             #:when (string=? (substring str (max 0 (min (- str.length -dimension.length)
                                                         str.length)))
                              -dimension)
             #:with TYPE:TYPE (format-id #'TYPE-DIMENSION "~a"
                                         (substring str 0 (- str.length -dimension.length))
                                         #:source #'TYPE-DIMENSION)
             #:with type #'TYPE.type
             #:with Type-Dimension #'TYPE.Type-Dimension
             #:with type-dimension #'TYPE.type-dimension
             #:with Type-Dimensionish #'TYPE.Type-Dimensionish
             #:with type-dimensionish? #'TYPE.type-dimensionish?
             #:with type-str #'TYPE.type-str
             #:with type-dimension-str #'TYPE.type-dimension-str]
    [pattern TYPE:id
             #:with Type:id (id-titlecase #'TYPE)
             #:with type:id (id-downcase #'TYPE)
             #:with Type-Dimension:id (format-id #'Type "~a-Dimension" #'Type #:source #'Type)
             #:with type-dimension:id (format-id #'type "~a-dimension" #'type #:source #'type)
             #:with Type-Dimensionish:id (format-id #'Type-Dimension "~aish" #'Type-Dimension
                                                    #:source #'Type-Dimension)
             #:with type-dimensionish?:id (format-id #'type-dimension "~aish?" #'type-dimension
                                                     #:source #'type-dimension)
             #:with type-str:str (~> #'type syntax-e symbol->string)
             #:with type-dimension-str:str (~> #'type-dimension syntax-e symbol->string)])
  (define-syntax-class dimension-clause
    [pattern [TYPE:TYPE type-dimensionish-extra-expr:expr]
             #:with type #'TYPE.type
             #:with Type-Dimension #'TYPE.Type-Dimension
             #:with type-dimension #'TYPE.type-dimension
             #:with Type-Dimensionish #'TYPE.Type-Dimensionish
             #:with type-dimensionish? #'TYPE.type-dimensionish?
             #:with type-str #'TYPE.type-str
             #:with type-dimension-str #'TYPE.type-dimension-str
             #:with type-dimensionish-expr #'(U Type-Dimension
                                                'type 'type-dimension
                                                type-str type-dimension-str
                                                type-dimensionish-extra-expr)]
    [pattern TYPE:TYPE
             #:with type #'TYPE.type
             #:with Type-Dimension #'TYPE.Type-Dimension
             #:with type-dimension #'TYPE.type-dimension
             #:with Type-Dimensionish #'TYPE.Type-Dimensionish
             #:with type-dimensionish? #'TYPE.type-dimensionish?
             #:with type-str #'TYPE.type-str
             #:with type-dimension-str #'TYPE.type-dimension-str
             #:with type-dimensionish-expr #'(U Type-Dimension
                                                'type 'type-dimension
                                                type-str type-dimension-str)])
  (define (id-titlecase id)
    (define Id-str
      (~> id syntax-e symbol->string string-titlecase))
    (format-id id "~a" Id-str #:source id))
  (define (id-downcase id)
    (define id-str
      (~> id syntax-e symbol->string string-downcase))
    (format-id id "~a" id-str #:source id)))

(define-syntax define-dimensionish
  (syntax-parser
    [(define-dimensionish Dimensionish:id ->dimension:id
       clause:dimension-clause
       ...)
     #:with dimensionish? (format-id #'Dimensionish "~a?" (id-downcase #'Dimensionish)
                                     #:source #'Dimensionish)
     #:with (Type-Dimension ...) #'(clause.Type-Dimension ...)
     #:with (type-dimension ...) #'(clause.type-dimension ...)
     #:with (Type-Dimensionish ...) #'(clause.Type-Dimensionish ...)
     #:with (type-dimensionish? ...) #'(clause.type-dimensionish? ...)
     #:with (type-dimensionish-expr ...) #'(clause.type-dimensionish-expr ...)
     #'(begin
         (define-type Dimensionish
           (U Dimension
              Type-Dimensionish ...))
         (define-type Type-Dimensionish type-dimensionish-expr)
         ...
         (define-predicates
           [dimensionish? Dimensionish]
           [type-dimensionish? Type-Dimensionish] ...)
         (: ->dimension : (All (a b c d e)
                               (case->
                                [(pdimension a b c d e) -> (pdimension a b c d e)]
                                [Type-Dimensionish -> Type-Dimension] ...
                                [Dimensionish -> Dimension])))
         (define (->dimension d)
           (cond [(pdimension? d) d]
                 [(type-dimensionish? d) (ann type-dimension Type-Dimension)] ...)))]
    ))


(define-dimensionish Dimensionish ->dimension
  [dimensionless 1]
  mass
  length
  time
  charge
  [temperature (U 'temperature-dimension 'temperature 'temp-dimension 'temp)]
  area
  volume
  mass-density
  charge-density
  velocity
  ;  acceleration
  ;  force
  ;  momentum
  ;  energy
  ;  work
  ;  power
  ;  pressure
  ;  entropy
  ;  electric-Field
  ;  electric-Potential
  ;  voltage
  ;  emf
  ;  capacitance
  ;  current
  ;  current-Density
  ;  resistance
  ;  resistivity
  ;  conductivity
  ;  magnetic-field
  ;  electric-flux
  ;  magnetic-flux
  ;  inductance
  )

(define d ->dimension)



(: dimension=? : [Dimensionish Dimensionish * -> Boolean])
(define (dimension=? d . rst)
  (let: ([d : Dimension (->dimension d)])
    (for/and: : Boolean ([d2 : Dimensionish (in-list rst)])
      (equal? d (->dimension d2)))))

(define d=? dimension=?)



(: dexpt : [Dimensionish Exact-Rational -> Dimension])
(define (dexpt d n)
  (define (raise-exponent-error n_0)
    (error 'dexpt (string-append
                   "can't have a dimension with a non-integer exponent" "\n"
                   "  given: ~v" "\n"
                   "  in: (dexpt ~v ~v)" "\n") n_0 d n))
  (let ([d : Dimension (->dimension d)])
    (cond [(zero? n) dimensionless-dimension]
          [(dimensionless-dimension? d) dimensionless-dimension]
          [(one? n) d]
          [(pdimension? d) (: f : [Integer -> Integer])
                           (define (f expt)
                             (let ([new-expt : Exact-Rational (* expt n)])
                               (cond [(exact-integer? new-expt) new-expt]
                                     [else (raise-exponent-error new-expt)])))
                           (dimension-map f d)]
          )))

(: dsqr : [Dimensionish -> Dimension])
(define (dsqr d)
  (dexpt d 2))

(: dsqrt : [Dimensionish -> Dimension])
(define (dsqrt d)
  (dexpt d 1/2))

(: d* : [Dimensionish * -> Dimension])
(define d*
  (case-lambda
    [() dimensionless-dimension]
    [(d) (->dimension d)]
    [ds (apply dimension-map + ds)]
    ))

(: d/ : [Dimensionish Dimensionish * -> Dimension])
(define d/
  (case-lambda
    [([d : Dimensionish]) (dimension-map - d)]
    [([d : Dimensionish] . [rst : Dimensionish *]) 
     (apply dimension-map - d rst)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dimension Types:

(define-type Area-Dimension (Length^ 2))
(define-type Volume-Dimension (Length^ 3))

(define-type Mass-Density-Dimension   (pdimension 1 -3 0 0 0))
(define-type Charge-Density-Dimension (pdimension 0 -3 0 1 0))

(define-type Velocity-Dimension     (pdimension 0 1 -1 0 0))
(define-type Speed-Dimension        Velocity-Dimension)
(define-type Acceleration-Dimension (pdimension 0 1 -2 0 0))
(define-type Force-Dimension        (pdimension 1 1 -2 0 0))
(define-type Momentum-Dimension     (pdimension 1 1 -1 0 0))

(define-type Energy-Dimension (pdimension 1 2 -2 0 0))
(define-type Work-Dimension Energy-Dimension)
(define-type Power-Dimension (pdimension 1 2 -3 0 0))

(define-type Pressure-Dimension (pdimension 1 -1 -2 0 0))

(define-type Entropy-Dimension (pdimension 1 2 -2 0 -1))

(define-type Electric-Field-Dimension     (pdimension 1 1 -2 -1 0))
(define-type Electric-Potential-Dimension (pdimension 1 2 -2 -1 0))
(define-type Voltage-Dimension Electric-Potential-Dimension)
(define-type Emf-Dimension Voltage-Dimension)
(define-type Capacitance-Dimension (pdimension -1 -2 2 2 0))

(define-type Current-Dimension         (pdimension  0  0 -1  1 0))
(define-type Current-Density-Dimension (pdimension  0 -2 -1  1 0))
(define-type Resistance-Dimension      (pdimension  1  2 -1 -2 0))
(define-type Resistivity-Dimension     (pdimension  1  3 -1 -2 0))
(define-type Conductivity-Dimension    (pdimension -1 -3  1  2 0))

(define-type Magnetic-Field-Dimension (pdimension 1 0 -1 -1 0))

(define-type Electric-Flux-Dimension (pdimension 1 3 -2 -1 0))
(define-type Magnetic-Flux-Dimension (pdimension 1 2 -1 -1 0))

(define-type Inductance-Dimension (pdimension 1 2 0 -2 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dimensions

(define: dimensionless-dimension : Dimensionless-Dimension (pdimension 0 0 0 0 0))
(define: mass-dimension : Mass-Dimension (mass^ 1))
(define: length-dimension : Length-Dimension (length^ 1))
(define: time-dimension : Time-Dimension (time^ 1))
(define: charge-dimension : Charge-Dimension (charge^ 1))
(define: temperature-dimension : Temperature-Dimension (temperature^ 1))

(define: area-dimension : Area-Dimension ((inst length^ 2) 2))
(define: volume-dimension : Volume-Dimension ((inst length^ 3) 3))

(def/cast mass-density-dimension : Mass-Density-Dimension (d/ mass-dimension volume-dimension))
(def/cast charge-density-dimension : Charge-Density-Dimension
  (d/ charge-dimension volume-dimension))

(def/cast velocity-dimension : Velocity-Dimension (d/ length-dimension time-dimension))
(def/cast acceleration-dimension : Acceleration-Dimension (d/ velocity-dimension time-dimension))
(def/cast force-dimension : Force-Dimension (d* mass-dimension acceleration-dimension))
(def/cast momentum-dimension : Momentum-Dimension (d* mass-dimension velocity-dimension))

(def/cast energy-dimension : Energy-Dimension (d* mass-dimension (dexpt velocity-dimension 2)))
(define: work-dimension : Work-Dimension energy-dimension)
(def/cast power-dimension : Power-Dimension (d/ energy-dimension time-dimension))

(def/cast pressure-dimension : Pressure-Dimension (d/ force-dimension area-dimension))

(def/cast entropy-dimension : Entropy-Dimension (d/ energy-dimension temperature-dimension))

(def/cast electric-field-dimension : Electric-Field-Dimension
  (d/ force-dimension charge-dimension))
(def/cast electric-potential-dimension : Electric-Potential-Dimension
  (d/ energy-dimension charge-dimension))
(define: voltage-dimension : Voltage-Dimension electric-potential-dimension)
(define: emf-dimension : Emf-Dimension voltage-dimension)
(def/cast capacitance-dimension : Capacitance-Dimension (d/ charge-dimension voltage-dimension))

(def/cast current-dimension : Current-Dimension (d/ charge-dimension time-dimension))
(def/cast current-density-dimension : Current-Density-Dimension
  (d/ current-dimension area-dimension))
(def/cast resistance-dimension : Resistance-Dimension (d/ voltage-dimension current-dimension))
(def/cast resistivity-dimension : Resistivity-Dimension
  (d/ electric-field-dimension current-density-dimension))
(def/cast conductivity-dimension : Conductivity-Dimension (dexpt resistivity-dimension -1))

(def/cast magnetic-field-dimension : Magnetic-Field-Dimension
  (d/ force-dimension charge-dimension velocity-dimension))

(def/cast electric-flux-dimension : Electric-Flux-Dimension
  (d* electric-field-dimension area-dimension))
(def/cast magnetic-flux-dimension : Magnetic-Flux-Dimension
  (d* magnetic-field-dimension area-dimension))

(def/cast inductance-dimension : Inductance-Dimension
  (d/ emf-dimension (d/ current-dimension time-dimension)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: pdimension-write-proc : [Dimension Output-Port (U 0 1 #t #f) -> Void])
(define (pdimension-write-proc d out mode)
  (: write-s : [String -> Void])
  (define (write-s s)
    (void (write-string s out)))
  (: write-sub : ([String Integer] [(U 0 1 #t #f)] . ->* . Void))
  (define (write-sub str expt [mode mode])
    (cond [(zero? expt) (display 1 out)]
          [(one? expt)  (write-s str)]
          [(zero? mode) (write-char #\( out)
                        (write-s str)
                        (write-s "^ ")
                        (display expt out)
                        (write-char #\) out)]
          [else (write-s str)
                (write-char #\^ out)
                (display expt out)]))
  (: display-sub : [String Integer -> Void])
  (define (display-sub str expt)
    (write-sub str expt #f))
  (let ([mass-expt   : Integer (pdimension-mass-expt d)]
        [length-expt : Integer (pdimension-length-expt d)]
        [time-expt   : Integer (pdimension-time-expt d)]
        [charge-expt : Integer (pdimension-charge-expt d)]
        [temp-expt   : Integer (pdimension-temperature-expt d)])
    (cond [(= 0 mass-expt length-expt time-expt charge-expt temp-expt) (write-s "(d 1)")]
          [(= 0 length-expt time-expt charge-expt temp-expt) (write-sub "mass" mass-expt)]
          [(= 0 mass-expt time-expt charge-expt temp-expt)   (write-sub "length" length-expt)]
          [(= 0 mass-expt length-expt charge-expt temp-expt) (write-sub "time" time-expt)]
          [(= 0 mass-expt length-expt time-expt temp-expt)   (write-sub "charge" charge-expt)]
          [(= 0 mass-expt length-expt time-expt charge-expt) (write-sub "temperature" temp-expt)]
          [else
           (: maybe-write-sub : [String Integer -> Void])
           (define (maybe-write-sub str expt)
             (cond [(zero? expt) (void)]
                   [else (write-char #\space out)
                         (display-sub str expt)]))
           (write-s "(d*")
           (maybe-write-sub "mass" mass-expt)
           (maybe-write-sub "length" length-expt)
           (maybe-write-sub "time" time-expt)
           (maybe-write-sub "charge" charge-expt)
           (maybe-write-sub "temperature" temp-expt)
           (write-char #\) out)]
          )))
