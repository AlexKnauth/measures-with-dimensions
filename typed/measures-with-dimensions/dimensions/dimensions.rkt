#lang sweet-exp typed/racket/base

provide (all-defined-out)

require syntax/parse/define
        "../preds.rkt"
        "dimension-struct.rkt"
        "dimension-operations.rkt"
        "../untyped-utils.rkt"
        for-syntax racket/base
                   syntax/parse
                   racket/syntax
                   racket/match
                   threading
                   racket/contract/base
                   racket/contract/region
                   for-syntax racket/base
                              syntax/parse


(define-simple-macro (def/cast id:id (~literal :) t:expr val:expr)
  (begin
    (: id : t)
    (define id (cast val t))))

(begin-for-syntax
  (define ds-hash (make-hash))
  (define dfs-hash (make-hash))
  (define (define-dimension! sym dat)
    (hash-set! ds-hash sym (parse-dimension dat)))
  (define (define-dimension-function! sym f)
    (hash-set! dfs-hash sym f))
  (define-syntax define-dimension-function
    (syntax-parser
      [(define-dimension-function id:id f:expr)
       #'(begin
           (define id f)
           (define-dimension-function! `id id))]
      [(define-dimension-function (id:id . args) body:expr ...+)
       #'(begin
           (define (id . args) body ...)
           (define-dimension-function! `id id))]))
  (define (parse-dimension dat #:i [i 0])
    (unless (<= i 20)
      (error 'parse-dimension "too much recursion, given: ~v" dat))
    (match dat
      [`(dimension ,(? exact-integer?)
                   ,(? exact-integer?)
                   ,(? exact-integer?)
                   ,(? exact-integer?)
                   ,(? exact-integer?))
       dat]
      [(? symbol? sym)
       (parse-dimension (hash-ref ds-hash sym))]
      [`(,f-sym . ,args)
       (parse-dimension
        (apply (hash-ref dfs-hash f-sym)
               (map parse-dimension args)))]
      [_ dat]
      ))
  (define (id-titlecase id)
    (define Id-str
      (~> id syntax-e symbol->string string-titlecase))
    (format-id id "~a" Id-str #:source id))
  (define (id-downcase id)
    (define id-str
      (~> id syntax-e symbol->string string-downcase))
    (format-id id "~a" id-str #:source id))
  )
       
(define-syntax define-dimension
  (lambda (stx)
    (syntax-parse stx
      [(define-dimension id:id expr:expr)
       #:with parsed-expr (datum->syntax #'expr (parse-dimension (syntax->datum #'expr)) #'expr)
       #:with Id (id-titlecase #'id)
       #'(begin
           (begin-for-syntax
             (define id 'parsed-expr)
             (define-dimension! 'id id))
           (define-type Id parsed-expr)
           (define-type id Id #:omit-define-syntaxes)
           (def/cast id : Id expr))]
      [(define-dimension id:id expr:expr #:omit-define-syntaxes #:omit-define-values)
       #:with parsed-expr (datum->syntax #'expr (parse-dimension (syntax->datum #'expr)) #'expr)
       #'(begin
           (begin-for-syntax
             (define id 'parsed-expr)
             (define-dimension! 'id id)))]
       )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(begin-for-syntax
  (define-dimension-function (M^ n)
    `(dimension ,n 0 0 0 0))
  (define-dimension-function (L^ n)
    `(dimension 0 ,n 0 0 0))
  (define-dimension-function (T^ n)
    `(dimension 0 0 ,n 0 0))
  (define-dimension-function (Q^ n)
    `(dimension 0 0 0 ,n 0))
  (define-dimension-function (Θ^ n)
    `(dimension 0 0 0 0 ,n))
  
  
  (define-dimension-function (dimension-map f . ds)
    (match-define `[(dimension ,mass-expts ,length-expts ,time-expts ,charge-expts ,temp-expts) ...]
      ds)
    `(dimension ,(apply f mass-expts)
                ,(apply f length-expts)
                ,(apply f time-expts)
                ,(apply f charge-expts)
                ,(apply f temp-expts)))
  
  
  
  (define-dimension-function (dexpt d n)
    (define (raise-exponent-error n_0)
      (error 'dexpt (string-append
                     "can't have a dimension with a non-integer exponent" "\n"
                     "  given: ~v" "\n"
                     "  in: (dexpt ~v ~v)" "\n") n_0 d n))
    (match `(dexpt ,(parse-dimension d) ,n)
      [`(dexpt ,_ 0) dimensionless-dimension]
      [`(dexpt (dimension 0 0 0 0 0) ,_) dimensionless-dimension]
      [`(dexpt ,d 1) d]
      [`(dexpt ,d ,n)
       (define (f expt)
         (let ([new-expt (* expt n)])
           (cond [(exact-integer? new-expt) new-expt]
                 [else (raise-exponent-error new-expt)])))
       (dimension-map f d)]))
  
  (define-dimension-function (dsqr d)
    (dexpt d 2))
  
  (define-dimension-function (dsqrt d)
    (dexpt d 1/2))
  
  (define-dimension-function (d* . ds)
    (apply dimension-map + ds))
  
  (define-dimension-function (d1/ d)
    (dimension-map - d))
  (define-dimension-function (d/ d1 d2 . ds)
    (apply dimension-map - d1 d2 ds)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dimensions:

(define-simple-macro
  (define-dimensions [id:id expr:expr] ...)
  (begin (define-dimension id expr) ...))

(define-dimension dimensionless-dimension (dimension 0 0 0 0 0)
  #:omit-define-syntaxes #:omit-define-values)
(define-dimensions
  [1-dimension dimensionless-dimension]
  [mass-dimension (M^ 1)]
  [length-dimension (L^ 1)]
  [time-dimension (T^ 1)]
  [charge-dimension (Q^ 1)]
  [temperature-dimension (Θ^ 1)]
  
  [area-dimension (L^ 2)]
  [volume-dimension (L^ 3)]

  [mass-density-dimension (d/ mass-dimension volume-dimension)]
  [charge-density-dimension (d/ charge-dimension volume-dimension)]
  
  [velocity-dimension (d/ length-dimension time-dimension)]
  [speed-dimension velocity-dimension]
  [acceleration-dimension (d/ velocity-dimension time-dimension)]
  
  [force-dimension (d* mass-dimension acceleration-dimension)]
  [momentum-dimension (d* mass-dimension velocity-dimension)]
  [angular-momentum-dimension (d* length-dimension momentum-dimension)]
  
  [energy-dimension (d* mass-dimension (dexpt velocity-dimension 2))]
  [work-dimension energy-dimension]
  [torque-dimension work-dimension]
  [power-dimension (d/ energy-dimension time-dimension)]
  
  [pressure-dimension (d/ force-dimension area-dimension)]
  
  [entropy-dimension (d/ energy-dimension temperature-dimension)]
  [heat-capacity-dimension entropy-dimension]
  [specific-heat-dimension (d/ heat-capacity-dimension mass-dimension)]
  [molar-specific-heat-dimension heat-capacity-dimension]
  
  [electric-field-dimension (d/ force-dimension charge-dimension)]
  [electric-potential-dimension (d/ energy-dimension charge-dimension)]
  [voltage-dimension electric-potential-dimension]
  [emf-dimension voltage-dimension]
  [capacitance-dimension (d/ charge-dimension voltage-dimension)]
  
  [current-dimension (d/ charge-dimension time-dimension)]
  [current-density-dimension (d/ current-dimension area-dimension)]
  [resistance-dimension (d/ voltage-dimension current-dimension)]
  [resistivity-dimension (d/ electric-field-dimension current-density-dimension)]
  [conductivity-dimension (d1/ resistivity-dimension)]
  
  [magnetic-field-dimension (d/ force-dimension charge-dimension velocity-dimension)]
  
  [electric-flux-dimension (d* electric-field-dimension area-dimension)]
  [magnetic-flux-dimension (d* magnetic-field-dimension area-dimension)]
  
  [inductance-dimension (d/ emf-dimension (d/ current-dimension time-dimension))]
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(untyped-module*
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module* test racket/base
  (require (submod ".." untyped)
           (submod "dimension-struct.rkt" untyped)
           (submod "dimension-operations.rkt" untyped)
           rackunit)
  
  (check-equal? (d* velocity-dimension time-dimension) length-dimension)
  (check-equal? (d* acceleration-dimension time-dimension) velocity-dimension)
  
  (check-equal? (d* mass-dimension acceleration-dimension) force-dimension)
  
  (check-equal? (d* force-dimension length-dimension) energy-dimension)
  (check-equal? (d* mass-dimension (dsqr velocity-dimension)) energy-dimension)
  
  (check-equal? (dsqrt (d* acceleration-dimension length-dimension)) velocity-dimension)
  
  )
