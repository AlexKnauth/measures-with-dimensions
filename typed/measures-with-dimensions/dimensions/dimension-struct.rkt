#lang typed/racket/base

(provide (struct-out dimension)
         Dimension
         Dimension?
         Dimension-M-expt
         Dimension-L-expt
         Dimension-T-expt
         Dimension-Q-expt
         Dimension-Θ-expt
         make-dimension
         make-Dimension
         Dimensionless-Dimension
         dimensionless-dimension
         )

(require syntax/parse/define
         "../preds.rkt"
         "../untyped-utils.rkt"
         (for-syntax racket/base
                     syntax/parse
                     racket/struct-info
                     racket/local))

(define-simple-macro (inst-Int*5 f:expr)
  (inst f Integer Integer Integer Integer Integer))



(struct: (M-Expt L-Expt T-Expt Q-Expt Θ-Expt)
  dimension
  ([M-expt : M-Expt] [L-expt : L-Expt] [T-expt : T-Expt] [Q-expt : Q-Expt] [Θ-expt : Θ-Expt])
  #:transparent
  #:guard (lambda (M-expt L-expt T-expt Q-expt Θ-expt _)
            (define (check val sym)
              (unless (exact-integer? val)
                (error 'pdimension "expected exact-integer? for ~a argument, given ~v" sym val))
              val)
            (values (check M-expt 'M-expt)
                    (check L-expt 'L-expt)
                    (check T-expt 'T-expt)
                    (check Q-expt 'Q-expt)
                    (check Θ-expt 'Θ-expt)))
  #:property prop:custom-write (lambda (d out mode)
                                 (dimension-write-proc d out mode)))



(define-type Dimension
  (dimension Integer Integer Integer Integer Integer))

(define Dimension? (make-predicate Dimension))
(define Dimension-M-expt (inst-Int*5 dimension-M-expt))
(define Dimension-L-expt (inst-Int*5 dimension-L-expt))
(define Dimension-T-expt (inst-Int*5 dimension-T-expt))
(define Dimension-Q-expt (inst-Int*5 dimension-Q-expt))
(define Dimension-Θ-expt (inst-Int*5 dimension-Θ-expt))

(: make-dimension : (All (M-Expt L-Expt T-Expt Q-Expt Θ-Expt)
                         (-> #:M^ M-Expt #:L^ L-Expt #:T^ T-Expt #:Q^ Q-Expt #:Θ^ Θ-Expt
                             (dimension M-Expt L-Expt T-Expt Q-Expt Θ-Expt)
                             )))
(define (make-dimension #:M^ M-expt #:L^ L-expt #:T^ T-expt #:Q^ Q-expt #:Θ^ Θ-expt)
  (dimension M-expt L-expt T-expt Q-expt Θ-expt))

(define (make-Dimension #:M^ [M-expt : Integer 0]
                        #:L^ [L-expt : Integer 0]
                        #:T^ [T-expt : Integer 0]
                        #:Q^ [Q-expt : Integer 0]
                        #:Θ^ [Θ-expt : Integer 0]) : Dimension
  (dimension M-expt L-expt T-expt Q-expt Θ-expt))



(define-type Dimensionless-Dimension (dimension 0 0 0 0 0))
(: dimensionless-dimension : Dimensionless-Dimension)
(define dimensionless-dimension (dimension 0 0 0 0 0))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: dimension-write-proc : [Dimension Output-Port (U 0 1 #t #f) -> Void])
(define (dimension-write-proc d out mode)
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
  (let ([M-expt   : Integer (dimension-M-expt d)]
        [L-expt : Integer (dimension-L-expt d)]
        [T-expt   : Integer (dimension-T-expt d)]
        [Q-expt : Integer (dimension-Q-expt d)]
        [Θ-expt   : Integer (dimension-Θ-expt d)])
    (cond [(= 0 M-expt L-expt T-expt Q-expt Θ-expt) (write-s "(d*)")]
          [(= 0 L-expt T-expt Q-expt Θ-expt) (write-sub "M" M-expt)]
          [(= 0 M-expt T-expt Q-expt Θ-expt) (write-sub "L" L-expt)]
          [(= 0 M-expt L-expt Q-expt Θ-expt) (write-sub "T" T-expt)]
          [(= 0 M-expt L-expt T-expt Θ-expt) (write-sub "C" Q-expt)]
          [(= 0 M-expt L-expt T-expt Q-expt) (write-sub "Θ" Θ-expt)]
          [else
           (: maybe-write-sub : [String Integer -> Void])
           (define (maybe-write-sub str expt)
             (cond [(zero? expt) (void)]
                   [else (write-char #\space out)
                         (display-sub str expt)]))
           (write-s "(d*")
           (maybe-write-sub "M" M-expt)
           (maybe-write-sub "L" L-expt)
           (maybe-write-sub "T" T-expt)
           (maybe-write-sub "Q" Q-expt)
           (maybe-write-sub "Θ" Θ-expt)
           (write-char #\) out)]
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(untyped-module*
 )



