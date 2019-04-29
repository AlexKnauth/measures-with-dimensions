#lang sweet-exp racket/base

provide compound
        compound:
        Compound
        compound?
        make-compound
        compound-alist


require racket/match
        "element-struct.rkt"
        "elements.rkt"
        (rename-in "compound-struct.rkt" [compound make-compound])
        for-syntax racket/base
                   syntax/parse
                   racket/match
                   (only-in colon-match :match define-:match-class)
                   racket/list
                   threading
                   predicates

module+ test
  require rackunit

(begin-for-syntax
  (define-syntax-class compound-match-exp-clause
    [pattern [sub n] #:with norm #'(cons sub n)]
    [pattern (~literal ...) #:with norm this-syntax]
    )
  )

(define-match-expander compound
  (syntax-parser
    [(compound :compound-match-exp-clause ...)
     #'(app ->compound (make-compound (list-no-order norm ...)))])
  (syntax-parser
    [(compound [sub n] ...)
     #'(make-compound (list (cons sub n) ...))]))

(begin-for-syntax
  (define-:match-class cau (and? char-alphabetic? char-upper-case?))
  (define-:match-class cal (and? char-alphabetic? char-lower-case?))
  (define-:match-class cnm char-numeric?)
  ;; parse-compound-string : String -> (Listof (Cons Symbol PosInt))
  (define (parse-compound-string str)
    (:match (string->list str)
      [(list) '()]
      [(list-rest C:cau cs:cal ... cns:cnm ... (and rst (or '() (cons :cau _))))
       (define sym (~> (cons C cs) list->string string->symbol))
       (define n (if (empty? cns) 1 (~> cns list->string string->number)))
       (cons (cons sym n) (parse-compound-string (list->string rst)))]
      ))
  )

(define-syntax compound:
  (syntax-parser
    [(compound: id:id)
     #:do [(define str (~> #'id syntax-e symbol->string))
           (define alist (parse-compound-string str))]
     #:with (cls ...) (for/list ([p (in-list alist)])
                        (match-define (cons sym n) p)
                        (with-syntax ([id (datum->syntax #'id sym #'id #'id)]
                                      [n (datum->syntax #'id n #'id)])
                          (syntax/loc #'id [id n])))
     (syntax/loc #'id (compound cls ...))]))

(module+ test
  (check-equal? (compound: H2O) (compound [H 2] [O 1]))
  (check-equal? (format "~v" (compound: H2O)) "H2O")
  )
