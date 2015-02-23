#lang typed/racket/base

(provide (struct-out compound)
         Compound
         ->compound
         )

(require racket/match
         (only-in typed/racket/base [U Un])
         "element-struct.rkt"
         "elements.rkt"
         )

(struct compound
  ([alist : (Listof (Pairof (Un Element Compound) Natural))])
  #:transparent
  #:property prop:custom-write
  (lambda ([comp : Compound] [out : Output-Port] [mode : (Un 0 1 #t #f)])
    (match mode
      [_ #:when (andmap zero? (map cdr (compound-alist comp)))
         (write-string "#<compound:>" out) (void)]
      [(or 0 #f) (display-compound comp out)]
      [(or 1 #t) (write-string "#<componud:" out)
                 (display-compound comp out)
                 (write-string ">" out)
                 (void)]))
  )

(define-type Compound compound)

(: ->compound : [(Un Compound Element Symbol) -> Compound])
(define (->compound x)
  (match x
    [(? compound?) x]
    [(? element?) (compound (list (cons x 1)))]
    [(? symbol?) (->compound (->element x))]
    ))

(: display-compound : [Compound Output-Port -> Void])
(define (display-compound comp out)
  (for ([p (in-list (compound-alist comp))])
    (match-define (cons sub n) p)
    (match sub
      [(? element?) (match n
                      [0 (void)]
                      [1 (fprintf out "~a" (element-symbol sub))]
                      [_ (fprintf out "~a~a" (element-symbol sub) n)])]
      [(? compound?) (match n
                       [0 (void)]
                       [1 (fprintf out "(~a)" sub)]
                       [_ (fprintf out "(~a)~a" sub n)])])))
