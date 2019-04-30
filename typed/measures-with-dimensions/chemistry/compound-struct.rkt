#lang sweet-exp typed/racket/base

provide (struct-out compound)
        Compound
        ->compound


require racket/match
        (only-in typed/racket/base [U Un])
        "element-struct.rkt"
        "elements.rkt"
        "../untyped-utils.rkt"


(struct compound
  ([alist : (Listof (Pairof (Un Element Compound) Natural))])
  #:transparent
  #:property prop:custom-write
  (lambda ([comp : Compound] [out : Output-Port] [mode : (Un 0 1 #t #f)])
    (match mode
      [_ #:when (andmap zero? (map (inst cdr (Un Element Compound) Natural)
                                   (compound-alist comp)))
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
  (cond
    [(compound? x) x]
    [(element? x) (compound (list (cons x 1)))]
    [(symbol? x) (->compound (->element x))]
    ))

(: display-compound : [Compound Output-Port -> Void])
(define (display-compound comp out)
  (for ([p (in-list (compound-alist comp))])
    (match-define (cons sub n) p)
    (cond
      [(element? sub) (match n
                        [0 (void)]
                        [1 (fprintf out "~a" (element-symbol sub))]
                        [_ (fprintf out "~a~a" (element-symbol sub) n)])]
      [(compound? sub) (match n
                         [0 (void)]
                         [1 (fprintf out "(~a)" sub)]
                         [_ (fprintf out "(~a)~a" sub n)])]
      [else (error 'display-compound "sub: ~v" sub)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(untyped-module*
 )
