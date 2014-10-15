#lang typed/racket/base

(provide defmulti)

(require syntax/parse/define
         (for-syntax racket/base
                     syntax/parse)
         )

(begin-for-syntax
  (define-syntax-class clause
    [pattern [id:id expr:expr]
             #:with def #'(define id expr)]
    [pattern [id:id (~and colon (~literal :)) type:expr expr:expr]
             #:with def #'(define id colon type expr)]))

(define-simple-macro
  (defmulti clause:clause ...)
  (begin clause.def ...))

