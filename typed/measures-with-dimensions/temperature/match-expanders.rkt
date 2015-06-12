#lang racket/base

(provide celcius:
         fahrenheit:
         )

(require racket/match
         "temperature-functions.rkt"
         (for-syntax racket/base
                     syntax/parse
                     unstable/syntax
                     ))

(define-match-expander celcius:
  (syntax-parser
    [(celcius: pat)
     #'(app get-celcius pat)])
  (make-variable-like-transformer #'celcius))

(define-match-expander fahrenheit:
  (syntax-parser
    [(fahrenheit: pat)
     #'(app get-fahrenheit pat)])
  (make-variable-like-transformer #'fahrenheit))

