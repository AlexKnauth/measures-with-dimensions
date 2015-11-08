#lang sweet-exp racket/base

provide kelvin:
        rankine:
        celcius:
        fahrenheit:


require racket/match
        "temperature-functions.rkt"
        for-syntax racket/base
                   syntax/parse
                   unstable/syntax


(define-match-expander kelvin:
  (syntax-parser
    [(kelvin: pat)
     #'(app get-kelvin pat)])
  (make-variable-like-transformer #'make-kelvin))

(define-match-expander rankine:
  (syntax-parser
    [(rankine: pat)
     #'(app get-rankine pat)])
  (make-variable-like-transformer #'make-rankine))

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

