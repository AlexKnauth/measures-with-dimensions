#lang at-exp racket/base

(provide deftype)

(require scribble/manual
         (for-syntax racket/base
                     syntax/parse))

(define-syntax deftype
  (syntax-parser
    [@deftype[id:id stuff:expr ...]
     #'@defidform[#:kind "type" id stuff ...]]
    [@deftype[(~and form-dat (id:id . args:expr)) stuff:expr ...]
     #'@defform[#:kind "type" form-dat stuff ...]]))

