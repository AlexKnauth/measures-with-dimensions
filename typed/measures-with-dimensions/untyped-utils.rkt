#lang racket/base

(provide untyped-module*
         combine-in/priority
         )

(require racket/require
         racket/require-syntax
         (for-syntax racket/base
                     syntax/parse
                     syntax/strip-context
                     version/utils
                     ))

(define-require-syntax combine-in/priority
  (lambda (stx)
    (syntax-parse stx
      [(combine-in/priority)
       #'(combine-in)]
      [(combine-in/priority require-spec:expr)
       #'require-spec]
      [(combine-in/priority spec1:expr spec2:expr)
       #'(combine-in spec1 (subtract-in spec2 spec1))]
      [(combine-in/priority spec1:expr spec:expr ...)
       #'(combine-in/priority spec1 (combine-in/priority spec ...))]
      )))

(define-syntax untyped-module*
  (lambda (stx)
    (syntax-parse stx
      [(untyped-module*
        (~or [#:begin stuff ...]
             [id:id type:expr]
             [id2:id #:from from-module-path:expr]
             [id3 type3:expr #:as id4]
             [#:all-from all-from-module-path:expr ...])
        ...
        )
       #:with submodup/requnctc
       (if (version<=? "7.7.0.2" (version)) #'(submod "..") #'(submod ".." ".."))
       (replace-context
        stx
        #'(module* untyped racket/base
           (provide (all-from-out (submod "..") all-from-module-path ... ...)
                    id ... id2 ... (rename-out [id3 id4] ...))
           (require racket/require
                    typed/measures-with-dimensions/untyped-utils
                    typed/untyped-utils
                    (except-in (combine-in/priority
                                (combine-in all-from-module-path ... ...
                                            (only-in from-module-path id2) ...)
                                (submod "..")
                                (subtract-in typed/racket/base
                                             racket/base))
                               id ... id3 ...))
           stuff ... ...
           (require/untyped-contract
            (begin (require typed/measures-with-dimensions/untyped-utils
                            (except-in (combine-in/priority
                                        (combine-in all-from-module-path ... ...
                                                    (only-in from-module-path id2) ...)
                                        (submod ".." ".."))
                                       id ... id3 ...))
                   stuff ... ...)
            submodup/requnctc
            [id type] ... [id3 type3] ...)))
       ])))

