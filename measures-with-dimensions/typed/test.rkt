#lang typed/racket

(require typed/rackunit)

(require "measures.rkt")

;; Dimmensions

(check-equal? (d* velocity-dimmension 'time-dimmension) 'length-dimmension)
(check-equal? (d* acceleration-dimmension 'time-dimmension) velocity-dimmension)

(check-equal? (d* 'mass-dimmension acceleration-dimmension) force-dimmension)

(check-equal? (d* force-dimmension 'length-dimmension) energy-dimmension)
(check-equal? (d* 'mass-dimmension (dsqr velocity-dimmension)) energy-dimmension)

(check-equal? (dsqrt (d* acceleration-dimmension 'length-dimmension)) velocity-dimmension)