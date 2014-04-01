#lang typed/racket

(provide (all-defined-out))

(require "typed-struct-kw.rkt")

(struct/kw: climate-variables
            ([season : Integer]
             [time-of-year : Flonum]
             [solar-equator : Flonum])
            #:transparent)

(struct/kw: climate-parameters
            ([axial-tilt : Flonum]
             [seasons-per-cycle : Positive-Integer]
             [acceptable-delta : Flonum])
            #:transparent)

(define initial-climate-variables
  (climate-variables/kw
   #:season -1
   #:time-of-year 0.0
   #:solar-equator 0.0))
