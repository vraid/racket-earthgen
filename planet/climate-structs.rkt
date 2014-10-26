#lang typed/racket

(provide (all-defined-out))

(require vraid/struct)

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

(: default-climate-parameters (-> climate-parameters))
(define (default-climate-parameters)
  (climate-parameters/kw
   #:acceptable-delta 0.01
   #:axial-tilt (/ pi 8.0)
   #:seasons-per-cycle 24))

(define initial-climate-variables
  (climate-variables/kw
   #:season 0
   #:time-of-year 0.0
   #:solar-equator 0.0))
