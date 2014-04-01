#lang typed/racket

(provide (all-defined-out))

(: freezing-temperature Flonum)
(define freezing-temperature 273.15)

(: temperature-lapse-rate Flonum)
(define temperature-lapse-rate 9.8e-3)

(: temperature-lapse (Flonum -> Flonum))
(define (temperature-lapse altitude)
  (* altitude temperature-lapse-rate))
