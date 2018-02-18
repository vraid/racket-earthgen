#lang typed/racket

(provide (all-defined-out))

(require racket/flonum)

(: freezing-temperature Float)
(define freezing-temperature 273.15)

(: below-freezing-temperature? (Float -> Boolean))
(define (below-freezing-temperature? temperature)
  (fl<= temperature freezing-temperature))

(define above-freezing-temperature?
  (compose not below-freezing-temperature?))

(: temperature-lapse-rate Float)
(define temperature-lapse-rate 9.8e-3)

(: temperature-lapse (Float -> Float))
(define (temperature-lapse altitude)
  (* altitude temperature-lapse-rate))
