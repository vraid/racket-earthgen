#lang typed/racket

(provide (all-defined-out))

(require racket/flonum)

(: freezing-temperature Float)
(define freezing-temperature 273.15)

(: above-freezing-temperature? (Float -> Boolean))
(define (above-freezing-temperature? temperature)
  (fl> temperature freezing-temperature))

(: below-freezing-temperature? (Float -> Boolean))
(define (below-freezing-temperature? temperature)
  (fl< temperature freezing-temperature))

(: temperature-lapse-rate Float)
(define temperature-lapse-rate 9.8e-3)

(: temperature-lapse (Float -> Float))
(define (temperature-lapse altitude)
  (* altitude temperature-lapse-rate))
