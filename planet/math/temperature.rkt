#lang typed/racket

(provide (all-defined-out))

(require racket/flonum)

(: freezing-temperature Flonum)
(define freezing-temperature 273.15)

(: above-freezing-temperature? (Flonum -> Boolean))
(define (above-freezing-temperature? temperature)
  (fl> temperature freezing-temperature))

(: below-freezing-temperature? (Flonum -> Boolean))
(define (below-freezing-temperature? temperature)
  (fl< temperature freezing-temperature))

(: temperature-lapse-rate Flonum)
(define temperature-lapse-rate 9.8e-3)

(: temperature-lapse (Flonum -> Flonum))
(define (temperature-lapse altitude)
  (* altitude temperature-lapse-rate))
