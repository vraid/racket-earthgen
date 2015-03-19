#lang typed/racket

(provide (all-defined-out))

(require "temperature.rkt"
         vraid/math
         math/flonum)

(: saturation-humidity (Flonum -> Flonum))
(define (saturation-humidity temperature)
  (* 4.6e-9
     (flexp (* 0.05174 temperature))))

(: relative-humidity (Flonum Flonum -> Flonum))
(define (relative-humidity temperature humidity)
  (min 1.0
       (/ humidity
          (saturation-humidity temperature))))

(: potential-evapotranspiration (Flonum Flonum -> Flonum))
(define (potential-evapotranspiration temperature humidity)
  (subtract humidity
            (saturation-humidity temperature)))

(define aridity-reference (potential-evapotranspiration
                           (+ freezing-temperature
                              30.0)
                           0.0))

(: aridity (Flonum Flonum -> Flonum))
(define (aridity temperature humidity)
  (fl/ (potential-evapotranspiration temperature humidity)
       aridity-reference))
