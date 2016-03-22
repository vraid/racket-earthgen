#lang typed/racket

(provide (all-defined-out))

(require vraid/math
         math/flonum
         "temperature.rkt")

(: saturation-humidity (Float -> Float))
(define (saturation-humidity temperature)
  (* 4.6e-9
     (flexp (* 0.05174 temperature))))

(: relative-humidity (Float Float -> Float))
(define (relative-humidity temperature humidity)
  (min 1.0
       (/ humidity
          (saturation-humidity temperature))))

(: potential-evapotranspiration (Float Float -> Float))
(define (potential-evapotranspiration temperature humidity)
  (subtract-by humidity
               (saturation-humidity temperature)))

(define aridity-reference (potential-evapotranspiration
                           (+ freezing-temperature
                              30.0)
                           0.0))

(: aridity (Float Float -> Float))
(define (aridity temperature humidity)
  (max 0.0
       (fl/ (potential-evapotranspiration temperature humidity)
            aridity-reference)))
