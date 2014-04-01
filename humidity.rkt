#lang typed/racket

(provide (all-defined-out))

(require "math.rkt"
         math/flonum)

(: saturation-humidity (Flonum -> Flonum))
(define (saturation-humidity temperature)
  (* 4.6e-9
     (flexp (* 0.05174 temperature))))

(: relative-humidity (Flonum Flonum -> Flonum))
(define (relative-humidity temperature humidity)
  (/ humidity
     (saturation-humidity temperature)))

(: potential-evapotranspiration (Flonum Flonum -> Flonum))
(define (potential-evapotranspiration temperature humidity)
  (subtract humidity
            (saturation-humidity temperature)))
