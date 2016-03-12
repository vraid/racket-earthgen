#lang typed/racket

(provide (all-defined-out))

(require vraid/math)

(: solar-equator (Float Float -> Float))
(define (solar-equator axial-tilt time-of-year)
  (* axial-tilt
     (sin (* time-of-year tau))))

(: solar-irradiance (Float Float -> Float))
(define (solar-irradiance solar-equator latitude)
  (max 0.0
       (cos (- solar-equator latitude))))
