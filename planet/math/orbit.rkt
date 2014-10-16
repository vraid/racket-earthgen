#lang typed/racket

(require vraid/math)

(provide solar-equator
         solar-irradiance)

(: solar-equator (Flonum Flonum -> Flonum))
(define (solar-equator axial-tilt time-of-year)
  (* axial-tilt
     (sin (* time-of-year tau))))

(: solar-irradiance (Flonum Flonum -> Flonum))
(define (solar-irradiance solar-equator latitude)
  (max 0.0
       (cos (- solar-equator latitude))))
