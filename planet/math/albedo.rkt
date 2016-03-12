#lang typed/racket

(provide tile-albedo)

(require "../water.rkt"
         "../climate.rkt"
         racket/flonum)

(define cloud-albedo 0.8)
(define water-albedo 0.1)
(define water-ice-albedo 0.6)
(define snow-albedo 0.8)
(define desert-albedo 0.4)
(define forest-albedo 0.15)
(define grassland-albedo 0.25)

(: interpolate (Float Float Float -> Float))
(define (interpolate a b d)
  (fl+ (fl* a (fl- 1.0 d))
       (fl* b d)))

(: water-surface-albedo (planet-climate Integer -> Float))
(define (water-surface-albedo p n)
  water-albedo)

(: land-surface-albedo (planet-climate Integer -> Float))
(define (land-surface-albedo p n)
  (interpolate
   desert-albedo
   snow-albedo
   (tile-snow-cover p n)))

(: surface-albedo (planet-climate Integer -> Float))
(define (surface-albedo p n)
  (if (tile-water? p n)
      (water-surface-albedo p n)
      (land-surface-albedo p n)))

(: sky-albedo (planet-climate Integer -> Float))
(define (sky-albedo p n)
  (* cloud-albedo
     (tile-cloud-cover p n)))

(: tile-albedo (planet-climate Integer -> Float))
(define (tile-albedo p n)
  (- 1.0 (* (- 1.0 (surface-albedo p n))
            (- 1.0 (sky-albedo p n)))))
