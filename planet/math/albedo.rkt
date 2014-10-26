#lang typed/racket

(require vraid/types
         "../planet-structs.rkt"
         "../tile-terrain.rkt"
         "atmosphere.rkt"
         racket/flonum)

(provide tile-albedo)

(define cloud-albedo 0.8)
(define water-albedo 0.1)
(define water-ice-albedo 0.6)
(define snow-albedo 0.8)
(define desert-albedo 0.4)
(define forest-albedo 0.15)
(define grassland-albedo 0.25)

(: interpolate (Flonum Flonum Flonum -> Flonum))
(define (interpolate a b d)
  (fl+ (fl* a (fl- 1.0 d))
       (fl* b d)))

(: water-surface-albedo (planet integer -> Flonum))
(define (water-surface-albedo p n)
  (interpolate water-albedo
               water-ice-albedo
               (tile-ice-cover p n)))

(: land-surface-albedo (planet integer -> Flonum))
(define (land-surface-albedo p n)
  (interpolate
   (interpolate desert-albedo
                forest-albedo
                (tile-vegetation-cover p n))
   snow-albedo
   (tile-snow-cover p n)))

(: surface-albedo (planet integer -> Flonum))
(define (surface-albedo p n)
  (if (tile-water? p n)
      (water-surface-albedo p n)
      (land-surface-albedo p n)))

(: sky-albedo (planet integer -> Flonum))
(define (sky-albedo p n)
  (* cloud-albedo
     (tile-cloud-cover p n)))

(: tile-albedo (planet integer -> Flonum))
(define (tile-albedo p n)
  (- 1.0 (* (- 1.0 (surface-albedo p n))
            (- 1.0 (sky-albedo p n)))))