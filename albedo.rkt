#lang typed/racket

(require "planet-structs.rkt"
         "tile-terrain.rkt"
         "tile-atmosphere.rkt"
         racket/flonum)

(provide planet-tile-albedo)

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

(: water-surface-albedo (planet-tile -> Flonum))
(define (water-surface-albedo tile)
  (interpolate water-albedo
               water-ice-albedo
               (planet-tile-ice-cover tile)))

(: land-surface-albedo (planet-tile -> Flonum))
(define (land-surface-albedo tile)
  (interpolate
   (interpolate desert-albedo
                forest-albedo
                (planet-tile-vegetation-cover tile))
   snow-albedo
   (planet-tile-snow-cover tile)))

(: surface-albedo (planet-tile -> Flonum))
(define (surface-albedo tile)
  (if (planet-tile-water? tile)
      (water-surface-albedo tile)
      (land-surface-albedo tile)))

(: sky-albedo (planet-tile -> Flonum))
(define (sky-albedo tile)
  (* cloud-albedo
     (planet-tile-cloud-cover tile)))

(: planet-tile-albedo (planet-tile -> Flonum))
(define (planet-tile-albedo tile)
  (- 1.0 (* (- 1.0 (surface-albedo tile))
            (- 1.0 (sky-albedo tile)))))