#lang racket

(require "planet-tile-struct.rkt"
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

(define (interpolate a b d)
  (fl+ (fl* a (fl- 1.0 d))
       (fl* b d)))

(define (water-surface-albedo tile)
  (interpolate water-albedo
               water-ice-albedo
               (planet-tile-ice-cover tile)))

(define (land-surface-albedo tile)
  (interpolate
   (interpolate desert-albedo
               forest-albedo
               (planet-tile-vegetation-cover tile)))
  snow-albedo
  (planet-tile-snow-cover tile))

(define (surface-albedo tile)
  (if (planet-tile-water? tile)
      (water-surface-albedo tile)
      (land-surface-albedo tile)))

(define (sky-albedo tile)
  (fl* cloud-albedo
       (planet-tile-cloud-cover tile)))

(define (planet-tile-albedo tile)
  (fl- 1.0 (fl* (fl- 1.0 (surface-albedo tile))
                (fl- 1.0 (sky-albedo tile)))))