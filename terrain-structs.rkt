#lang racket

(require math/flonum)

(provide terrain
         terrain-tile-elevations
         terrain-corner-elevations
         terrain-tile-water-levels
         terrain-tile-elevation
         terrain-corner-elevation
         terrain-tile-water-level)

(struct terrain
  (tile-elevations
   corner-elevations
   tile-water-levels)
  #:transparent)

(define (terrain-tile-elevation t n)
  (flvector-ref (terrain-tile-elevations t) n))

(define (terrain-corner-elevation t n)
  (flvector-ref (terrain-corner-elevations t) n))

(define (terrain-tile-water-level t n)
  (flvector-ref (terrain-tile-water-levels t) n))