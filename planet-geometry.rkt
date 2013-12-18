#lang typed/racket

(provide (all-defined-out))

(require "types.rkt"
         "planet-structs.rkt"
         "planet-variables.rkt"
         "grid.rkt"
         "vector3.rkt")

(: coordinate-latitude (planet flvector3 -> Flonum))
(define (coordinate-latitude p v)
  (- (/ pi 2.0)
     (acos (flvector3-dot-product v (planet-axis p)))))

(: tile-latitude (planet index -> Flonum))
(define (tile-latitude p n)
  (coordinate-latitude p (tile-coordinates (grid-tile (planet-grid p) n))))
