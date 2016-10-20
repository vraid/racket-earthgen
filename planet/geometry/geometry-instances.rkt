#lang typed/racket

(require "geometry-structs.rkt"
         "../grid/grid-create.rkt")

(provide (all-defined-out))

(define empty-planet-geometry
  (planet-geometry/kw
   #:grid (n-grid 0)
   #:axis default-axis
   #:radius 0.0
   #:tile (tile-geometry-data
           (lambda ([n : Integer])
             0.0))))
