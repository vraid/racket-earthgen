#lang typed/racket

(provide (all-defined-out))

(require vraid/struct
         vraid/types
         math/flonum
         "../direct-access.rkt"
         "../grid.rkt")

(struct: tile-geometry-data
  ([area : float-get]))

(struct/kw: planet-geometry grid
            ([axis : FlVector]
             [radius : Float]
             [tile : tile-geometry-data]))

(define default-radius 6371000.0)
(define default-axis (flvector 0.0 0.0 1.0))

(define empty-planet-geometry
  (planet-geometry/kw
   #:grid (n-grid 0)
   #:axis default-axis
   #:radius 0.0
   #:tile (tile-geometry-data
           (lambda ([n : Integer])
             0.0))))

(define planet-axis planet-geometry-axis)

(define planet-radius planet-geometry-radius)

(direct-access planet-geometry tile tile-geometry-data
               ([area Float]))
