#lang typed/racket

(require vraid/struct
         vraid/types
         math/flonum
         "../direct-access.rkt"
         "../grid/grid-structs.rkt")

(provide (all-defined-out))

(struct tile-geometry-data
  ([area : float-get]))

(struct/kw planet-geometry grid
           ([axis : FlVector]
            [radius : Float]
            [tile : tile-geometry-data]))

(define default-radius 6371000.0)
(define default-axis (flvector 0.0 0.0 1.0))

(define planet-axis planet-geometry-axis)

(define planet-radius planet-geometry-radius)

(direct-access planet-geometry tile tile-geometry-data
               ([area Float]))
