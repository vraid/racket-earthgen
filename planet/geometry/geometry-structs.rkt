#lang typed/racket

(provide (all-defined-out))

(require vraid/struct
         vraid/types
         "../direct-access.rkt"
         "../grid/grid-structs.rkt")

(struct: tile-geometry-data
  ([area : float-get]))

(struct/kw: planet-geometry grid
            ([axis : FlVector]
             [radius : Float]
             [tile : tile-geometry-data]))

(define planet-axis planet-geometry-axis)

(define planet-radius planet-geometry-radius)

(direct-access planet-geometry tile tile-geometry-data
               ([area Float]))
