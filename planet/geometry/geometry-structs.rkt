#lang typed/racket

(provide (all-defined-out))

(require vraid/struct
         vraid/types
         "../direct-access.rkt"
         "../grid/grid-structs.rkt")

(struct: tile-geometry-data
  ([area : (integer -> flonum)]))

(struct/kw: planet-geometry grid
            ([axis : FlVector]
             [radius : flonum]
             [tile : tile-geometry-data]))

(define planet-axis planet-geometry-axis)

(define planet-radius planet-geometry-radius)

(direct-access planet-geometry tile tile-geometry-data
               ([area flonum]))
