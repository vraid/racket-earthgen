#lang typed/racket

(provide (all-defined-out)
         (all-from-out "terrain-data-structs.rkt"))

(require vraid/types
         vraid/struct
         "../geometry/geometry-structs.rkt"
         "terrain-data-structs.rkt"
         "../direct-access.rkt")

(struct/kw: river
            ([location : integer]
             [sources : river-list])
            #:transparent)

(define-type river-list (Listof river))

(struct/kw: planet-terrain planet-geometry
            ([sea-level : Flonum]
             [tile : tile-terrain-data]
             [corner : corner-terrain-data]
             [rivers : river-list]))

(direct-access planet-terrain tile tile-terrain-data
               ([elevation flonum]
                [water-level flonum]))

(direct-access planet-terrain corner corner-terrain-data
               ([elevation flonum]))

(define planet-sea-level planet-terrain-sea-level)
(define planet-rivers planet-terrain-rivers)
