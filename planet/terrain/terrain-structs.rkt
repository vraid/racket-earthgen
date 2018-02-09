#lang typed/racket

(require vraid/struct
         "../direct-access.rkt"
         "../geometry/geometry-structs.rkt")

(provide (all-defined-out))

(struct/kw river
           ([location : Integer]
            [sources : river-list])
           #:transparent)

(define-type river-list (Listof river))

(vector-struct tile-terrain-data
               ([elevation : flvec Float]
                [water-level : flvec Float]))

(vector-struct corner-terrain-data
               ([elevation : flvec Float]
                [river-direction : vec (Option Integer)]))

(struct/kw planet-terrain planet-geometry
           ([sea-level : Float]
            [tile : tile-terrain-data]
            [corner : corner-terrain-data]
            [rivers : river-list]))

(define planet-sea-level planet-terrain-sea-level)
(define planet-rivers planet-terrain-rivers)

(direct-access planet-terrain tile tile-terrain-data
               ([elevation Float]
                [water-level Float]))

(direct-access planet-terrain corner corner-terrain-data
               ([elevation Float]
                [river-direction (Option Integer)]))
