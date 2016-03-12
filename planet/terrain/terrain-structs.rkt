#lang typed/racket

(provide (all-defined-out)
         (all-from-out "terrain-data-structs.rkt"))

(require vraid/types
         vraid/struct
         "../geometry/geometry-structs.rkt"
         "terrain-data-structs.rkt"
         "../direct-access.rkt")

(struct/kw: planet-terrain planet-geometry
            ([tile : tile-terrain-data]
             [corner : corner-terrain-data]))

(direct-access planet-terrain tile tile-terrain-data
               ([elevation Float]))

(direct-access planet-terrain corner corner-terrain-data
               ([elevation Float]))
