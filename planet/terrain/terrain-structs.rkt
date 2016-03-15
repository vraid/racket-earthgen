#lang typed/racket

(require vraid/struct
         vraid/require
         "../direct-access.rkt"
         "../geometry/geometry-structs.rkt")

(require/provide "terrain-data-structs.rkt")

(provide (all-defined-out))

(struct/kw: terrain-parameters
            ([grid-size : Integer]
             [sea-level : Float]
             [axis : FlVector]))

(struct/kw: planet-terrain planet-geometry
            ([tile : tile-terrain-data]
             [corner : corner-terrain-data]))

(direct-access planet-terrain tile tile-terrain-data
               ([elevation Float]))

(direct-access planet-terrain corner corner-terrain-data
               ([elevation Float]))
