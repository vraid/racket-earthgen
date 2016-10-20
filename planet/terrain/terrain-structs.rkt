#lang typed/racket

(require vraid/struct
         "../direct-access.rkt"
         "../geometry/geometry-structs.rkt")

(provide (all-defined-out))

(struct/kw terrain-parameters
           ([algorithm : Any]
            [seed : String]
            [grid-size : Integer]
            [radius : Float]
            [sea-level : Float]
            [axis : FlVector]))

(vector-struct tile-terrain-data
               ([elevation : Float]))

(vector-struct corner-terrain-data
               ([elevation : Float]))

(struct/kw planet-terrain planet-geometry
           ([tile : tile-terrain-data]
            [corner : corner-terrain-data]))

(direct-access planet-terrain tile tile-terrain-data
               ([elevation Float]))

(direct-access planet-terrain corner corner-terrain-data
               ([elevation Float]))
