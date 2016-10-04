#lang typed/racket

(provide (all-defined-out))

(require vraid/struct)

(vector-struct tile-climate-data
               ([snow : Float]
                [sunlight : Float]
                [temperature : Float]
                [humidity : Float]
                [precipitation : Float]
                [leaf-area-index : Float]))

(vector-struct corner-climate-data
               ([river-flow : Float]))

(vector-struct edge-climate-data
               ([river-flow : Float]
                [air-flow : Float]))
