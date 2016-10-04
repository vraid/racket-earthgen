#lang typed/racket

(provide (all-defined-out))

(require vraid/struct)

(vector-struct tile-terrain-data
               ([elevation : Float]))

(vector-struct corner-terrain-data
               ([elevation : Float]))
