#lang typed/racket

(provide (all-defined-out))

(require vraid/struct)

(vector-struct tile-water-data
               ([water-level : Float]))

(vector-struct corner-water-data
               ([river-direction : (Option Integer)]))
