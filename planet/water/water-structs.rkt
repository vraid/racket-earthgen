#lang typed/racket

(provide (all-defined-out)
         (all-from-out "water-data-structs.rkt"))

(require vraid/struct
         "../direct-access.rkt"
         "water-data-structs.rkt"
         "../terrain.rkt")

(struct/kw: river
            ([location : Integer]
             [sources : river-list])
            #:transparent)

(define-type river-list (Listof river))

(struct/kw: planet-water planet-terrain
            ([sea-level : Float]
             [tile : tile-water-data]
             [corner : corner-water-data]
             [rivers : river-list]))

(define planet-sea-level planet-water-sea-level)
(define planet-rivers planet-water-rivers)

(direct-access planet-water tile tile-water-data
               ([water-level Float]))
