#lang typed/racket

(require vraid/struct
         vraid/require
         "../direct-access.rkt"
         "../terrain/terrain-structs.rkt")

(require/provide "water-data-structs.rkt")

(provide (all-defined-out))

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
