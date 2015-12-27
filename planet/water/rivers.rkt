#lang typed/racket

(provide (all-defined-out))

(require vraid/types
         vraid/flow
         "water-structs.rkt"
         "../grid.rkt")

(: corner-river-direction (planet-water integer -> (maybe integer)))
(define (corner-river-direction planet n)
  (let ([direction ((corner-water-data-river-direction (planet-water-corner planet)) n)])
    (if (<= 0 direction)
        direction
        #f)))

(: corner-river-destination (planet-water integer -> (maybe integer)))
(define (corner-river-destination planet n)
  (and-let ([direction (corner-river-direction planet n)])
    ((grid-corner-corner planet) n direction)))

(: river-flows-to? (planet-water integer -> (integer -> Boolean)))
(define ((river-flows-to? planet to) from)
  (and-let ([destination (corner-river-destination planet from)])
    (= to destination)))

(: corner-river-sources (planet-water integer -> integer-list))
(define (corner-river-sources planet n)
  (filter (river-flows-to? planet n)
          (grid-corner-corner-list planet n)))
