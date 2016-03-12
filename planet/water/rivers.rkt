#lang typed/racket

(provide (all-defined-out))

(require vraid/types
         vraid/flow
         "water-structs.rkt"
         "../grid.rkt")

(: corner-river-direction (planet-water Integer -> (Option Integer)))
(define (corner-river-direction planet n)
  (let ([direction ((corner-water-data-river-direction (planet-water-corner planet)) n)])
    (if (<= 0 direction)
        direction
        #f)))

(: corner-river-destination (planet-water Integer -> (Option Integer)))
(define (corner-river-destination planet n)
  (and-let ([direction (corner-river-direction planet n)])
    ((grid-corner-corner planet) n direction)))

(: river-flows-to? (planet-water Integer -> (Integer -> Boolean)))
(define ((river-flows-to? planet to) from)
  (and-let ([destination (corner-river-destination planet from)])
    (= to destination)))

(: corner-river-sources (planet-water Integer -> integer-list))
(define (corner-river-sources planet n)
  (filter (river-flows-to? planet n)
          (grid-corner-corner-list planet n)))
