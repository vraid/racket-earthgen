#lang typed/racket

(require vraid/flow
         "terrain-structs.rkt"
         "../grid-base.rkt")

(provide (all-defined-out))

(: corner-river-destination (planet-terrain Integer -> (Option Integer)))
(define (corner-river-destination planet n)
  (and-let ([direction (corner-river-direction planet n)])
    ((grid-corner-corner planet) n direction)))

(: river-flows-to? (planet-terrain Integer -> (Integer -> Boolean)))
(define ((river-flows-to? planet to) from)
  (equal? to (corner-river-destination planet from)))

(: corner-river-sources (planet-terrain Integer -> (Listof Integer)))
(define (corner-river-sources planet n)
  (filter (river-flows-to? planet n)
          (grid-corner-corner-list planet n)))
