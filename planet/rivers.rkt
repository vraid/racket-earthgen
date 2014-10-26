#lang typed/racket

(provide (all-defined-out))

(require vraid/types
         vraid/flow
         "planet-structs.rkt")

(: corner-river-destination (planet Integer -> (maybe Integer)))
(define (corner-river-destination planet n)
  (and-let ([direction (corner-river-direction planet n)])
    ((grid-corner-corner planet) n direction)))

(: river-flows-to? (planet Integer -> (Integer -> Boolean)))
(define ((river-flows-to? planet to) from)
  (and-let ([destination (corner-river-destination planet from)])
    (= to destination)))

(: corner-river-sources (planet integer -> integer-list))
(define (corner-river-sources planet n)
  (filter (river-flows-to? planet n)
          (grid-corner-corner-list planet n)))
