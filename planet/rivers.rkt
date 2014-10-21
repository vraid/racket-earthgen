#lang typed/racket

(provide (all-defined-out))

(require vraid/types
         vraid/flow
         "planet-structs.rkt")

(: river-destination (planet Integer -> (maybe Integer)))
(define (river-destination planet n)
  (and-let ([direction (corner-river-direction planet n)])
    ((grid-corner-corner planet) n direction)))

(: river-flows-to? (planet Integer -> (Integer -> Boolean)))
(define ((river-flows-to? planet to) from)
  (and-let ([target (river-destination planet from)])
    (= to target)))
