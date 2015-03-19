#lang typed/racket

(provide (all-defined-out))

(require vraid/types
         "terrain-structs.rkt"
         "tile-terrain.rkt"
         "../grid.rkt")

(: corner-land? (planet-terrain integer -> Boolean))
(define (corner-land? planet n)
  (not (ormap (curry tile-water? planet) (grid-corner-tile-list planet n))))

(: corner-water? (planet-terrain integer -> Boolean))
(define (corner-water? planet n)
  (not (ormap (curry tile-land? planet) (grid-corner-tile-list planet n))))

(: corner-coast? (planet-terrain integer -> Boolean))
(define (corner-coast? planet n)
  (not (zero? (remainder (count (curry tile-land? planet) (grid-corner-tile-list planet n))
                         3))))
