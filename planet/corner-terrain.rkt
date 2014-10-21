#lang typed/racket

(provide (all-defined-out))

(require vraid/types
         "planet-structs.rkt"
         "tile-terrain.rkt")

(: corner-land? (planet integer -> Boolean))
(define (corner-land? planet n)
  (not (ormap (curry tile-water? planet) (grid-corner-tile-list planet n))))

(: corner-water? (planet integer -> Boolean))
(define (corner-water? planet n)
  (not (ormap (curry tile-land? planet) (grid-corner-tile-list planet n))))

(: corner-coast? (planet integer -> Boolean))
(define (corner-coast? planet n)
  (not (zero? (remainder (count (curry tile-land? planet) (grid-corner-tile-list planet n))
                         3))))
