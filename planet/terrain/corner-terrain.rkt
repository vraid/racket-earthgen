#lang typed/racket

(require "terrain-structs.rkt"
         "tile-terrain.rkt"
         "../grid-base.rkt")

(provide (all-defined-out))

(: corner-land? (planet-terrain Integer -> Boolean))
(define (corner-land? planet n)
  (not (ormap (curry tile-water? planet) (grid-corner-tile-list planet n))))

(: corner-water? (planet-terrain Integer -> Boolean))
(define (corner-water? planet n)
  (not (ormap (curry tile-land? planet) (grid-corner-tile-list planet n))))

(: corner-coast? (planet-terrain Integer -> Boolean))
(define (corner-coast? planet n)
  (not (zero? (remainder (count (curry tile-land? planet) (grid-corner-tile-list planet n))
                         3))))
