#lang typed/racket

(provide (all-defined-out))

(require "water-structs.rkt"
         "tile-water.rkt"
         "../grid.rkt")

(: corner-land? (planet-water Integer -> Boolean))
(define (corner-land? planet n)
  (not (ormap (curry tile-water? planet) (grid-corner-tile-list planet n))))

(: corner-water? (planet-water Integer -> Boolean))
(define (corner-water? planet n)
  (not (ormap (curry tile-land? planet) (grid-corner-tile-list planet n))))

(: corner-coast? (planet-water Integer -> Boolean))
(define (corner-coast? planet n)
  (not (zero? (remainder (count (curry tile-land? planet) (grid-corner-tile-list planet n))
                         3))))
