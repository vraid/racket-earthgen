#lang typed/racket

(require "terrain-structs.rkt")

(provide (all-defined-out))

(: tile-water-depth (planet-terrain Integer -> Float))
(define (tile-water-depth planet n)
  (- (tile-water-level planet n)
     (tile-elevation planet n)))

(: tile-water? (planet-terrain Integer -> Boolean))
(define (tile-water? planet n)
  (< (tile-elevation planet n) (tile-water-level planet n)))

(: tile-land? (planet-terrain Integer -> Boolean))
(define (tile-land? planet n)
  (not (tile-water? planet n)))

(: tile-surface-friction (planet-terrain Integer -> Float))
(define (tile-surface-friction planet n)
  (* 0.002
     (if (tile-water? planet n)
         0.000040
         0.000045)))

(: tile-porosity (planet-terrain Integer -> Float))
(define (tile-porosity planet n)
  0.1)
