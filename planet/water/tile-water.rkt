#lang typed/racket

(provide (all-defined-out))

(require vraid/types
         "../terrain.rkt"
         "water-structs.rkt")

(: tile-water-depth (planet-water integer -> flonum))
(define (tile-water-depth planet n)
  (- (tile-water-level planet n)
     (tile-elevation planet n)))

(: tile-water? (planet-water integer -> Boolean))
(define (tile-water? planet n)
  (< (tile-elevation planet n) (tile-water-level planet n)))

(: tile-land? (planet-water integer -> Boolean))
(define (tile-land? planet n)
  (not (tile-water? planet n)))

(: tile-surface-friction (planet-water integer -> flonum))
(define (tile-surface-friction planet n)
  (* 0.002 (if (tile-land? planet n)
      0.000045
      0.000040)))

(: tile-porosity (planet-water integer -> flonum))
(define (tile-porosity planet n)
  0.1)
