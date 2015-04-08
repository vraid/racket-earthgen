#lang typed/racket

(provide (all-defined-out))

(require vraid/types
         "terrain-structs.rkt")

(: tile-water-depth (planet-terrain integer -> Flonum))
(define (tile-water-depth p n)
  (- (tile-water-level p n)
     (tile-elevation p n)))

(: tile-water? (planet-terrain integer -> Boolean))
(define (tile-water? p n)
  (< (tile-elevation p n) (tile-water-level p n)))

(: tile-land? (planet-terrain integer -> Boolean))
(define (tile-land? p n)
  (not (tile-water? p n)))

(: tile-surface-friction (planet-terrain integer -> Flonum))
(define (tile-surface-friction p n)
  (* 0.002 (if (tile-land? p n)
      0.000045
      0.000040)))
