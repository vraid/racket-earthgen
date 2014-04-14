#lang typed/racket

(provide (all-defined-out))

(require "types.rkt"
         "planet-structs.rkt")

(: tile-water-depth (planet index -> Flonum))
(define (tile-water-depth p n)
  (- (tile-water-level p n)
     (tile-elevation p n)))

(: tile-water? (planet index -> Boolean))
(define (tile-water? p n)
  (< 0.0 (tile-water-depth p n)))

(: tile-land? (planet index -> Boolean))
(define (tile-land? p n)
  (not (tile-water? p n)))

(: tile-snow-cover (planet index -> Flonum))
(define (tile-snow-cover p n)
  0.0)

(: tile-ice-cover (planet index -> Flonum))
(define (tile-ice-cover p n)
  0.0)

(: tile-vegetation-cover (planet index -> Flonum))
(define (tile-vegetation-cover p n)
  0.0)

(: tile-surface-friction (planet index -> Flonum))
(define (tile-surface-friction p n)
  (* 0.002 (if (tile-land? p n)
      0.000045
      0.000040)))
