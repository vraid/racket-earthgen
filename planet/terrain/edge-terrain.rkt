#lang typed/racket

(require "terrain-structs.rkt"
         "tile-terrain.rkt"
         "rivers.rkt"
         "../grid-base.rkt")

(provide (all-defined-out))

(: edge-coast? (planet-terrain Integer -> Boolean))
(define (edge-coast? planet n)
  (let ([tiles (grid-edge-tile-list planet n)])
    (= 1 (count (curry tile-land? planet) tiles))))

(: edge-land? (planet-terrain Integer -> Boolean))
(define (edge-land? planet n)
  (andmap (curry tile-land? planet)
          (grid-edge-tile-list planet n)))

(: edge-has-river? (planet-terrain Integer -> Boolean))
(define (edge-has-river? planet n)
  (and (edge-land? planet n)
       (let ([a ((grid-edge-corner planet) n 0)]
             [b ((grid-edge-corner planet) n 1)]
             [flows-to? (curry river-flows-to? planet)])
         (or ((flows-to? a) b)
             ((flows-to? b) a)))))
