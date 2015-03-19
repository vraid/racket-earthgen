#lang typed/racket

(provide (all-defined-out))

(require vraid/types
         "terrain-structs.rkt"
         "tile-terrain.rkt"
         "rivers.rkt"
         "../grid.rkt")

(: edge-coast? (planet-terrain integer -> Boolean))
(define (edge-coast? planet n)
  (let ([tiles (grid-edge-tile-list planet n)])
    (= 1 (count (curry tile-land? planet) tiles))))

(: edge-land? (planet-terrain integer -> Boolean))
(define (edge-land? planet n)
  (andmap (curry tile-land? planet)
          (grid-edge-tile-list planet n)))

(: edge-has-river? (planet-terrain integer -> Boolean))
(define (edge-has-river? planet n)
  (and (edge-land? planet n)
       (let ([a ((grid-edge-corner planet) n 0)]
             [b ((grid-edge-corner planet) n 1)])
         (or ((river-flows-to? planet a) b)
             ((river-flows-to? planet b) a)))))
