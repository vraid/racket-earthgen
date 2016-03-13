#lang typed/racket

(provide (all-defined-out))

(require "water-structs.rkt"
         "tile-water.rkt"
         "rivers.rkt"
         "../grid-base.rkt")

(: edge-coast? (planet-water Integer -> Boolean))
(define (edge-coast? planet n)
  (let ([tiles (grid-edge-tile-list planet n)])
    (= 1 (count (curry tile-land? planet) tiles))))

(: edge-land? (planet-water Integer -> Boolean))
(define (edge-land? planet n)
  (andmap (curry tile-land? planet)
          (grid-edge-tile-list planet n)))

(: edge-has-river? (planet-water Integer -> Boolean))
(define (edge-has-river? planet n)
  (and (edge-land? planet n)
       (let ([a ((grid-edge-corner planet) n 0)]
             [b ((grid-edge-corner planet) n 1)])
         (or ((river-flows-to? planet a) b)
             ((river-flows-to? planet b) a)))))
