#lang typed/racket

(require "planet-structs.rkt"
         racket/flonum)

(provide planet-tile-water-depth
         planet-tile-water?
         planet-tile-land?
         planet-tile-snow-cover
         planet-tile-ice-cover
         planet-tile-vegetation-cover)

(: planet-tile-water-depth (planet-tile -> Flonum))
(define (planet-tile-water-depth t)
  (- (planet-tile-water-level t)
     (planet-tile-elevation t)))

(: planet-tile-water? (planet-tile -> Boolean))
(define (planet-tile-water? tile)
  (< 0.0 (planet-tile-water-depth tile)))

(: planet-tile-land? (planet-tile -> Boolean))
(define (planet-tile-land? tile)
  (not (planet-tile-water? tile)))

(: planet-tile-snow-cover (planet-tile -> Flonum))
(define (planet-tile-snow-cover tile)
  0.0)

(: planet-tile-ice-cover (planet-tile -> Flonum))
(define (planet-tile-ice-cover tile)
  0.0)

(: planet-tile-vegetation-cover (planet-tile -> Flonum))
(define (planet-tile-vegetation-cover tile)
  0.0)
