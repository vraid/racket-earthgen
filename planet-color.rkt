#lang racket

(require "planet.rkt"
         "color.rkt")

(provide base-color
         color-temperature)

(define water-surface
  (color 0.0 0.4 0.8))
(define water-deep
  (color 0.0 0.0 0.3))
(define land-low
  (color 0.5 0.8 0.0))
(define land-high
  (color 0.2 0.2 0.1))

(define (base-color-water depth)
      (color-interpolate
       water-surface
       water-deep
       (exact->inexact (min 1.0 (/ depth 3000.0)))))

(define (base-color-land elevation)
      (color-interpolate
       land-low
       land-high
       (exact->inexact (min 1.0 (/ elevation 3000.0)))))

(define (base-color tile)
  (if (< 0 (planet-tile-water-depth tile))
      (base-color-water (planet-tile-water-depth tile))
      (base-color-land (planet-tile-elevation tile))))

(define temperature-zero
  (color 1.0 0.0 1.0))

(define (color-temperature tile)
  temperature-zero)
  