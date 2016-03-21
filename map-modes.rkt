#lang typed/racket

(provide (all-defined-out))

(require "map-mode.rkt"
         "planet-color.rkt"
         "planet/water-base.rkt"
         "planet/climate-base.rkt")

(define-map-modes terrain planet-water?
  (topography color-topography))

(define-map-modes climate planet-climate?
  (landscape color-landscape)
  (vegetation color-leaf-area-index)
  (temperature color-temperature)
  (insolation color-insolation)
  (aridity color-aridity)
  (humidity color-humidity)
  (precipitation color-precipitation))
