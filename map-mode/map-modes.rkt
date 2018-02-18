#lang racket

(require "planet-color.rkt")

(provide (all-defined-out))

(struct map-mode
  (name function)
  #:transparent)

(define terrain-map-modes
  (list
   (map-mode 'topography color-topography)))

(define landscape-map-mode
  (map-mode 'landscape color-landscape))

(define climate-map-modes
  (list
   landscape-map-mode
   (map-mode 'vegetation color-leaf-area-index)
   (map-mode 'temperature color-temperature)
   (map-mode 'insolation color-insolation)
   (map-mode 'aridity color-aridity)
   (map-mode 'humidity color-humidity)
   (map-mode 'precipitation color-precipitation)))
