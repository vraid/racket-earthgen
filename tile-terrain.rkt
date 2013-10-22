#lang typed/racket

(require "planet-tile-struct.rkt"
         racket/flonum)

(provide planet-tile-water?
         planet-tile-land?
         planet-tile-snow-cover
         planet-tile-ice-cover
         planet-tile-vegetation-cover)

(define planet-tile-water?
  (lambda: ([tile : planet-tile])
    (fl< 0.0 (planet-tile-water-depth tile))))

(define planet-tile-land?
  (lambda: ([tile : planet-tile])
    (not (planet-tile-water? tile))))

(define planet-tile-snow-cover
  (lambda: ([tile : planet-tile])
    0.0))

(define planet-tile-ice-cover
  (lambda: ([tile : planet-tile])
    0.0))

(define planet-tile-vegetation-cover
  (lambda: ([tile : planet-tile])
    0.0))