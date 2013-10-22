#lang racket

(require "planet-tile-struct.rkt"
         racket/flonum)

(provide planet-tile-water?
         planet-tile-land?
         planet-tile-snow-cover
         planet-tile-ice-cover
         planet-tile-vegetation-cover)

(define (planet-tile-water? tile)
  (< 0 (planet-tile-water-depth tile)))

(define (planet-tile-land? tile)
  (not (planet-tile-water? tile)))

(define (planet-tile-snow-cover tile) 0.0)

(define (planet-tile-ice-cover tile) 0.0)

(define (planet-tile-vegetation-cover tile) 0.0)