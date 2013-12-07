#lang typed/racket

(require "planet-structs.rkt"
         "grid.rkt"
         "grid-functions.rkt")

(provide planet-tile-tile
         planet-tile-corner
         planet-tile-edge
         planet-corner-tile
         planet-corner-corner
         planet-corner-edge
         planet-edge-tile
         planet-edge-corner)

(: planet-tile-tile (planet-tile Integer -> planet-tile))
(define (planet-tile-tile tile n)
  (vector-ref (planet-tiles (planet-tile-planet tile))
              (tile-tile (planet-tile-grid tile) n)))

(: planet-tile-corner (planet-tile Integer -> planet-corner))
(define (planet-tile-corner tile n)
  (vector-ref (planet-corners (planet-tile-planet tile))
              (tile-corner (planet-tile-grid tile) n)))

(: planet-tile-edge (planet-tile Integer -> planet-edge))
(define (planet-tile-edge tile n)
  (vector-ref (planet-edges (planet-tile-planet tile))
              (tile-edge (planet-tile-grid tile) n)))

(: planet-corner-tile (planet-corner Integer -> planet-tile))
(define (planet-corner-tile corner n)
  (vector-ref (planet-tiles (planet-corner-planet corner))
              (corner-tile (planet-corner-grid corner) n)))

(: planet-corner-corner (planet-corner Integer -> planet-corner))
(define (planet-corner-corner corner n)
  (vector-ref (planet-corners (planet-corner-planet corner))
              (corner-corner (planet-corner-grid corner) n)))

(: planet-corner-edge (planet-corner Integer -> planet-edge))
(define (planet-corner-edge corner n)
  (vector-ref (planet-edges (planet-corner-planet corner))
              (corner-edge (planet-corner-grid corner) n)))

(: planet-edge-tile (planet-edge Integer -> planet-tile))
(define (planet-edge-tile edge n)
  (vector-ref (planet-tiles (planet-edge-planet edge))
              (edge-tile (planet-edge-grid edge) n)))

(: planet-edge-corner (planet-edge Integer -> planet-corner))
(define (planet-edge-corner edge n)
  (vector-ref (planet-corners (planet-edge-planet edge))
              (edge-corner (planet-edge-grid edge) n)))
