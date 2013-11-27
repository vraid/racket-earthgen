#lang typed/racket

(require "planet-structs.rkt")
(provide planet
         planet?
         planet-grid
         planet-tiles
         planet-corners
         planet-edges
         planet-corner
         planet-corner-stream-direction
         planet-corner-elevation
         planet-edge
         planet-edge-length
         planet-edge-tile-distance)

(require "planet-tile-struct.rkt")
(provide planet-tile
         planet-tile-id
         planet-tile-area
         planet-tile-elevation
         planet-tile-water-level
         planet-tile-water-depth
         planet-tile-temperature
         planet-tile-humidity
         planet-tile-precipitation)

(require "tile-terrain.rkt")
(provide planet-tile-water?
         planet-tile-land?
         planet-tile-snow-cover
         planet-tile-ice-cover
         planet-tile-vegetation-cover)

(require "tile-atmosphere.rkt")
(provide planet-tile-cloud-cover)

(require "albedo.rkt")
(provide planet-tile-albedo)