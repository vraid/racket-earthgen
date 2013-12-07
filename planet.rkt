#lang typed/racket

(require "planet-structs.rkt")
(provide planet
         planet?
         planet-grid
         planet-tiles
         planet-corners
         planet-edges
         planet-tile
         planet-tile-area
         planet-tile-elevation
         planet-tile-water-level
         planet-tile-temperature
         planet-tile-humidity
         planet-tile-precipitation
         planet-corner
         planet-corner-elevation
         planet-corner-river-direction
         planet-edge
         planet-edge-length
         planet-edge-tile-distance
         planet-edge-wind
         planet-edge-river-flow
         river-direction
         river-flow)

(require "tile-terrain.rkt")
(provide planet-tile-water-depth
         planet-tile-water?
         planet-tile-land?
         planet-tile-snow-cover
         planet-tile-ice-cover
         planet-tile-vegetation-cover)

(require "tile-atmosphere.rkt")
(provide planet-tile-cloud-cover)

(require "albedo.rkt")
(provide planet-tile-albedo)