#lang typed/racket

(require "types.rkt"
         "grid.rkt"
         "planet-tile-struct.rkt")

(provide planet
         planet?
         planet-grid
         planet-tiles
         planet-corners
         planet-edges
         planet-corner
         planet-corner-river
         planet-corner-elevation
         planet-edge
         planet-edge-length
         planet-edge-tile-distance
         river-direction
         river-flow)

(struct: planet
  ([grid : grid]
   [tiles : (Vectorof planet-tile)]
   [corners : (Vectorof planet-corner)]
   [edges : Any]))

(struct: planet-corner
  ([river : (maybe river)]
   [elevation : Flonum]))

(struct: planet-edge
  ([length : Flonum]
   [tile-distance : Positive-Flonum]))

(struct: river
  ([direction : index]
   [flow : Positive-Flonum]))
