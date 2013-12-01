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
         planet-corner-stream-direction
         planet-corner-elevation
         planet-edge
         planet-edge-length
         planet-edge-tile-distance)

(struct: planet
  ([grid : grid]
   [tiles : (Vectorof planet-tile)]
   [corners : (Vectorof planet-corner)]
   [edges : Any]))

(struct: planet-corner
  ([stream-direction : (maybe index)]
   [elevation : Flonum]))

(struct: planet-edge
  ([length : Flonum]
   [tile-distance : Flonum]))
