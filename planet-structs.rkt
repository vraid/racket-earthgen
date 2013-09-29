#lang typed/racket

(provide planet
         planet-grid
         planet-tiles
         planet-corners
         planet-edges
         planet-corner
         planet-corner-stream-direction
         planet-corner-elevation
         planet-edge
         planet-edge-length)

(struct: planet
  ([grid : Any]
   [tiles : Any]
   [corners : Any]
   [edges : Any]))

(struct: planet-corner
  ([stream-direction : (U Boolean Fixnum)]
   [elevation : Flonum]))

(struct: planet-edge
  ([length : Flonum]))
