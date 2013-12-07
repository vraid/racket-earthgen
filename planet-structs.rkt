#lang typed/racket

(require "types.rkt"
         "grid.rkt")

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
         
         set-planet-tile-temperature!
         set-planet-tile-humidity!
         set-planet-tile-precipitation!
         set-planet-edge-wind!)

(struct: planet
  ([grid : grid]
   [tiles : (Vectorof planet-tile)]
   [corners : (Vectorof planet-corner)]
   [edges : Any]))

(struct: planet-tile
  ([grid : tile]
   [area : Flonum]
   [elevation : Flonum]
   [water-level : Flonum]
   [temperature : Flonum]
   [humidity : Flonum]
   [precipitation : Flonum])
  #:mutable)

(struct: planet-corner
  ([grid : corner]
   [elevation : Flonum]
   [river-direction : (maybe index)]))

(struct: planet-edge
  ([grid : edge]
   [length : Flonum]
   [tile-distance : Positive-Flonum]
   [wind : Flonum]
   [river-flow : (maybe Flonum)])
  #:mutable)
