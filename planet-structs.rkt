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
         planet-tile-planet
         planet-tile-grid
         planet-tile-area
         planet-tile-elevation
         planet-tile-water-level
         planet-tile-temperature
         planet-tile-humidity
         planet-tile-precipitation
         planet-corner
         planet-corner-planet
         planet-corner-grid
         planet-corner-elevation
         planet-corner-river-direction
         planet-edge
         planet-edge-planet
         planet-edge-grid
         planet-edge-length
         planet-edge-tile-distance
         planet-edge-wind
         planet-edge-river-flow)
         
(provide set-planet-tiles!
         set-planet-corners!
         set-planet-edges!
         set-planet-tile-temperature!
         set-planet-tile-humidity!
         set-planet-tile-precipitation!
         set-planet-edge-wind!)

(struct: planet
  ([grid : grid]
   [tiles : (Vectorof planet-tile)]
   [corners : (Vectorof planet-corner)]
   [edges : (Vectorof planet-edge)])
  #:mutable)

(struct: planet-tile
  ([planet : planet]
   [grid : tile]
   [area : Flonum]
   [elevation : Flonum]
   [water-level : Flonum]
   [temperature : Flonum]
   [humidity : Flonum]
   [precipitation : Flonum])
  #:mutable)

(struct: planet-corner
  ([planet : planet]
   [grid : corner]
   [elevation : Flonum]
   [river-direction : (maybe index)]))

(struct: planet-edge
  ([planet : planet]
   [grid : edge]
   [length : Flonum]
   [tile-distance : Positive-Flonum]
   [wind : Flonum]
   [river-flow : (maybe Flonum)])
  #:mutable)
