#lang typed/racket

(provide planet-tile
         planet-tile-area
         planet-tile-elevation
         planet-tile-water-level
         planet-tile-temperature
         planet-tile-humidity
         planet-tile-precipitation)

(struct: planet-tile
  ([area : Flonum]
   [elevation : Flonum]
   [water-level : Flonum]
   [temperature : Flonum]
   [humidity : Flonum]
   [precipitation : Flonum]))
