#lang typed/racket

(require racket/flonum)

(provide planet-tile
         planet-tile-id
         planet-tile-area
         planet-tile-elevation
         planet-tile-water-level
         planet-tile-water-depth
         planet-tile-temperature
         planet-tile-humidity
         planet-tile-precipitation)

(struct: planet-tile
  ([id : Fixnum]
   [area : Flonum]
   [elevation : Flonum]
   [water-level : Flonum]
   [temperature : Flonum]
   [humidity : Flonum]
   [precipitation : Flonum]))

(define planet-tile-water-depth
  (lambda: ([t : planet-tile])
    (fl- (planet-tile-water-level t)
         (planet-tile-elevation t))))