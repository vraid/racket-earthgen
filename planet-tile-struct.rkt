#lang typed/racket

(require racket/flonum
         "types.rkt")

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
  ([id : index]
   [area : Flonum]
   [elevation : Flonum]
   [water-level : Flonum]
   [temperature : Flonum]
   [humidity : Flonum]
   [precipitation : Flonum]))

(: planet-tile-water-depth (planet-tile -> Flonum))
(define (planet-tile-water-depth t)
    (fl- (planet-tile-water-level t)
         (planet-tile-elevation t)))