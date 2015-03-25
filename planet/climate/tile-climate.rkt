#lang typed/racket

(provide (all-defined-out))

(require vraid/types
         "climate-structs.rkt"
         "humidity.rkt"
         "sunlight.rkt")

(: tile-relative-humidity (planet-climate integer -> flonum))
(define (tile-relative-humidity planet tile)
  (relative-humidity (tile-temperature planet tile)
                     (tile-humidity planet tile)))

(: tile-aridity (planet-climate integer -> flonum))
(define (tile-aridity planet tile)
  (aridity (tile-temperature planet tile)
           (tile-humidity planet tile)))

(: tile-potential-evapotranspiration (planet-climate integer -> flonum))
(define (tile-potential-evapotranspiration planet tile)
  (potential-evapotranspiration (tile-temperature planet tile)
                                (tile-humidity planet tile)))

(: tile-cloud-cover (planet-climate integer -> flonum))
(define (tile-cloud-cover planet tile)
  (cloud-cover (tile-relative-humidity planet tile)))

(: tile-insolation (planet-climate integer -> flonum))
(define (tile-insolation planet tile)
  (insolation (tile-sunlight planet tile)
              (tile-cloud-cover planet tile)))
