#lang typed/racket

(provide (all-defined-out))

(require "climate-structs.rkt"
         "humidity.rkt"
         "sunlight.rkt")

(: tile-relative-humidity (planet-climate Integer -> Float))
(define (tile-relative-humidity planet tile)
  (relative-humidity (tile-temperature planet tile)
                     (tile-humidity planet tile)))

(: tile-aridity (planet-climate Integer -> Float))
(define (tile-aridity planet tile)
  (aridity (tile-temperature planet tile)
           (tile-humidity planet tile)))

(: tile-potential-evapotranspiration (planet-climate Integer -> Float))
(define (tile-potential-evapotranspiration planet tile)
  (potential-evapotranspiration (tile-temperature planet tile)
                                (tile-humidity planet tile)))

(: tile-snow-cover (planet-climate Integer -> Float))
(define (tile-snow-cover planet tile)
  (* 5.0 (min 0.2 (tile-snow planet tile))))

(: tile-cloud-cover (planet-climate Integer -> Float))
(define (tile-cloud-cover planet tile)
  (cloud-cover (tile-relative-humidity planet tile)))

(: tile-insolation (planet-climate Integer -> Float))
(define (tile-insolation planet tile)
  (insolation (tile-sunlight planet tile)
              (tile-cloud-cover planet tile)))
