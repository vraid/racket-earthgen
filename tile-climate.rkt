#lang typed/racket

(provide (all-defined-out))

(require "planet-structs.rkt"
         "humidity.rkt")

(: tile-relative-humidity (planet Integer -> Flonum))
(define (tile-relative-humidity p n)
  (relative-humidity
   (tile-temperature p n)
   (tile-humidity p n)))
