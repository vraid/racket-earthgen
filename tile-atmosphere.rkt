#lang typed/racket

(require "planet-structs.rkt"
         racket/flonum)

(provide planet-tile-cloud-cover)

(: planet-tile-cloud-cover (planet-tile -> Flonum))
(define (planet-tile-cloud-cover tile) 0.0)
