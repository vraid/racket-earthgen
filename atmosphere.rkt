#lang typed/racket

(require "types.rkt"
         "planet-structs.rkt"
         racket/flonum)

(provide (all-defined-out))

(: tile-cloud-cover (planet index -> Flonum))
(define (tile-cloud-cover p n)
  0.0)
