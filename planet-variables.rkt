#lang typed/racket

(provide (all-defined-out))

(require "planet-structs.rkt"
         "flvector3.rkt"
         math/flonum)

(: planet-axis (planet -> flvector3))
(define (planet-axis p)
  (flvector 0.0 0.0 1.0))

(: planet-radius (planet -> Flonum))
(define (planet-radius p)
  6371000.0)
