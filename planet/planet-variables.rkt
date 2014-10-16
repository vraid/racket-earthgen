#lang typed/racket

(provide (all-defined-out))

(require "planet-structs.rkt"
         math/flonum)

(define default-axis (flvector 0.0 0.0 1.0))

(: planet-radius (planet -> Flonum))
(define (planet-radius p)
  6371000.0)
