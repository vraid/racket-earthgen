#lang racket

(require "terrain-create.rkt"
         "math.rkt")

(provide terrain-elevation-map
         terrain-elevation-lower
         terrain-elevation-raise)

(define (terrain-elevation-map fn t)
  (terrain
   (vector-map fn (terrain-tile-elevation t))
   (vector-map fn (terrain-corner-elevation t))))

(define (terrain-elevation-lower n t)
  (terrain-elevation-map (subtract n) t))

(define (terrain-elevation-raise n t)
  (terrain-elevation-map (add n) t))
