#lang racket

(require "terrain-structs.rkt"
         "terrain-create.rkt"
         "math.rkt"
         math/flonum)

(provide terrain-elevation-map
         terrain-elevation-lower
         terrain-elevation-raise)  

(define (terrain-elevation-map fn t)
  (terrain
   (flvector-map fn (terrain-tile-elevations t))
   (flvector-map fn (terrain-corner-elevations t))
   #f))

(define (terrain-elevation-lower n t)
  (terrain-elevation-map (lambda (t)
                           (fl- t (exact->inexact n)))
                         t))

(define (terrain-elevation-raise n t)
  (terrain-elevation-map (lambda (t)
                           (fl+ t (exact->inexact n)))
                         t))
