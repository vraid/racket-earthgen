#lang typed/racket

(require "geometry-structs.rkt"
         "time.rkt"
         math/flonum)

(provide (all-defined-out))

(: planet-angular-velocity (planet-geometry -> Float))
(define (planet-angular-velocity p)
  (/ (* 2 pi) (planet-rotation-period p)))

(: planet-equatorial-speed (planet-geometry -> Float))
(define (planet-equatorial-speed p)
  (* (planet-radius p)
     (planet-angular-velocity p)))

(: planet-rotation-period (planet-geometry -> Float))
(define (planet-rotation-period p)
  (fl seconds-per-day))
