#lang typed/racket

(provide (all-defined-out))

(require "geometry-structs.rkt"
         "time.rkt"
         vraid/math
         math/flonum)

(: planet-angular-velocity (planet-geometry -> Float))
(define (planet-angular-velocity p)
  (/ tau (planet-rotation-period p)))

(: planet-equatorial-speed (planet-geometry -> Float))
(define (planet-equatorial-speed p)
  (* (planet-radius p)
     (planet-angular-velocity p)))

(: planet-rotation-period (planet-geometry -> Float))
(define (planet-rotation-period p)
  (fl seconds-per-day))
