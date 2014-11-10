#lang typed/racket

(provide (all-defined-out))

(require "../planet-structs.rkt"
         "../planet-variables.rkt"
         "time.rkt"
         vraid/math
         math/flonum)

(: planet-angular-velocity (planet -> Flonum))
(define (planet-angular-velocity p)
  (/ tau (planet-rotation-period p)))

(: planet-equatorial-speed (planet -> Flonum))
(define (planet-equatorial-speed p)
  (* (planet-radius p)
     (planet-angular-velocity p)))

(: planet-rotation-period (planet -> Flonum))
(define (planet-rotation-period p)
  (fl seconds-per-day))
