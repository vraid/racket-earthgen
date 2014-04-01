#lang typed/racket

(provide (all-defined-out))

(require "planet-structs.rkt" 
         "planet-variables.rkt"
         "planet-rotation.rkt"
         "planet-geometry.rkt"
         "math.rkt"
         "flvector3.rkt"
         math/flonum)

(define wind-speed-constant 1.0)

(: coriolis-coefficient (Flonum Flonum -> Flonum))
(define (coriolis-coefficient angular-velocity latitude)
  (* 2.0 angular-velocity latitude))

(: prevailing-wind (planet FlVector FlVector Flonum -> FlVector))
(define (prevailing-wind p
                         normal
                         pressure-gradient-force
                         friction-coefficient)
  (if (near-zero? (flvector3-length pressure-gradient-force))
      (flvector3-zero)
      (let* ([coriolis-coefficient (coriolis-coefficient (planet-angular-velocity p) (coordinate-latitude (planet-axis p) normal))]
             [perpendicular-component (flvector3-cross-product pressure-gradient-force normal)])
        (flvector3-scale (flvector3+ pressure-gradient-force
                                     (flvector3-scale perpendicular-component
                                                      (/ coriolis-coefficient friction-coefficient)))
                         wind-speed-constant))))
