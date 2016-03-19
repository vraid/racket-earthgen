#lang typed/racket

(provide (all-defined-out))

(require "geometry-structs.rkt"
         "planet-rotation.rkt"
         "planet-geometry.rkt"
         vraid/math)

(define wind-speed-constant 4.0e3)

(: coriolis-coefficient (Float Float -> Float))
(define (coriolis-coefficient angular-velocity latitude)
  (* 2.0 angular-velocity latitude))

(: prevailing-wind (planet-geometry FlVector FlVector Float -> FlVector))
(define (prevailing-wind p normal pressure-gradient-force friction-coefficient)
  (if (flvector3-zero? pressure-gradient-force)
      (flvector3-zero)
      (let* ([coriolis-coefficient (coriolis-coefficient (planet-angular-velocity p)
                                                         (coordinate-latitude (planet-axis p) normal))]
             [perpendicular-component (flvector3-cross-product normal pressure-gradient-force)])
        (flvector3-scale (product wind-speed-constant
                                  (flvector3-length pressure-gradient-force))
                         (flvector3-sum (flvector3-normal pressure-gradient-force)
                                        (flvector3-scale (divide friction-coefficient coriolis-coefficient)
                                                         perpendicular-component))))))
