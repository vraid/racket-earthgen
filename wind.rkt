#lang typed/racket

(provide
 (struct-out wind))

(require "flvector2.rkt"
         math/flonum)

(struct: wind
  ([direction : Flonum]
   [speed : Flonum]))

(struct: force
  ([direction : Flonum]
   [magnitude : Flonum]))

(: wind/coriolis (force Flonum Flonum -> wind))
(define (wind/coriolis force friction coriolis-effect)
  (wind 0.0 0.0))

(: prevailing-wind (force Flonum Flonum -> wind))
(define (prevailing-wind pressure-gradient-force coriolis-coefficient friction-coefficient)
  (wind
   (+ (force-direction pressure-gradient-force) 0)
   (/ (force-magnitude pressure-gradient-force)
      (flvector2-length-squared (flvector coriolis-coefficient
                                          friction-coefficient))))) 

(: coriolis-effect (Flonum Flonum -> Flonum))
(define (coriolis-effect coriolis-coefficient latitude)
  0.0)

(: coriolis-coefficient (Flonum Flonum -> Flonum))
(define (coriolis-coefficient angular-velocity latitude)
  (* 2.0 angular-velocity latitude))
  