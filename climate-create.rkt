#lang typed/racket

(require math/flonum
         "types.rkt"
         "planet.rkt"
         "grid.rkt"
         "climate-structs.rkt"
         "vector3.rkt"
         "parallel-util.rkt")

(provide climate-first
         climate-next
         climate-parameters)

(: temperature-first (flvector3 -> Flonum))
(define (temperature-first v)
  (let ([light (flsqrt (fl- 1.0 (flexpt (flvector-ref v 2) 2.0)))])
    (fl* light 340.0))
  0.0)

(: climate-first (climate-parameters planet -> planet))
(define (climate-first par prev)
  prev)

(: climate-next (climate-parameters planet -> planet))
(define (climate-next par prev)
  prev)
