#lang typed/racket

(require math/flonum
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
  (define p
    (planet
     (planet-grid prev)
     (vector)
     (planet-corners prev)
     (planet-edges prev)))
  (begin
    (set-planet-tiles!
     p
     (vector-map
      (lambda: ([t : planet-tile])
        (let ([c (tile-coordinates (planet-tile-grid t))])
          (planet-tile
           p
           (planet-tile-grid t)
           (planet-tile-area t)
           (planet-tile-elevation t)
           (planet-tile-water-level t)
           (temperature-first c)
           (planet-tile-humidity t)
           (planet-tile-precipitation t))))
      (planet-tiles prev)))
    p))

(: climate-next (climate-parameters planet -> planet))
(define (climate-next par prev)
  (define p
    (planet
     (planet-grid prev)
     (vector)
     (vector)
     (vector)))
  (begin
    p))
