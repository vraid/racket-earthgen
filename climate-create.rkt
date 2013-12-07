#lang typed/racket

(require math/flonum
         "planet.rkt"
         "grid.rkt"
         "vector3.rkt"
         "parallel-util.rkt")

(provide climate-first
         climate-next)

(: temperature-first (flvector3 -> Flonum))
(define (temperature-first v)
  (if (flvector? v)
      (let ([light (flsqrt (fl- 1.0 (flexpt (flvector-ref v 2) 2.0)))])
        (fl* light 340.0))
      0.0))

(: climate-first (planet -> planet))
(define (climate-first prev)
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

(: climate-next (planet -> planet))
(define (climate-next prev)
  (define p
    (planet
     (planet-grid prev)
     (planet-tiles prev)
     (planet-corners prev)
     (vector)))
  (begin
    p))
