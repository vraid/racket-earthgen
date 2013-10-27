#lang racket

(require math/flonum
         "planet.rkt"
         "grid.rkt"
         "vector3.rkt"
         "parallel-util.rkt")

(provide climate-first
         climate-next)

(define (temperature-first v)
  (if (flvector? v)
      (let ([light (flsqrt (fl- 1.0 (flexpt (flvector-ref v 2) 2.0)))])
        (fl* light 340.0))
      0.0))

(define (climate-first p grid)
  (if (planet? p)
      (planet
       (planet-grid p)
       (vector-map!-parallel (lambda (t)
                     (let ([c (tile-coordinates (grid-tile grid (planet-tile-id t)))])
                       (planet-tile
                        (planet-tile-id t)
                        (planet-tile-area t)
                        (planet-tile-elevation t)
                        (planet-tile-water-level t)
                        (temperature-first c)
                        (planet-tile-humidity t)
                        (planet-tile-precipitation t))))
                   (planet-tiles p))
       (planet-corners p)
       (planet-edges p))
      p))

(define (climate-next p grid)
  (begin
    (vector-map!-parallel identity
                          (planet-tiles p))
    p))