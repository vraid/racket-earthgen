#lang racket

(require "planet.rkt"
         "grid.rkt"
         "vector3.rkt"
         "heightmap-structs.rkt"
         racket/flonum)

(provide heightmap->planet)

(define planet-radius 6371000.0)
(define (segment-area v1 v2 v3)
  (let* ([a (vector3-distance v1 v2)]
         [b (vector3-distance v1 v3)]
         [c (vector3-distance v2 v3)]
         [s (fl* 0.5 (+ a b c))])
    (fl* planet-radius
         (flsqrt
          (* s
             (- s a)
             (- s b)
             (- s c))))))
(define (tile-area grid tile)
  (foldl
   fl+
   0.0
   (map (lambda (n)
          (segment-area
           (tile-coordinates tile)
           (corner-coordinates (grid-corner grid (tile-corner tile n)))
           (corner-coordinates (grid-corner grid (tile-corner tile (+ n 1))))))
        (range 1 (tile-edge-count tile)))))

(define (heightmap->planet grid)
  (lambda (heightmap)
    (planet
     grid
     (vector-map (lambda (n)
                   (planet-tile
                    (tile-area grid (grid-tile grid n))
                    (flvector-ref (heightmap-tiles heightmap) n)
                    0.0
                    0.0
                    0.0
                    0.0))
                 (build-vector (grid-tile-count grid) identity))
     (vector-map (lambda (n)
                   (planet-corner
                    #f
                    (flvector-ref (heightmap-corners heightmap) n)))
                 (build-vector (grid-corner-count grid) identity))
     #f)))
