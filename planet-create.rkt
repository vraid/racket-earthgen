#lang typed/racket

(require "types.rkt"
         "planet.rkt"
         "grid.rkt"
         "vector3.rkt"
         "heightmap-structs.rkt"
         racket/flonum)

(provide heightmap->planet)

(: planet-radius Flonum)
(define planet-radius 6371000.0)

(: segment-area (flvector3 flvector3 flvector3 -> Flonum))
(define (segment-area v1 v2 v3)
  (let* ([a (flvector3-distance v1 v2)]
         [b (flvector3-distance v1 v3)]
         [c (flvector3-distance v2 v3)]
         [s (fl* 0.5 (+ a b c))])
    (fl* planet-radius
         (flsqrt
          (* s
             (- s a)
             (- s b)
             (- s c))))))

(: tile-area (grid tile -> Flonum))
(define (tile-area grid tile)
  (foldl
   fl+
   0.0
   (map (lambda: ([n : Integer])
          (segment-area
           (tile-coordinates tile)
           (corner-coordinates (grid-corner grid (tile-corner tile n)))
           (corner-coordinates (grid-corner grid (tile-corner tile (+ n 1))))))
        (range 1 (tile-edge-count tile)))))

(: natural->integer (natural -> Integer))
(define (natural->integer n)
  n)

(: integer->index (Integer -> index))
(define (integer->index n)
  (if (> 0 n)
      0
      n))

(: heightmap->planet (grid -> (heightmap -> planet)))
(define (heightmap->planet grid)
  (lambda: ([h : heightmap])
    (planet
     grid
     (build-vector (natural->integer (grid-tile-count grid))
                   (lambda: ([n : index])
                     (planet-tile
                      n
                      (tile-area grid (grid-tile grid n))
                      (flvector-ref (heightmap-tiles h) n)
                      0.0
                      0.0
                      0.0
                      0.0)))
     (build-vector (natural->integer (grid-corner-count grid))
                   (lambda: ([n : index])
                     (planet-corner
                      #f
                      (flvector-ref (heightmap-corners h) n))))
     #f)))
