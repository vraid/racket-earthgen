#lang typed/racket

(provide heightmap->planet)

(require "types.rkt"
         "planet.rkt"
         "grid.rkt"
         "vector3.rkt"
         "heightmap-structs.rkt"
         racket/flonum)

(: segment-area (Flonum flvector3 flvector3 flvector3 -> Flonum))
(define (segment-area radius v1 v2 v3)
  (let* ([a (flvector3-distance v1 v2)]
         [b (flvector3-distance v1 v3)]
         [c (flvector3-distance v2 v3)]
         [s (fl* 0.5 (+ a b c))])
    (fl* radius
         (flsqrt
          (* s
             (- s a)
             (- s b)
             (- s c))))))

(: tile-area (planet index -> Flonum))
(define (tile-area p t)
  (foldl
   fl+
   0.0
   (map (lambda: ([n : Integer])
          (segment-area
           (planet-radius p)
           (tile-coordinates p t)
           (corner-coordinates p (tile-corner p t n))
           (corner-coordinates p (tile-corner p t (+ n 1)))))
        (range 1 (tile-edge-count t)))))

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
    (define empty (lambda: ([n : index]) 0.0))
    (define void-set (lambda: ([n : index]
                               [f : Flonum])
                       (void)))
    (planet
     grid
     false
     
     (tile-data 
      (lambda: ([n : index])
        (flvector-ref (heightmap-tiles h) n))
      empty empty empty empty empty empty
      void-set void-set void-set void-set void-set void-set void-set)
     
     (corner-data
      (lambda: ([n : index])
        (flvector-ref (heightmap-corners h) n))
      (lambda: ([n : index]) 0)
      void-set (lambda: ([n : index]
                         [d : Integer]) (void)))
     
     (edge-data
      (lambda: ([n : index]) #f)
      empty empty empty
      (lambda: ([n : index]
                [b : Boolean]) (void))
      void-set void-set void-set))))
