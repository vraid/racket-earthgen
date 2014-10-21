#lang typed/racket

(provide heightmap->planet)

(require vraid/types
         vraid/math
         "planet.rkt"
         "grid/grid.rkt"
         "heightmap/heightmap-structs.rkt"
         racket/flonum)

(: natural->integer (natural -> Integer))
(define (natural->integer n)
  n)

(: integer->index (Integer -> index))
(define (integer->index n)
  (if (> 0 n)
      0
      n))

(: heightmap->planet (grid -> (heightmap FlVector -> planet)))
(define (heightmap->planet grid)
  (define empty-hash (lambda: ([n : integer])
                    (lambda: ([key :  Symbol])
                      #f)))
  (lambda: ([h : heightmap]
            [axis : FlVector])
    (define empty (lambda: ([n : index]) 0.0))
    (define void-set (lambda: ([n : index]
                               [f : Flonum])
                       (void)))
    (planet/kw
     #:grid grid
     #:has-climate? false
     #:climate-parameters (default-climate-parameters)
     #:climate-variables initial-climate-variables
     #:axis axis
     #:sea-level 0.0
     
     #:tile (let ([water-level (make-flvector (grid-tile-count grid) 0.0)])
              (tile-data
               (lambda: ([n : index])
                 (flvector-ref (heightmap-tiles h) n))
               (lambda: ([n : index])
                 (flvector-ref water-level n))
               empty empty empty empty empty empty empty empty
               (lambda: ([n : index]
                         [e : Flonum])
                 (flvector-set! (heightmap-tiles h) n e))
               (lambda: ([n : index]
                         [e : Flonum])
                 (flvector-set! water-level n e))
               void-set void-set void-set void-set void-set void-set void-set void-set))
     
     #:corner (corner-data
                    (lambda: ([n : index])
                      (flvector-ref (heightmap-corners h) n))
                    (lambda: ([n : index]) 0)
                    void-set (lambda: ([n : index]
                                       [d : Integer]) (void)))
     
     #:edge (edge-data
                  empty empty empty
                  void-set void-set void-set)
     
     #:land-ratio empty-hash
     #:population empty-hash)))
