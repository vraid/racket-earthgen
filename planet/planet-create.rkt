#lang typed/racket

(provide (all-defined-out))

(require vraid/types
         vraid/math
         "planet-structs.rkt"
         "planet-variables.rkt"
         "grid/grid.rkt"
         "heightmap/heightmap-structs.rkt"
         racket/flonum)

(: heightmap->planet (grid -> (heightmap FlVector -> planet)))
(define (heightmap->planet grid)
  (define empty-hash (lambda: ([n : integer])
                    (lambda: ([key :  Symbol])
                      #f)))
  (lambda: ([h : heightmap]
            [axis : FlVector])
    (define empty (lambda: ([n : integer]) 0.0))
    (define void-set (lambda: ([n : integer]
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
               (lambda: ([n : integer])
                 (flvector-ref (heightmap-tiles h) n))
               (lambda: ([n : integer])
                 (flvector-ref water-level n))
               empty empty empty empty empty empty empty empty
               (lambda: ([n : integer]
                         [e : Flonum])
                 (flvector-set! (heightmap-tiles h) n e))
               (lambda: ([n : integer]
                         [e : Flonum])
                 (flvector-set! water-level n e))
               void-set void-set void-set void-set void-set void-set void-set void-set))
     
     #:corner (corner-data
                    (lambda: ([n : integer])
                      (flvector-ref (heightmap-corners h) n))
                    (lambda: ([n : integer]) -1)
                    void-set (lambda: ([n : integer]
                                       [d : Integer]) (void)))
     
     #:edge (edge-data
                  empty empty
                  void-set void-set)
     
     #:rivers '()
     
     #:land-ratio empty-hash
     #:population empty-hash)))

(: empty-planet planet)
(define empty-planet
  ((heightmap->planet (n-grid 0))
   (heightmap
    (make-flvector 12 0.0)
    (make-flvector 20 0.0))
   default-axis))
