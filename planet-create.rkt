#lang typed/racket

(provide heightmap->planet)

(require "types.rkt"
         "planet.rkt"
         "grid.rkt"
         "flvector3.rkt"
         "heightmap-structs.rkt"
         racket/flonum)

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
    (planet/kw
     #:grid grid
     #:has-climate? false
     #:climate-variables initial-climate-variables
     
     #:tile (tile-data 
                  (lambda: ([n : index])
                    (flvector-ref (heightmap-tiles h) n))
                  empty empty empty empty empty empty empty empty
                  void-set void-set void-set void-set void-set void-set void-set void-set void-set)
     
     #:corner (corner-data
                    (lambda: ([n : index])
                      (flvector-ref (heightmap-corners h) n))
                    (lambda: ([n : index]) 0)
                    void-set (lambda: ([n : index]
                                       [d : Integer]) (void)))
     
     #:edge (edge-data
                  (lambda: ([n : index]) #f)
                  empty empty empty
                  (lambda: ([n : index]
                            [b : Boolean]) (void))
                  void-set void-set void-set))))
