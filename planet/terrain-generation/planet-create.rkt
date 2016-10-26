
#lang typed/racket

(provide (all-defined-out))

(require vraid/math
         vraid/util
         "../grid-base.rkt"
         "../heightmap-base.rkt"
         "../geometry.rkt"
         "../terrain-base.rkt"
         "../water-base.rkt"
         racket/flonum)

(: init-array (Integer -> ((Integer Float -> Void) (Integer -> Float) -> Void)))
(define ((init-array count) set get)
  (for ([n count])
    (set n (get n))))

(define tile-init
  (lambda ([grid : grid])
    (init-array (tile-count grid))))

(define corner-init
  (lambda ([grid : grid])
    (init-array (corner-count grid))))

(define edge-init
  (lambda ([grid : grid])
    (init-array (edge-count grid))))

(struct (A) get/set
  ([get : (Integer -> A)]
   [set : (Integer A -> Void)]))

(: get/set-flvector (Integer -> (get/set Flonum)))
(define (get/set-flvector count)
  (let ([v (make-flvector count 0.0)])
    (get/set (lambda ([n : Integer]) (flvector-ref v n))
             (lambda ([n : Integer]
                      [value : Float]) (flvector-set! v n value)))))

(: make-tile-terrain-data (Integer -> tile-terrain-data))
(define (make-tile-terrain-data n)
  (let ([elevation (get/set-flvector n)])
    (tile-terrain-data/kw #:elevation (get/set-get elevation)
                          #:elevation-set! (get/set-set elevation))))

(: make-corner-terrain-data (Integer -> corner-terrain-data))
(define (make-corner-terrain-data n)
  (let ([elevation (get/set-flvector n)])
    (corner-terrain-data/kw #:elevation (get/set-get elevation)
                            #:elevation-set! (get/set-set elevation))))

(: planet/sea-level (Float planet-terrain -> planet-water))
(define (planet/sea-level sea-level p)
  (let* ([void-fl-set! (lambda ([n : Integer]
                                [v : Float])
                         (void))]
         [void-int-set! (lambda ([n : Integer]
                                 [v : Integer])
                          (void))])
    (planet-water/kw
     #:planet-terrain p
     #:sea-level sea-level
     #:tile (tile-water-data (lambda ([n : Integer]) sea-level)
                             void-fl-set!)
     #:corner (corner-water-data (lambda ([n : Integer]) -1)
                                 (lambda ([n : Integer]
                                          [a : (Option Integer)])
                                   (void)))
     #:rivers '())))

(: copy-planet-geography (planet-terrain -> planet-terrain))
(define (copy-planet-geography p)
  (let ([tiles (make-tile-terrain-data (tile-count p))]
        [corners (make-corner-terrain-data (corner-count p))])
    ((tile-init p) (tile-terrain-data-elevation-set! tiles)
                   (curry tile-elevation p))
    ((corner-init p) (corner-terrain-data-elevation-set! corners)
                     (curry corner-elevation p))
    (planet-terrain/kw
     #:planet-geometry p
     #:tile tiles
     #:corner corners)))

(: heightmap->planet (grid -> (heightmap Float FlVector -> planet-terrain)))
(define (heightmap->planet grid)
  (define empty-hash (lambda ([n : Integer])
                       (lambda ([key :  Symbol])
                         #f)))
  (lambda ([h : heightmap]
           [radius : Float]
           [axis : FlVector])
    (define zero (lambda ([n : Integer]) 0.0))
    (define void-set (lambda ([n : Integer]
                              [f : Float])
                       (void)))
    (define tile (let ([v (flvector-copy (heightmap-tiles h))])
                   (tile-terrain-data
                    (lambda ([n : Integer])
                      (flvector-ref v n))
                    (lambda ([n : Integer]
                             [value : Float])
                      (flvector-set! v n value)))))
    
    (define corner (let* ([corner-count (corner-count grid)]
                          [corner (make-corner-terrain-data corner-count)]
                          [init-corner-array (init-array corner-count)])
                     (init-corner-array (corner-terrain-data-elevation-set! corner)
                                        (curry flvector-ref (heightmap-corners h)))
                     corner))
    
    (planet-terrain/kw
     #:planet-geometry (planet-geometry/kw
                        #:grid grid
                        #:axis (flvector3-normal axis)
                        #:radius radius
                        #:tile (tile-geometry-data
                                (build-flvector-ref (tile-count grid)
                                                    (lambda ([n : Integer])
                                                      (n-gon-area radius grid n)))))
     #:tile tile
     #:corner corner)))

(: empty-planet (grid -> planet-terrain))
(define (empty-planet grid)
  ((heightmap->planet grid)
   (heightmap
    (make-flvector (grid-tile-count grid) 0.0)
    (make-flvector (grid-corner-count grid) 0.0))
   default-radius
   default-axis))
