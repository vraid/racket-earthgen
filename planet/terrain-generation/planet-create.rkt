#lang typed/racket

(require vraid/math
         vraid/util
         math/flonum
         "terrain-data.rkt"
         "../grid-base.rkt"
         "../heightmap.rkt"
         "../geometry.rkt"
         "../terrain-base.rkt")

(provide (all-defined-out))

(struct grid/heightmap
  ([grid : grid]
   [heightmap : heightmap]))

(: planet/sea-level (Float -> (planet-terrain -> planet-terrain)))
(define ((planet/sea-level sea-level) p)
  (let* ([tile-count (tile-count p)]
         [corner-count [corner-count p]]
         [tile-data (make-tile-terrain-data tile-count
                                            (curry tile-elevation p)
                                            (λ ([n : Integer])
                                              sea-level))]
         [corner-data (make-corner-terrain-data corner-count
                                                (curry corner-elevation p)
                                                (curry corner-river-direction p))])
    (planet-terrain/kw
     #:planet-geometry p
     #:sea-level sea-level
     #:tile tile-data
     #:corner corner-data
     #:rivers '())))

(: copy-planet-geography (planet-terrain -> planet-terrain))
(define (copy-planet-geography p)
  (planet-terrain/kw
   #:planet-geometry p
   #:sea-level (planet-sea-level p)
   #:tile (make-tile-terrain-data (tile-count p)
                                  (curry tile-elevation p)
                                  (curry tile-water-level p))
   #:corner (make-corner-terrain-data (corner-count p)
                                      (curry corner-elevation p)
                                      (curry corner-river-direction p))
   #:rivers (planet-rivers p)))

(: heightmap->planet (Float FlVector -> (grid/heightmap -> planet-terrain)))
(define ((heightmap->planet radius axis) gh)
  (let* ([grid (grid/heightmap-grid gh)]
         [h (grid/heightmap-heightmap gh)]
         [tile (make-tile-terrain-data (tile-count grid)
                                       (curry flvector-ref (heightmap-tiles h))
                                       (λ ([n : Integer]) 0.0))]
         [corner (make-corner-terrain-data (corner-count grid)
                                           (curry flvector-ref (heightmap-corners h))
                                           (λ ([n : Integer]) #f))])
    
    (planet-terrain/kw
     #:planet-geometry (planet-geometry/kw
                        #:grid grid
                        #:axis (flvector3-normal axis)
                        #:radius radius
                        #:tile (tile-geometry-data
                                (build-flvector-ref (tile-count grid)
                                                    (λ ([n : Integer])
                                                      (n-gon-area radius grid n)))))
     #:sea-level 0.0
     #:tile tile
     #:corner corner
     #:rivers '())))

(: empty-planet (grid -> planet-terrain))
(define (empty-planet grid)
  ((heightmap->planet default-radius default-axis)
   (grid/heightmap
    grid
    (heightmap
     (make-flvector (grid-tile-count grid) 0.0)
     (make-flvector (grid-corner-count grid) 0.0)))))
