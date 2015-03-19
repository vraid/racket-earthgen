#lang typed/racket

(provide (all-defined-out))

(require vraid/types
         vraid/math
         vraid/typed-array
         "../grid.rkt"
         "../geometry.rkt"
         "../terrain.rkt"
         "../heightmap.rkt"
         racket/flonum)

(: copy-planet-geography (planet-terrain -> planet-terrain))
(define (copy-planet-geography p)
  (let* ([tile-count (tile-count p)]
         [corner-count (corner-count p)]
         [init-tile-array (init-array tile-count)]
         [init-corner-array (init-array corner-count)]
         [tiles (make-tile-terrain-data tile-count)]
         [corners (make-corner-terrain-data corner-count)])
    (init-tile-array (tile-terrain-data-elevation-set! tiles)
                     (curry tile-elevation p))
    (init-corner-array (corner-terrain-data-elevation-set! corners)
                       (curry corner-elevation p))
    (init-corner-array (corner-terrain-data-river-direction-set! corners)
                       (corner-terrain-data-river-direction (planet-terrain-corner p)))
    (planet-terrain/kw
     #:planet-geometry p
     #:sea-level (planet-sea-level p)
     #:tile tiles
     #:corner corners
     #:rivers (planet-rivers p))))

(: heightmap->planet (grid -> (heightmap FlVector -> planet-terrain)))
(define (heightmap->planet grid)
  (define empty-hash (lambda ([n : integer])
                       (lambda ([key :  Symbol])
                         #f)))
  (lambda ([h : heightmap]
           [axis : FlVector])
    (define zero (lambda ([n : integer]) 0.0))
    (define void-set (lambda ([n : integer]
                              [f : Flonum])
                       (void)))
    (define tile (let ([v (flvector-copy (heightmap-tiles h))])
                   (tile-terrain-data
                    (lambda ([n : integer])
                      (flvector-ref v n))
                    (lambda ([n : integer]
                             [value : flonum])
                      (flvector-set! v n value)))))
    
    (define corner (let* ([corner-count (corner-count grid)]
                          [corner (make-corner-terrain-data corner-count)]
                          [init-corner-array (init-array corner-count)])
                     (init-corner-array (corner-terrain-data-elevation-set! corner)
                                        (curry flvector-ref (heightmap-corners h)))
                     (init-corner-array (corner-terrain-data-river-direction-set! corner)
                                        (lambda ([n : integer]) -1))
                     corner))
    
    (planet-terrain/kw
     #:planet-geometry (planet-geometry/kw
                        #:grid grid
                        #:axis axis)
     #:sea-level 0.0
     #:tile tile
     #:corner corner
     #:rivers '())))

(: empty-planet planet-terrain)
(define empty-planet
  ((heightmap->planet (n-grid 0))
   (heightmap
    (make-flvector 12 0.0)
    (make-flvector 20 0.0))
   default-axis))
