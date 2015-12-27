
#lang typed/racket

(provide (all-defined-out))

(require vraid/types
         vraid/math
         vraid/typed-array
         vraid/util
         "../grid.rkt"
         "../heightmap.rkt"
         "../geometry.rkt"
         "../terrain.rkt"
         "../water.rkt"
         racket/flonum)

(define tile-init
  (lambda ([grid : grid])
    (init-array (tile-count grid))))

(define corner-init
  (lambda ([grid : grid])
    (init-array (corner-count grid))))

(define edge-init
  (lambda ([grid : grid])
    (init-array (edge-count grid))))

(: planet/sea-level (flonum planet-terrain -> planet-water))
(define (planet/sea-level sea-level p)
  (let* ([void-fl-set! (lambda ([n : integer]
                                [v : flonum])
                         (void))]
         [void-int-set! (lambda ([n : integer]
                                 [v : integer])
                          (void))])
    (planet-water/kw
     #:planet-terrain p
     #:sea-level sea-level
     #:tile (tile-water-data (lambda ([n : integer]) sea-level)
                             void-fl-set!)
     #:corner (corner-water-data (lambda ([n : integer]) -1)
                                 void-int-set!)
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

(: heightmap->planet (grid -> (heightmap FlVector -> planet-terrain)))
(define (heightmap->planet grid)
  (define empty-hash (lambda ([n : integer])
                       (lambda ([key :  Symbol])
                         #f)))
  (lambda ([h : heightmap]
           [axis : FlVector])
    (define sea-level 0.0)
    (define radius 6371000.0)
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
                     corner))
    
    (planet-terrain/kw
     #:planet-geometry (planet-geometry/kw
                        #:grid grid
                        #:axis axis
                        #:radius radius
                        #:tile (tile-geometry-data
                                (build-flvector-ref (tile-count grid)
                                                    (lambda ([n : integer])
                                                      (n-gon-area radius grid n)))))
     #:tile tile
     #:corner corner)))

(: empty-planet planet-terrain)
(define empty-planet
  ((heightmap->planet (n-grid 0))
   (heightmap
    (make-flvector 12 0.0)
    (make-flvector 20 0.0))
   default-axis))
