#lang racket

(require "grid.rkt"
         "pseudo-random-list.rkt"
         "heightmap-structs.rkt"
         math/flonum)

(provide heightmap-create
         heightmap-parameters)

(struct heightmap-parameters
  (seed
   base-level
   amplitude
   persistence)
  #:transparent)

(define (average-elevation tile-elevation corner)
  (fl/ (foldl fl+
            0.0
            (map (lambda (n) (flvector-ref tile-elevation n))
                 (corner-tiles corner)))
       (exact->inexact corner-edge-count)))

(define (elevation-from-number number scale)
  (fl* 2.0 (fl* (fl- number 0.5) scale)))

(define (elevation-from numbers scale n)
  (elevation-from-number (flvector-ref numbers n) scale))

(define (heightmap-all-random parameters grid)
  (let* ([tile-count (grid-tile-count grid)]
         [random-gen (pseudo-random-list-next
                      (+ tile-count (grid-corner-count grid))
                      (make-pseudo-random-list (heightmap-parameters-seed parameters)))]
         [elevation (map (lambda (number)
                           (elevation-from-number number (heightmap-parameters-amplitude parameters)))
                         (pseudo-random-list-numbers random-gen))])
    (list
     (heightmap
      (list->flvector (take elevation tile-count))
      (list->flvector (drop elevation tile-count)))
     (pseudo-random-list-rest random-gen))))

(define (heightmap-create parameters)
  (lambda (grids)
    (define (create grids)
      (let* ([grid (force (grid-list-first (force grids)))]
             [level (- (grid-subdivision-level grid) (heightmap-parameters-base-level parameters))]
             [scale (fl* (heightmap-parameters-amplitude parameters)
                         (flexpt (heightmap-parameters-persistence parameters) (exact->inexact (+ 1 level))))])
        (if (negative? level)
            (heightmap-all-random parameters grid)
            (if (zero? level)
                (let* ([random-gen (pseudo-random-list-next
                                    (+ (grid-tile-count grid) (grid-corner-count grid))
                                    (make-pseudo-random-list (heightmap-parameters-seed parameters)))]
                       [numbers (list->flvector (pseudo-random-list-numbers random-gen))]
                       [tile-elevation (vector->flvector
                                        (vector-map (lambda (tile)
                                                      ((lambda (n) (elevation-from numbers (heightmap-parameters-amplitude parameters) n))
                                                       (tile-id tile)))
                                                    (grid-tiles->vector grid)))]
                       [corner-elevation (vector->flvector
                                          (vector-map (lambda (corner)
                                                        ((lambda (n) (fl+ (average-elevation tile-elevation corner)
                                                                          (elevation-from numbers scale (+ n (grid-tile-count grid)))))
                                                         (corner-id corner)))
                                                      (grid-corners->vector grid)))])
                  (list
                   (heightmap
                    tile-elevation
                    corner-elevation)
                   (pseudo-random-list-rest random-gen)))
                (let* ([flvector-append (lambda (a b)
                                          (list->flvector
                                           (append (flvector->list a) (flvector->list b))))]
                       [previous (create (grid-list-rest (force grids)))]
                       [previous-terrain (first previous)]
                       [random-gen (pseudo-random-list-next (grid-corner-count grid)
                                                            (second previous))]
                       [numbers (list->flvector (pseudo-random-list-numbers random-gen))]
                       [tile-elevation (flvector-append
                                        (heightmap-tiles previous-terrain)
                                        (heightmap-corners previous-terrain))]
                       [corner-elevation (vector->flvector 
                                          (vector-map (lambda (corner)
                                                        ((lambda (n) (fl+ (average-elevation tile-elevation corner)
                                                                          (elevation-from numbers scale n)))
                                                         (corner-id corner)))
                                                      (grid-corners->vector grid)))])
                  (list
                   (heightmap
                    tile-elevation
                    corner-elevation)
                   (pseudo-random-list-rest random-gen)))))))
    (first (create grids))))