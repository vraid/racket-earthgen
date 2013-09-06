#lang racket

(require "grid-list.rkt"
         "grid-functions.rkt"
         "grid-structs.rkt"
         "pseudo-random-list.rkt"
         "terrain-structs.rkt"
         math/flonum)

(provide terrain-create
         terrain-parameters
         terrain
         terrain-tile-elevation
         terrain-corner-elevation)

(struct terrain-parameters
  (seed
   base-level
   magnitude
   frequency)
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

(define (terrain-all-random parameters grid)
  (let* ([tile-count (grid-tile-count grid)]
         [random-gen (pseudo-random-list-next
                      (+ tile-count (grid-corner-count grid))
                      (make-pseudo-random-list (terrain-parameters-seed parameters)))]
         [elevation (map (lambda (number)
                           (elevation-from-number number (terrain-parameters-magnitude parameters)))
                         (pseudo-random-list-numbers random-gen))])
    (list
     (terrain
      (list->flvector (take elevation tile-count))
      (list->flvector (drop elevation tile-count))
      #f)
     (pseudo-random-list-rest random-gen))))

(define (terrain-create parameters grids)
  (define (create grids)
    (let* ([grid (force (grid-list-first (force grids)))]
           [level (- (grid-subdivision-level grid) (terrain-parameters-base-level parameters))]
           [scale (fl* (terrain-parameters-magnitude parameters)
                       (flexpt (terrain-parameters-frequency parameters) (exact->inexact (+ 1 level))))])
      (if (negative? level)
          (terrain-all-random parameters grid)
          (if (zero? level)
              (let* ([random-gen (pseudo-random-list-next
                                  (+ (grid-tile-count grid) (grid-corner-count grid))
                                  (make-pseudo-random-list (terrain-parameters-seed parameters)))]
                     [numbers (list->flvector (pseudo-random-list-numbers random-gen))]
                     [tile-elevation (vector->flvector
                                      (vector-map (lambda (tile)
                                                    ((lambda (n) (elevation-from numbers (terrain-parameters-magnitude parameters) n))
                                                     (tile-id tile)))
                                                  (grid-tiles->vector grid)))]
                     [corner-elevation (vector->flvector
                                        (vector-map (lambda (corner)
                                                      ((lambda (n) (fl+ (average-elevation tile-elevation corner)
                                                                        (elevation-from numbers scale (+ n (grid-tile-count grid)))))
                                                       (corner-id corner)))
                                                    (grid-corners->vector grid)))])
                (list
                 (terrain
                  tile-elevation
                  corner-elevation
                  #f)
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
                                      (terrain-tile-elevations previous-terrain)
                                     (terrain-corner-elevations previous-terrain))]
                     [corner-elevation (vector->flvector 
                                        (vector-map (lambda (corner)
                                                      ((lambda (n) (fl+ (average-elevation tile-elevation corner)
                                                                        (elevation-from numbers scale n)))
                                                       (corner-id corner)))
                                                    (grid-corners->vector grid)))])
                (list
                 (terrain
                  tile-elevation
                  corner-elevation
                  #f)
                 (pseudo-random-list-rest random-gen)))))))
    (create grids))