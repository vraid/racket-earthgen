#lang racket

(require "grid-list.rkt"
         "grid-functions.rkt"
         "grid-structs.rkt"
         "pseudo-random-list.rkt"
         racket/flonum)

(provide terrain-create
         terrain-parameters)

(struct terrain-parameters
  (seed
   magnitude
   frequency)
  #:transparent)

(define (average-elevation tile-elevation corner)
  (fl/ (foldl fl+
            0.0
            (map (lambda (n) (vector-ref tile-elevation n))
                 (corner-tiles corner)))
       (exact->inexact corner-edge-count)))

(define (elevation-from numbers scale n)
  (fl* 2.0 (fl* (fl- (vector-ref numbers n) 0.5) scale)))

(define (terrain-create parameters grids)
  (define (create grids)
    (let* ([grid (force (grid-list-first (force grids)))]
           [level (grid-subdivision-level grid)]
           [scale (fl* (terrain-parameters-magnitude parameters)
                       (flexpt (terrain-parameters-frequency parameters) (exact->inexact (+ 1 level))))])
           (if (>= 3 level)
               (let* ([random-gen (pseudo-random-list-next
                                   (+ (grid-tile-count grid) (grid-corner-count grid))
                                   (make-pseudo-random-list (terrain-parameters-seed parameters)))]
                      [numbers (list->vector (pseudo-random-list-numbers random-gen))]
                      [tile-elevation (vector-map (lambda (tile) ((lambda (n) (elevation-from numbers (terrain-parameters-magnitude parameters) n))
                                                                  (tile-id tile)))
                                                  (grid-tiles->vector grid))]
                      [corner-elevation (vector-map (lambda (corner) ((lambda (n) (fl+ (average-elevation tile-elevation corner)
                                                                                       (elevation-from numbers scale (+ n (grid-tile-count grid)))))
                                                                      (corner-id corner)))
                                                    (grid-corners->vector grid))])
                 (list
                  tile-elevation
                  corner-elevation
                  (pseudo-random-list-rest random-gen)))
               (let* ([previous-terrain (create (grid-list-rest (force grids)))]
                      [random-gen (pseudo-random-list-next (grid-corner-count grid)
                                                           (third previous-terrain))]
                      [numbers (list->vector (pseudo-random-list-numbers random-gen))]
                      [tile-elevation (vector-append (first previous-terrain) (second previous-terrain))]
                      [corner-elevation (vector-map (lambda (corner) ((lambda (n) (fl+ (average-elevation tile-elevation corner)
                                                                                       (elevation-from numbers scale n)))
                                                                      (corner-id corner)))
                                                    (grid-corners->vector grid))])
                 (list
                  tile-elevation
                  corner-elevation
                  (pseudo-random-list-rest random-gen))))))
  (create grids))