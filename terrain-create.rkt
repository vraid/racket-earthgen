#lang racket

(require "grid-list.rkt"
         "grid-functions.rkt"
         "grid-structs.rkt"
         "pseudo-random-list.rkt"
         racket/flonum)

(provide terrain-create
         terrain-parameters
         terrain-tile-elevation
         terrain-corner-elevation)

(struct terrain-parameters
  (seed
   base-level
   magnitude
   frequency)
  #:transparent)

(struct terrain
  (tile-elevation
   corner-elevation)
  #:transparent)

(define (average-elevation tile-elevation corner)
  (fl/ (foldl fl+
            0.0
            (map (lambda (n) (vector-ref tile-elevation n))
                 (corner-tiles corner)))
       (exact->inexact corner-edge-count)))

(define (elevation-from-number number scale)
  (fl* 2.0 (fl* (fl- number 0.5) scale)))

(define (elevation-from numbers scale n)
  (elevation-from-number (vector-ref numbers n) scale))

(define (terrain-all-random parameters grid)
  (let* ([tile-count (grid-tile-count grid)]
         [random-gen (pseudo-random-list-next
                      (+ tile-count (grid-corner-count grid))
                      (make-pseudo-random-list (terrain-parameters-seed parameters)))]
         [elevation (list->vector
                     (map (lambda (number)
                            (elevation-from-number number (terrain-parameters-magnitude parameters)))
                          (pseudo-random-list-numbers random-gen)))])
    (terrain
     (vector-take tile-count elevation)
     (vector-drop tile-count elevation))))

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
                     [numbers (list->vector (pseudo-random-list-numbers random-gen))]
                     [tile-elevation (vector-map (lambda (tile) ((lambda (n) (elevation-from numbers (terrain-parameters-magnitude parameters) n))
                                                                 (tile-id tile)))
                                                 (grid-tiles->vector grid))]
                     [corner-elevation (vector-map (lambda (corner) ((lambda (n) (fl+ (average-elevation tile-elevation corner)
                                                                                      (elevation-from numbers scale (+ n (grid-tile-count grid)))))
                                                                     (corner-id corner)))
                                                   (grid-corners->vector grid))])
                (list
                 (terrain
                  tile-elevation
                  corner-elevation)
                 (pseudo-random-list-rest random-gen)))
              (let* ([previous (create (grid-list-rest (force grids)))]
                     [previous-terrain (first previous)]
                     [random-gen (pseudo-random-list-next (grid-corner-count grid)
                                                          (second previous))]
                     [numbers (list->vector (pseudo-random-list-numbers random-gen))]
                     [tile-elevation (vector-append (terrain-tile-elevation previous-terrain) (terrain-corner-elevation previous-terrain))]
                     [corner-elevation (vector-map (lambda (corner) ((lambda (n) (fl+ (average-elevation tile-elevation corner)
                                                                                      (elevation-from numbers scale n)))
                                                                     (corner-id corner)))
                                                   (grid-corners->vector grid))])
                (list
                 (terrain
                  tile-elevation
                  corner-elevation)
                 (pseudo-random-list-rest random-gen)))))))
    (create grids))