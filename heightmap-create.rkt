#lang typed/racket

(require "types.rkt"
         "grid.rkt"
         "pseudo-random-list.rkt"
         "heightmap-structs.rkt"
         "grid-list.rkt"
         "typed-struct-kw.rkt"
         math/flonum)

(provide heightmap-create
         heightmap-function
         (struct-out heightmap-parameters)
         heightmap-parameters/kw)

(define-type heightmap-function (grid-list -> heightmap))

(struct:/kw heightmap-parameters
            ([seed : String]
             [base-level : index]
             [amplitude : Flonum]
             [persistence : Flonum])
            #:transparent)

(: average-elevation (FlVector corner -> Flonum))
(define (average-elevation tile-elevation corner)
  (fl/ (foldl fl+
            0.0
            (map (lambda: ([n : index]) (flvector-ref tile-elevation n))
                 (corner-tiles corner)))
       (exact->inexact corner-edge-count)))

(: elevation-from-number (Flonum Flonum -> Flonum))
(define (elevation-from-number number scale)
  (fl* 2.0 (fl* (fl- number 0.5) scale)))

(: elevation-from (FlVector Flonum index -> Flonum))
(define (elevation-from numbers scale n)
  (elevation-from-number (flvector-ref numbers n) scale))

(: heightmap-all-random (heightmap-parameters grid -> (List heightmap pseudo-random-list)))
(define (heightmap-all-random parameters grid)
  (let* ([tile-count (grid-tile-count grid)]
         [random-gen (pseudo-random-list-next
                      (+ tile-count (grid-corner-count grid))
                      (make-pseudo-random-list (heightmap-parameters-seed parameters)))]
         [elevation (map (lambda: ([number : Flonum])
                           (elevation-from-number number (heightmap-parameters-amplitude parameters)))
                         (pseudo-random-list-numbers random-gen))])
    (list
     (heightmap
      (list->flvector (take elevation tile-count))
      (list->flvector (drop elevation tile-count)))
     (pseudo-random-list-rest random-gen))))

(: heightmap-create (heightmap-parameters -> heightmap-function))
(define (heightmap-create parameters)
  (lambda: ([grids : grid-list])
    (: create (grid-list -> (List heightmap pseudo-random-list)))
    (define (create grids)
      (let* ([grid (first grids)]
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
                                        (vector-map (lambda: ([t : tile])
                                                      ((lambda: ([n : index]) (elevation-from numbers (heightmap-parameters-amplitude parameters) n))
                                                       (tile-id t)))
                                                    (grid-tiles->vector grid)))]
                       [corner-elevation (vector->flvector
                                          (vector-map (lambda: ([c : corner])
                                                        ((lambda: ([n : index]) (fl+ (average-elevation tile-elevation c)
                                                                          (elevation-from numbers scale (+ n (grid-tile-count grid)))))
                                                         (corner-id c)))
                                                      (grid-corners->vector grid)))])
                  (list
                   (heightmap
                    tile-elevation
                    corner-elevation)
                   (pseudo-random-list-rest random-gen)))
                (let* ([flvector-append (lambda: ([a : FlVector]
                                                  [b : FlVector])
                                          (list->flvector
                                           (append (flvector->list a) (flvector->list b))))]
                       [previous (create (rest grids))]
                       [previous-terrain (first previous)]
                       [random-gen (pseudo-random-list-next (grid-corner-count grid)
                                                            (second previous))]
                       [numbers (list->flvector (pseudo-random-list-numbers random-gen))]
                       [tile-elevation (flvector-append
                                        (heightmap-tiles previous-terrain)
                                        (heightmap-corners previous-terrain))]
                       [corner-elevation (vector->flvector 
                                          (vector-map (lambda: ([c : corner])
                                                        ((lambda: ([n : index]) (fl+ (average-elevation tile-elevation c)
                                                                          (elevation-from numbers scale n)))
                                                         (corner-id c)))
                                                      (grid-corners->vector grid)))])
                  (list
                   (heightmap
                    tile-elevation
                    corner-elevation)
                   (pseudo-random-list-rest random-gen)))))))
    (first (create grids))))