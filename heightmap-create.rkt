#lang typed/racket

(provide heightmap-create
         heightmap-function
         (struct-out heightmap-parameters)
         heightmap-parameters/kw)

(require "types.rkt"
         "grid.rkt"
         "pseudo-random-list.rkt"
         "heightmap-structs.rkt"
         "grid-list.rkt"
         "typed-struct-kw.rkt"
         math/flonum)

(define-type heightmap-function (grid-list -> heightmap))

(struct/kw: heightmap-parameters
            ([seed : String]
             [base-level : index]
             [amplitude : Flonum]
             [persistence : Flonum])
            #:transparent)

(define inexact-corner-edge-count
  (exact->inexact corner-edge-count))

(: average-elevation (grid FlVector index -> Flonum))
(define (average-elevation g tile-elevation corner)
  (let ([f (grid-corner-tile g)])
    (fl/ (fl+ (flvector-ref tile-elevation (f corner 0))
              (fl+ (flvector-ref tile-elevation (f corner 1))
                   (flvector-ref tile-elevation (f corner 2))))
         inexact-corner-edge-count)))

(: elevation-from-number (Flonum Flonum -> Flonum))
(define (elevation-from-number number scale)
  (fl* 2.0 (fl* (fl- number 0.5) scale)))

(: elevation-from (FlVector Flonum index -> Flonum))
(define (elevation-from numbers scale n)
  (elevation-from-number (flvector-ref numbers n) scale))

(: heightmap-all-random (heightmap-parameters grid -> (List heightmap pseudo-random-list)))
(define (heightmap-all-random parameters grid)
  (let* ([tile-count (grid-tile-count grid)]
         [tile-gen (pseudo-random-list-next
                    tile-count
                    (make-pseudo-random-list (heightmap-parameters-seed parameters)))]
         [corner-gen (pseudo-random-list-next
                      (grid-corner-count grid)
                      (pseudo-random-list-rest tile-gen))]
         [tile-elevation (flvector-map (lambda: ([number : Flonum])
                                         (elevation-from-number number (heightmap-parameters-amplitude parameters)))
                                       (pseudo-random-list-numbers tile-gen))]
         [corner-elevation (flvector-map (lambda: ([number : Flonum])
                                           (elevation-from-number number (heightmap-parameters-amplitude parameters)))
                                         (pseudo-random-list-numbers corner-gen))])
    (list
     (heightmap
      tile-elevation
      corner-elevation)
     (pseudo-random-list-rest corner-gen))))

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
                       [numbers (pseudo-random-list-numbers random-gen)]
                       [tile-elevation (build-flvector (grid-tile-count grid)
                                                      (lambda: ([t : index])
                                                        (elevation-from numbers (heightmap-parameters-amplitude parameters) t)))]
                       [corner-elevation (build-flvector (grid-corner-count grid)
                                                        (lambda: ([c : index])
                                                          (fl+ (average-elevation grid tile-elevation c)
                                                               (elevation-from numbers scale (+ c (grid-tile-count grid))))))])
                  (list
                   (heightmap
                    tile-elevation
                    corner-elevation)
                   (pseudo-random-list-rest random-gen)))
                (let* ([flvector-append (lambda: ([a : FlVector]
                                                  [b : FlVector])
                                          (let ([a-length (flvector-length a)])
                                            (build-flvector (+ a-length (flvector-length b))
                                                            (lambda: ([n : Integer])
                                                              (if (< n a-length)
                                                                  (flvector-ref a n)
                                                                  (flvector-ref b (- n a-length)))))))]
                       [previous (create (rest grids))]
                       [previous-terrain (first previous)]
                       [random-gen (pseudo-random-list-next (grid-corner-count grid)
                                                            (second previous))]
                       [numbers (pseudo-random-list-numbers random-gen)]
                       [tile-elevation (flvector-append
                                        (heightmap-tiles previous-terrain)
                                        (heightmap-corners previous-terrain))]
                       [corner-elevation (build-flvector (grid-corner-count grid)
                                                         (lambda: ([c : index])
                                                           (fl+ (average-elevation grid tile-elevation c)
                                                                (elevation-from numbers scale c))))])
                  (list
                   (heightmap
                    tile-elevation
                    corner-elevation)
                   (pseudo-random-list-rest random-gen)))))))
    (first (create grids))))