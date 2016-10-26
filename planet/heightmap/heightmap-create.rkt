#lang typed/racket

(require vraid/random
         vraid/struct
         "../grid-base.rkt"
         "heightmap-structs.rkt"
         math/flonum)

(provide heightmap-create
         heightmap-function
         (struct-out heightmap-parameters)
         heightmap-parameters/kw)

(define-type heightmap-function (grid-list -> heightmap))

(struct/kw heightmap-parameters
           ([seed : seed]
            [base-level : Integer]
            [amplitude : Float]
            [alpha : Float]
            [beta : Float]
            [beta-change : Float])
           #:transparent)

(define inexact-corner-edge-count
  (exact->inexact corner-edge-count))

(: average-elevation (grid FlVector Integer -> Float))
(define (average-elevation g tile-elevation corner)
  (let ([f (grid-corner-tile g)])
    (fl/ (fl+ (flvector-ref tile-elevation (f corner 0))
              (fl+ (flvector-ref tile-elevation (f corner 1))
                   (flvector-ref tile-elevation (f corner 2))))
         inexact-corner-edge-count)))

(: rescale (heightmap-parameters -> (FlVector -> FlVector)))
(define ((rescale p) numbers)
  (flvector-map (rescale-number p) numbers))

(: rescale-number (heightmap-parameters -> (Float -> Float)))
(define (rescale-number p)
  (let ([amp (heightmap-parameters-amplitude p)])
    (lambda ([a : Float])
      (* 2.0 amp (- a 0.5)))))

(: rescaled-numbers (heightmap-parameters -> (pseudo-random-list -> (Integer -> Float))))
(define ((rescaled-numbers p) random-gen)
  (let* ([rescaled ((rescale p) (pseudo-random-list-numbers random-gen))])
    (lambda ([n : Integer])
      (flvector-ref rescaled n))))

(: heightmap-all-random (heightmap-parameters grid -> (List heightmap pseudo-random-list)))
(define (heightmap-all-random parameters grid)
  (let* ([rescale (lambda ([random-gen : pseudo-random-list])
                    ((rescale parameters) (pseudo-random-list-numbers random-gen)))]
         [tile-count (grid-tile-count grid)]
         [tile-gen (pseudo-random-list-next
                    tile-count
                    (make-pseudo-random-list (heightmap-parameters-seed parameters)))]
         [corner-gen (pseudo-random-list-next
                      (grid-corner-count grid)
                      (pseudo-random-list-rest tile-gen))]
         [tile-elevation (rescale tile-gen)]
         [corner-elevation (rescale corner-gen)])
    (list
     (heightmap
      tile-elevation
      corner-elevation)
     (pseudo-random-list-rest corner-gen))))

(: heightmap-create (heightmap-parameters -> heightmap-function))
(define (heightmap-create parameters)
  (let* ([rescale (rescaled-numbers parameters)])
    (lambda: ([grids : grid-list])
      (: create (grid-list -> (List heightmap pseudo-random-list)))
      (define (create grids)
        (let* ([grid (first grids)]
               [level (- (grid-subdivision-level grid) (heightmap-parameters-base-level parameters))]
               [smooth (let* ([alpha (heightmap-parameters-alpha parameters)]
                              [beta (heightmap-parameters-beta parameters)]
                              [beta-change (heightmap-parameters-beta-change parameters)])
                         (lambda ([a : Float]
                                  [b : Float])
                           (+ (* a alpha) (* b beta (flexpt beta-change (+ 1.0 level))))))])
          (if (negative? level)
              (heightmap-all-random parameters grid)
              (if (zero? level)
                  (let* ([random-gen (pseudo-random-list-next
                                      (+ (grid-tile-count grid) (grid-corner-count grid))
                                      (make-pseudo-random-list (heightmap-parameters-seed parameters)))]
                         [number (rescale random-gen)]
                         [tile-elevation (build-flvector (grid-tile-count grid)
                                                         number)]
                         [corner-elevation (build-flvector (grid-corner-count grid)
                                                           (lambda: ([c : Integer])
                                                             (smooth (average-elevation grid tile-elevation c)
                                                                     (number (+ c (grid-tile-count grid))))))])
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
                         [number (rescale random-gen)]
                         [tile-elevation (flvector-append
                                          (heightmap-tiles previous-terrain)
                                          (heightmap-corners previous-terrain))]
                         [corner-elevation (build-flvector (grid-corner-count grid)
                                                           (lambda: ([c : Integer])
                                                             (smooth (average-elevation grid tile-elevation c)
                                                                     (number c))))])
                    (list
                     (heightmap
                      tile-elevation
                      corner-elevation)
                     (pseudo-random-list-rest random-gen)))))))
      (first (create grids)))))
