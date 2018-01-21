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
    (λ ([a : Float])
      (* 2.0 amp (- a 0.5)))))

(: rescaled-numbers (heightmap-parameters -> (FlVector -> (Integer -> Float))))
(define ((rescaled-numbers p) vec)
  (let* ([rescaled ((rescale p) vec)])
    (λ ([n : Integer])
      (flvector-ref rescaled n))))

(: random-take/rescale (All (A) ((FlVector -> A) -> (Pseudo-Random-Generator Integer -> (Values A Pseudo-Random-Generator)))))
(define ((random-take/rescale rescale) s n)
  (let-values ([(nums state) (random-take s n)])
    (values (rescale nums) state)))
  
(: heightmap-all-random (heightmap-parameters grid -> (List heightmap Pseudo-Random-Generator)))
(define (heightmap-all-random parameters grid)
  (let* ([take (random-take/rescale (λ ([vec : FlVector])
                                      ((rescale parameters) vec)))]
         [tile-count (grid-tile-count grid)]
         [state (seed->state (heightmap-parameters-seed parameters))])
    (let*-values ([(tilenums tstate) (take state tile-count)]
                  [(cornernums cstate) (take tstate (grid-corner-count grid))])
      (list
       (heightmap
        tilenums
        cornernums)
       cstate))))

(: heightmap-create (heightmap-parameters -> heightmap-function))
(define (heightmap-create parameters)
  (let* ([rescale (rescaled-numbers parameters)]
         [take (random-take/rescale rescale)])
    (λ ([grids : grid-list])
      (: create (grid-list -> (List heightmap Pseudo-Random-Generator)))
      (define (create grids)
        (let* ([grid (first grids)]
               [level (- (grid-subdivision-level grid) (heightmap-parameters-base-level parameters))]
               [smooth (let* ([alpha (heightmap-parameters-alpha parameters)]
                              [beta (heightmap-parameters-beta parameters)]
                              [beta-change (heightmap-parameters-beta-change parameters)])
                         (λ ([a : Float]
                             [b : Float])
                           (+ (* a alpha) (* b beta (flexpt beta-change (+ 1.0 level))))))])
          (if (negative? level)
              (heightmap-all-random parameters grid)
              (if (zero? level)
                  (let-values ([(nums state) (take
                                              (seed->state (heightmap-parameters-seed parameters))
                                              (+ (grid-tile-count grid) (grid-corner-count grid)))])
                    (let* ([tile-elevation (build-flvector (grid-tile-count grid)
                                                           nums)]
                           [corner-elevation (build-flvector (grid-corner-count grid)
                                                             (λ ([c : Integer])
                                                               (smooth (average-elevation grid tile-elevation c)
                                                                       (nums (+ c (grid-tile-count grid))))))])
                      (list
                       (heightmap
                        tile-elevation
                        corner-elevation)
                       state)))
                  (let* ([flvector-append (λ ([a : FlVector]
                                              [b : FlVector])
                                            (let ([a-length (flvector-length a)])
                                              (build-flvector (+ a-length (flvector-length b))
                                                              (λ ([n : Integer])
                                                                (if (< n a-length)
                                                                    (flvector-ref a n)
                                                                    (flvector-ref b (- n a-length)))))))]
                         [previous (create (rest grids))]
                         [previous-terrain (first previous)])
                    (let-values ([(nums state) (take (second previous)
                                                     (grid-corner-count grid))])
                      (let* ([tile-elevation (flvector-append
                                              (heightmap-tiles previous-terrain)
                                              (heightmap-corners previous-terrain))]
                             [corner-elevation (build-flvector (grid-corner-count grid)
                                                               (λ ([c : Integer])
                                                                 (smooth (average-elevation grid tile-elevation c)
                                                                         (nums c))))])
                        (list
                         (heightmap
                          tile-elevation
                          corner-elevation)
                         state))))))))
      (first (create grids)))))
