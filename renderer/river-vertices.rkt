#lang typed/racket

(require vraid/math
         vraid/typed-gl
         vraid/color
         vraid/util
         math/flonum
         "planet-data.rkt")

(provide (all-defined-out))

(define init-buffer
  (λ ([data : planet-data]
      [transform : (FlVector -> (FlVector -> FlVector))]
      [color : flcolor]
      [buffer : gl-buffer])
    (let* ([tile-count (planet-data-tile-count data)]
           [tile-coordinates (planet-data-tile-coordinates data)]
           [corner-coordinates (planet-data-corner-coordinates data)]
           [tile-corner (planet-data-tile-corner data)])
      (let ([set-color (gl-buffer-set-vertex-color! buffer)])
        (for ([n (* 24 tile-count)])
          (set-color n color)))
      (let ([set-coord (gl-buffer-set-vertex-coord! buffer)]
            [set-index (gl-buffer-set-index! buffer)])
        (for ([n tile-count])
          (let ([f (transform (tile-coordinates n))])
            (for ([k 6])
              (let* ([index-offset (+ (* n 36) (* k 6))]
                     [vertex-offset (+ (* n 24) (* k 4))]
                     [set-index/vertex (λ ([i : Integer]
                                           [v : Integer])
                                         (set-index (+ index-offset i)
                                                    (+ vertex-offset v)))])
                (set-index/vertex 0 0)
                (set-index/vertex 1 1)
                (set-index/vertex 2 2)
                (set-index/vertex 3 1)
                (set-index/vertex 4 3)
                (set-index/vertex 5 2)
                (let* ([corner (tile-corner n k)]
                       [corner-coord (f (corner-coordinates corner))]
                       [next-corner (tile-corner n (+ k 1))]
                       [next-corner-coord (f (corner-coordinates next-corner))])
                  (for ([n 2])
                    (set-coord (+ vertex-offset (* 2 n)) corner-coord)
                    (set-coord (+ vertex-offset (* 2 n) 1) next-corner-coord)))))))))))

(define update-buffer
  (λ ([data : planet-data]
      [transform : (FlVector -> (FlVector -> FlVector))]
      [buffer : gl-buffer])
    (let* ([planet-radius (planet-data-radius data)]
           [tile-count (planet-data-tile-count data)]
           [tile-coordinates (planet-data-tile-coordinates data)]
           [corner-coordinates (planet-data-corner-coordinates data)]
           [tile-corner (planet-data-tile-corner data)]
           [tile-edge (planet-data-tile-edge data)]
           [tile-edge-count (planet-data-tile-edge-count data)]
           [river-flows-to? (planet-data-river-flows-to? data)]
           [corner-river-flow (planet-data-corner-river-flow data)]
           [edge-river-flow (planet-data-edge-river-flow data)]
           [max-scale (/ (flvector3-distance
                          (tile-coordinates 0)
                          (corner-coordinates (tile-corner 0 0)))
                         8)]
           [flow->scale (λ ([flow : Float])
                          (min max-scale
                               (fl/ (* 1.0 (flsqrt flow))
                                    planet-radius)))]
           [set-coord (gl-buffer-set-vertex-coord! buffer)])
      (for ([tile tile-count])
        (let* ([tile-coord (tile-coordinates tile)]
               [f (transform tile-coord)]
               [tile-coord (f tile-coord)]
               [edge-count (tile-edge-count tile)]
               [scale (λ ([flow : (Integer -> Float)])
                        (build-flvector-ref edge-count
                                            (λ ([n : Integer])
                                              (flow->scale (flow n)))))]
               [corner-scale (scale (λ ([n : Integer])
                                      (corner-river-flow (tile-corner tile n))))]
               [edge-scale (scale (λ ([n : Integer])
                                    (edge-river-flow (tile-edge tile n))))]            
               [river-direction (build-vector-ref edge-count
                                                  (λ ([edge : Integer])
                                                    (let ([c1 (tile-corner tile edge)]
                                                          [c2 (tile-corner tile (+ 1 edge))])
                                                      (cond [((river-flows-to? c1) c2) 'cw]
                                                            [((river-flows-to? c2) c1) 'ccw]
                                                            [else #f]))))])
          (for ([edge edge-count])
            (let* ([direction (river-direction edge)]
                   [cw? (equal? direction 'cw)]
                   [corner-coord (λ ([n : Integer])
                                   (f (corner-coordinates (tile-corner tile (+ edge n)))))]
                   [scale1 (if direction
                               (if cw?
                                   (edge-scale edge)
                                   (corner-scale edge))
                               0.0)]
                   [scale2 (if direction
                               (if cw?
                                   (corner-scale (modulo (+ 1 edge) edge-count))
                                   (edge-scale edge))
                               0.0)]
                   [scale-coord (λ ([scale : Float]
                                    [v : FlVector]
                                    [u : FlVector])
                                  (flvector3-sum v (flvector3-scale-to
                                                    scale
                                                    (flvector3-subtract u v))))]
                   [c1-scaled-coord (scale-coord scale1 (corner-coord 0) (corner-coord -1))]
                   [c2-scaled-coord (scale-coord scale2 (corner-coord 1) (corner-coord 2))]
                   [index-offset (+ (* 24 tile) (* 4 edge))])
              (set-coord (+ index-offset 2) c1-scaled-coord)
              (set-coord (+ index-offset 3) c2-scaled-coord))))))))
