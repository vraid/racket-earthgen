#lang typed/racket

(provide river-renderer%)

(require vraid/math
         vraid/typed-gl
         vraid/color
         vraid/util
         math/flonum
         "../planet/grid-base.rkt"
         "../planet/geometry-base.rkt"
         "../planet/water.rkt"
         "../planet/climate-base.rkt")

(define river-color
  (flcolor3 0.06862745098039216 0.17450980392156862 0.37058823529411766))

(define river-renderer%
  (class object%
    (super-new)
    (init-field [planet : (-> planet-climate)])
    (: buffer-tile-count Integer)
    (define buffer-tile-count 0)
    (: buffer gl-buffer)
    (define buffer (make-gl-buffer 0 0))
    (: resize-buffer? (-> Boolean))
    (define (resize-buffer?)
      (not (= buffer-tile-count (tile-count (planet)))))
    (: resize-buffer (-> Void))
    (define/public (resize-buffer)
      (when (resize-buffer?)
        (let* ([tile-count (tile-count (planet))]
               [buf (make-gl-buffer (* 24 tile-count) (* 36 tile-count))])
          (set! buffer-tile-count tile-count)
          (set! buffer buf)
          (init-buffer (planet) buffer))))
    (: remake-buffer (-> Void))
    (define/public (remake-buffer)
      (update-buffer (planet) buffer)
      ((gl-buffer-bind buffer)))
    (: render (-> Void))
    (define/public (render)
      (gl-cull-face 'back)
      ((gl-buffer-draw buffer)))))

(: init-buffer (grid gl-buffer -> Void))
(define (init-buffer planet buffer)
  (let ([set-color (gl-buffer-set-vertex-color! buffer)])
    (for ([n (* 24 (tile-count planet))])
      (set-color n river-color)))
  (let ([set-coord (gl-buffer-set-vertex-coord! buffer)]
        [set-index (gl-buffer-set-index! buffer)])
    (for ([n (tile-count planet)])
      (for ([k 6])
        (let* ([index-offset (+ (* n 36) (* k 6))]
               [vertex-offset (+ (* n 24) (* k 4))]
               [set-index/vertex (lambda ([i : Integer]
                                          [v : Integer])
                                   (set-index (+ index-offset i)
                                              (+ vertex-offset v)))])
          (set-index/vertex 0 0)
          (set-index/vertex 1 1)
          (set-index/vertex 2 2)
          (set-index/vertex 3 1)
          (set-index/vertex 4 3)
          (set-index/vertex 5 2)
          (let* ([corner (tile-corner planet n k)]
                 [corner-coord (corner-coordinates planet corner)]
                 [next-corner (tile-corner planet n (+ k 1))]
                 [next-corner-coord (corner-coordinates planet next-corner)])
            (for ([n 2])
              (set-coord (+ vertex-offset (* 2 n)) corner-coord)
              (set-coord (+ vertex-offset (* 2 n) 1) next-corner-coord))))))))

(: update-buffer (planet-climate gl-buffer -> Void))
(define (update-buffer planet buffer)
  (let* ([max-scale (/ (flvector3-distance
                        (tile-coordinates planet 0)
                        (corner-coordinates planet ((grid-tile-corner planet) 0 0)))
                       2)]
         [flow->scale (lambda ([flow : Float])
                        (min max-scale
                             (fl/ (* 60000.0 (flsqrt (fl/ flow 140000.0)))
                                  (planet-radius planet))))]
         [set-coord (gl-buffer-set-vertex-coord! buffer)])
    (for ([tile (grid-tile-count planet)])
      (let* ([edge-count (tile-edge-count tile)]
             [scale (lambda ([flow : (Integer -> Float)])
                      (build-flvector-ref edge-count
                                          (lambda ([n : Integer])
                                            (flow->scale (flow n)))))]
             [corner-scale (scale (lambda ([n : Integer])
                                    (corner-river-flow planet (tile-corner planet tile n))))]
             [edge-scale (scale (lambda ([n : Integer])
                                  (edge-river-flow planet (tile-edge planet tile n))))]            
             [river-direction (build-vector-ref edge-count
                                                (lambda ([edge : Integer])
                                                  (let ([c1 (tile-corner planet tile edge)]
                                                        [c2 (tile-corner planet tile (+ 1 edge))])
                                                    (if ((river-flows-to? planet c1) c2)
                                                        'cw
                                                        (if ((river-flows-to? planet c2) c1)
                                                            'ccw
                                                            #f)))))])
        (for ([edge edge-count])
          (let* ([direction (river-direction edge)]
                 [cw? (equal? direction 'cw)]
                 [corner-coord (lambda ([n : Integer])
                                 (corner-coordinates planet (tile-corner planet tile (+ edge n))))]
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
                 [tile-coord (tile-coordinates planet tile)]
                 [scale-coord (lambda ([scale : Float]
                                       [v : FlVector]
                                       [u : FlVector])
                                (flvector3-sum v (flvector3-scale-to
                                                  scale
                                                  (flvector3-subtract v u))))]
                 [c1-scaled-coord (scale-coord scale1 (corner-coord 0) (corner-coord -1))]
                 [c2-scaled-coord (scale-coord scale2 (corner-coord 1) (corner-coord 2))]
                 [index-offset (+ (* 24 tile) (* 4 edge))])
            (set-coord (+ index-offset 2) c1-scaled-coord)
            (set-coord (+ index-offset 3) c2-scaled-coord)))))))
