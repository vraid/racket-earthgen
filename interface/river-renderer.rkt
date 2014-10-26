#lang racket

(provide river-renderer%)

(require vraid/flow
         vraid/math
         vraid/opengl
         vraid/color
         "../planet/planet.rkt"
         "../planet/corner-terrain.rkt"
         racket/flonum
         ffi/cvector
         ffi/unsafe)

(define river-color
  (flcolor3 0.06862745098039216 0.17450980392156862 0.37058823529411766))

(define river-renderer%
  (class object%
    (super-new)
    (init-field planet)
    (define buffer-corner-count 0)
    (define river-vertex-buffer
      (gl-vertex-buffer
       (generate-gl-buffer-handle)
       (make-cvector _uint 0)))
    (define river-index-buffer
      (gl-index-buffer
       (generate-gl-buffer-handle)
       (make-cvector _uint 0)))
    (define (resize-buffer?)
      (not (= buffer-corner-count (corner-count (planet)))))
    (define/public (resize-buffer)
      (when (resize-buffer?)
        (let ([b (make-river-buffer (planet))])
          (set! buffer-corner-count (river-buffer-count b))
          (set! river-index-buffer
                (gl-index-buffer
                 (gl-index-buffer-handle river-index-buffer)
                 (river-buffer-indices b)))
          (set! river-vertex-buffer
                (gl-vertex-buffer
                 (gl-vertex-buffer-handle river-vertex-buffer)
                 (river-buffer-vertices b))))))
    (define/public (remake-buffer)
      (let* ([planet (planet)]
             [b (update-buffer planet
                              (river-buffer
                               (corner-count planet)
                               (gl-vertex-buffer-data river-vertex-buffer)
                               (gl-index-buffer-data river-index-buffer)))])
        (set! river-index-buffer
              (gl-index-buffer
               (gl-index-buffer-handle river-index-buffer)
               (river-buffer-indices b)))
        (set! river-vertex-buffer
              (gl-vertex-buffer
               (gl-vertex-buffer-handle river-vertex-buffer)
               (river-buffer-vertices b)))
        (set-gl-vertex-buffer! river-vertex-buffer)
        (set-gl-index-buffer! river-index-buffer)))
    (define/public (render)
      (gl-cull-face 'back)
      (gl-draw river-vertex-buffer
               river-index-buffer))))

(struct river-buffer
  (count vertices indices))

(define (->gl-vertex coord color)
  (make-gl-vertex
   (flvector-ref coord 0)
   (flvector-ref coord 1)
   (flvector-ref coord 2)
   (flcolor->byte (flcolor-red color))
   (flcolor->byte (flcolor-green color))
   (flcolor->byte (flcolor-blue color))
   (flcolor->byte (flcolor-alpha color))))

(define (make-river-buffer planet)
  (let* ([count (corner-count planet)]
         [vertices (make-cvector _gl-vertex (* 6 count))]
         [indices (make-cvector _uint (* 12 count))])
    (river-buffer
     count
     vertices
     indices)))

(define (update-buffer planet buffer)
  (define base-indices
    '(#(2 3 1)
      #(1 3 4)
      #(4 5 1)
      #(1 5 0)))
  (let* ([count (river-buffer-count buffer)]
         [max-scale (/ (flvector3-distance
                        (tile-coordinates planet 0)
                        (corner-coordinates planet ((grid-tile-corner planet) 0 0)))
                       2)]
         [vertices (river-buffer-vertices buffer)]
         [indices (river-buffer-indices buffer)]
         [color river-color])
    (for ([n count])
      (let* ([direction (corner-river-direction planet n)]
             [river? direction])
        (when river?
          (let* ([flow->scale (lambda (flow)
                                (min max-scale
                                     (/ (* 60000.0 (sqrt (/ flow 140000.0)))
                                        (planet-radius planet))))]
                 [flow (edge-river-flow planet (corner-edge planet n direction))]
                 [inflow (if (zero? (length (corner-river-sources planet n)))
                             0.0
                             flow)]
                 [outflow (if-let* ([i (corner-corner planet n direction)]
                                    [dir (corner-river-direction planet i)]
                                    [_ (= 1 (length (corner-river-sources planet i)))])
                            (edge-river-flow planet (corner-edge planet i dir))
                            flow)]
                 [inscale (flow->scale inflow)]
                 [outscale (flow->scale outflow)]
                 [opposite ((grid-corner-corner planet) n direction)]
                 [opposite-direction (grid-corner-corner-position planet opposite n)]
                 [vectors (lambda (scale n direction)
                            (let ([v (corner-coordinates planet n)]
                                  [a (corner-coordinates planet ((grid-corner-corner planet) n (- direction 1)))]
                                  [b (corner-coordinates planet ((grid-corner-corner planet) n (+ direction 1)))]
                                  [scaled (lambda (v u)
                                            (flvector3-sum v (flvector3-scale-to
                                                              scale
                                                              (flvector3-subtract v u))))])
                              (values (scaled v b) v (scaled v a))))])
            (let-values ([(b v a) (vectors inscale n direction)]
                         [(d u c) (vectors outscale opposite opposite-direction)])
              (cvector-set! vertices (* n 6) (->gl-vertex b color))
              (cvector-set! vertices (+ 1 (* n 6)) (->gl-vertex v color))
              (cvector-set! vertices (+ 2 (* n 6)) (->gl-vertex a color))
              (cvector-set! vertices (+ 3 (* n 6)) (->gl-vertex d color))
              (cvector-set! vertices (+ 4 (* n 6)) (->gl-vertex u color))
              (cvector-set! vertices (+ 5 (* n 6)) (->gl-vertex c color)))))
        (for ([k (range 4)]
              [v base-indices])
          (let ([ref (if river?
                         (curry vector-ref v)
                         (thunk* 0))]
                [offset (* n 6)])
            (cvector-set! indices (+ (* k 3) (* n 12)) (+ offset (ref 0)))
            (cvector-set! indices (+ 1 (* k 3) (* n 12)) (+ offset (ref 1)))
            (cvector-set! indices (+ 2 (* k 3) (* n 12)) (+ offset (ref 2)))))))
    (river-buffer
     tile-count
     vertices
     indices)))
