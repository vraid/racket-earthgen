#lang typed/racket

(require vraid/typed-gl
         vraid/color
         "planet-data.rkt")

(provide (all-defined-out))

(define init-buffer
  (λ ([data : planet-data]
      [transform : (FlVector -> (FlVector -> FlVector))]
      [buffer : gl-buffer])
    (let* ([tile-count (planet-data-tile-count data)]
           [tile-coordinates (planet-data-tile-coordinates data)]
           [corner-coordinates (planet-data-corner-coordinates data)]
           [tile-corner (planet-data-tile-corner data)]
           [set-coord (gl-buffer-set-vertex-coord! buffer)]
           [set-index (gl-buffer-set-index! buffer)])
      (for ([n tile-count])
        (let ([f (transform (tile-coordinates n))])
          (let ([k (* n 7)])
            (set-coord k (f (tile-coordinates n))))
          (for ([i 6])
            (let ([k (+ 1 i (* n 7))])
              (set-coord k (f (corner-coordinates (tile-corner n i)))))
            (let ([k (+ (* i 3) (* n 18))])
              (set-index k (* n 7))
              (set-index (+ 1 k) (+ 1 (modulo i 6) (* n 7)))
              (set-index (+ 2 k) (+ 1 (modulo (+ i 1) 6) (* n 7))))))))))

(: update-vertices! (-> Integer gl-buffer (-> Integer flcolor) Void))
(define (update-vertices! count buffer color-function)
  (let* ([set-color (gl-buffer-set-vertex-color! buffer)])
    (for ([n count])
      (let ([color (color-function n)])
        (set-color (* n 7) color)
        (for ([i 6])
          (set-color (+ 1 i (* n 7)) color))))))
