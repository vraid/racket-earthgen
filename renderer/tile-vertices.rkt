#lang typed/racket

(require vraid/typed-gl
         vraid/color
         "../planet/grid-base.rkt")

(provide (all-defined-out))

(: init-buffer (-> grid (-> FlVector (-> FlVector FlVector)) gl-buffer Void))
(define (init-buffer grid transform buffer)
  (let ([set-coord (gl-buffer-set-vertex-coord! buffer)]
        [set-index (gl-buffer-set-index! buffer)])
    (for ([n (tile-count grid)])
      (let ([f (transform ((grid-tile-coordinates grid) n))])
        (let ([k (* n 7)])
          (set-coord k (f ((grid-tile-coordinates grid) n))))
        (for ([i 6])
          (let ([k (+ 1 i (* n 7))])
            (set-coord k (f ((grid-corner-coordinates grid) ((grid-tile-corner grid) n i)))))
          (let ([k (+ (* i 3) (* n 18))])
            (set-index k (* n 7))
            (set-index (+ 1 k) (+ 1 (modulo i 6) (* n 7)))
            (set-index (+ 2 k) (+ 1 (modulo (+ i 1) 6) (* n 7)))))))))

(: update-vertices! (-> Integer gl-buffer (-> Integer flcolor) Void))
(define (update-vertices! count buffer color-function)
  (let* ([set-color (gl-buffer-set-vertex-color! buffer)])
    (for ([n count])
      (let ([color (color-function n)])
        (set-color (* n 7) color)
        (for ([i 6])
          (set-color (+ 1 i (* n 7)) color))))))
